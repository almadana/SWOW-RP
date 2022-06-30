# compare cue pairs based on their responses, and their (grpah) semantic similarities



library(tidyverse)

source('scripts/settings.R')
source('functions/importDataFunctions.R')


#load data
#tagged.lexicon.file = paste0('data/SWOW/processed/SWOW-RP-tagged-lexicon-',release,'.csv')
lexicon.pairs.file = paste0('data/SWOW/processed/SWOW-RP-lexicon-pairs-',release,'.csv')
lexicon.pairs = read.csv(lexicon.pairs.file)
lexicon.number.pairs.file = paste0('data/SWOW/processed/SWOW-RP-lexicon-number-pairs-',release,'.csv')
lexicon.number.pairs = read.csv(lexicon.number.pairs.file)


lexicon.pairs$normalized = lexicon.pairs$Fem #normalized response variant to "Fem" variant
lexicon.number.pairs$normalized = lexicon.number.pairs$Plur #normalized response variant to "Fem" variant


file.swow     = paste0('data/SWOW/processed/SWOW-RP.R70.',release,'.csv')

lexicon.long = lexicon.pairs %>% pivot_longer(c(Masc,Fem),values_to="word",names_to="gender")
lexicon.number.long = lexicon.number.pairs %>% pivot_longer(c(Sing,Plur),values_to="word",names_to="number")


## --- load strength data and join with pair data-----

#swow.strength.data.file = paste0('data/SWOW/output/strength.SWOW-RP.R123.',release,'.csv')
# swow data
response        = 'R123'
X.R123          = importDataSWOW(file.swow,response)

# #import strength data
# swow.strength = read.csv(swow.strength.data.file,sep = "\t",stringsAsFactors = F)

#obtain cue pairs
X.R123 = X.R123 %>% rename(gender.pp = gender)
X.pairs = X.R123 %>% left_join(lexicon.long,by = c("cue"="word"))

#obtain response pairs in cue pair data
X.pairs = X.pairs %>% rename(cue_lemma=lemma,cue_nPair = nPair,cue_gender=gender,cue_number = number,cue_POS=POS) %>% 
  select(-normalized) %>% 
  left_join(lexicon.long,by=c("response"="word")) %>% 
  rename(response_lemma=lemma,response_nPair = nPair,response_gender=gender,response_number = number,response_POS=POS) 


#only cue pairs, without normalizing responses
X.cue_pairs = X.pairs %>% filter(!is.na(cue_nPair),!is.na(response)) %>%
  group_by(cue, response,cue_nPair,cue_gender,cue_POS,cue_lemma) %>% 
  summarise(R123=n(),.groups = "drop") %>% 
  group_by(cue,cue_nPair,cue_gender,cue_POS,cue_lemma) %>%  mutate(N=sum(R123),R123.Strength = R123/N) %>% arrange(cue,-R123.Strength)
  
#only cue pairs, with normalizing responses
X.cue_pairs.normalized = X.pairs %>% filter(!is.na(cue_nPair),!is.na(response)) %>%
  mutate(response = ifelse(!is.na(normalized),normalized,response)) %>% 
  group_by(cue, response,cue_nPair,cue_gender,cue_POS,cue_lemma) %>% 
  summarise(R123=n(),.groups = "drop") %>% 
  group_by(cue,cue_nPair,cue_gender,cue_POS,cue_lemma) %>%  mutate(N=sum(R123),R123.Strength = R123/N) %>% arrange(cue,-R123.Strength)


#calculate cue pair distances, from raw responses (this includes responses that are not in the cue set, i.e. it has more pairs!)
cue_pairs.distances = 
  X.cue_pairs %>% 
  ungroup() %>% 
  select(cue_nPair,response,R123,cue_gender,cue_POS) %>% 
  group_by(cue_nPair,response,cue_POS) %>% 
  pivot_wider(names_from = cue_gender,values_from=R123,values_fill=0) %>%
  #filter(response %in% cues$cue) %>% #keep only responses that are in the cue list
  filter(Fem>2 | Masc>2) %>% ##keep only responses that appear at least two times in one cue
  group_by(cue_nPair,cue_POS) %>% 
  summarize(tanimoto=philentropy::distance(rbind(Masc,Fem),method="tanimoto"),
            cosine=philentropy::distance(rbind(Masc,Fem),method="cosine"),
            jensen_shannon=philentropy::distance(rbind(Masc,Fem),method="jensen-shannon")) %>% 
  filter(!is.nan(cosine))

cue_pairs.distances = cue_pairs.distances %>%  left_join(lexicon.pairs %>% select(nPair,Masc),by=c("cue_nPair"="nPair")) %>% 
  rename(cue=Masc)

cue_pairs.distances.normalized = 
  X.cue_pairs.normalized %>% 
  ungroup() %>% 
  select(cue_nPair,response,R123,cue_gender,cue_POS,cue_lemma) %>% 
  group_by(cue_nPair,response,cue_POS,cue_lemma) %>% 
  pivot_wider(names_from = cue_gender,values_from=R123,values_fill=0) %>%
  #filter(response %in% cues$cue) %>% #keep only responses that are in the cue list
  filter(Fem>2 | Masc>2) %>% ##keep only responses that appear at least two times in one cue
  group_by(cue_nPair,cue_POS,cue_lemma) %>% 
  summarize(tanimoto=philentropy::distance(rbind(Masc,Fem),method="tanimoto"),
            cosine=philentropy::distance(rbind(Masc,Fem),method="cosine"),
            jensen_shannon=philentropy::distance(rbind(Masc,Fem),method="jensen-shannon")) %>% 
  filter(!is.nan(cosine)) %>% ungroup()

cue_pairs.distances.normalized = cue_pairs.distances.normalized %>%  left_join(lexicon.pairs %>% select(nPair,Masc),by=c("cue_nPair"="nPair")) %>% 
  rename(cue=Masc)

cue_pairs.distances = cue_pairs.distances %>% 
  inner_join(cue_pairs.distances.normalized %>%  select(cue_nPair,cosine),by="cue_nPair") %>% 
  rename(cosine=cosine.x,cosine.normalized=cosine.y)



# NUMBER ----


X.number.pairs = X.R123 %>% left_join(lexicon.number.long,by = c("cue"="word"))


# NUMBER - obtain response pairs in cue pair data
X.number.pairs = X.number.pairs %>% rename(cue_lemma=lemma,cue_nPair = nPair,cue_gender=gender,cue_number = number,cue_POS=POS) %>% 
  select(-normalized) %>% 
  left_join(lexicon.number.long,by=c("response"="word")) %>% 
  rename(response_lemma=lemma,response_nPair = nPair,response_gender=gender,response_number = number,response_POS=POS) 

# NUMBER - only cue pairs, without normalizing responses
X.cue_num_pairs = X.number.pairs %>% filter(!is.na(cue_nPair),!is.na(response)) %>%
  group_by(cue, response,cue_nPair,cue_number,cue_POS,cue_lemma) %>% 
  summarise(R123=n(),.groups = "drop") %>% 
  group_by(cue,cue_nPair,cue_number,cue_POS,cue_lemma) %>%  mutate(N=sum(R123),R123.Strength = R123/N) %>% arrange(cue,-R123.Strength)

# NUMBER - only cue pairs, with normalizing responses
X.cue_num_pairs.normalized = X.number.pairs %>% filter(!is.na(cue_nPair),!is.na(response)) %>%
  mutate(response = ifelse(!is.na(normalized),normalized,response)) %>% 
  group_by(cue, response,cue_nPair,cue_number,cue_POS,cue_lemma) %>% 
  summarise(R123=n(),.groups = "drop") %>% 
  group_by(cue,cue_nPair,cue_number,cue_POS,cue_lemma) %>%  mutate(N=sum(R123),R123.Strength = R123/N) %>% arrange(cue,-R123.Strength)


#calculate cue pair distances, from raw responses (this includes responses that are not in the cue set, i.e. it has more pairs!)
cue_num_pairs.distances = 
  X.cue_num_pairs %>% 
  ungroup() %>% 
  select(cue_nPair,response,R123,cue_number,cue_POS) %>% 
  group_by(cue_nPair,response,cue_POS) %>% 
  pivot_wider(names_from = cue_number,values_from=R123,values_fill=0) %>%
  #filter(response %in% cues$cue) %>% #keep only responses that are in the cue list
  filter(Sing>2 | Plur>2) %>% ##keep only responses that appear at least two times in one cue
  group_by(cue_nPair,cue_POS) %>% 
  summarize(tanimoto=philentropy::distance(rbind(Sing,Plur),method="tanimoto"),
            cosine=philentropy::distance(rbind(Sing,Plur),method="cosine"),
            jensen_shannon=philentropy::distance(rbind(Sing,Plur),method="jensen-shannon")) %>% 
  filter(!is.nan(cosine))

cue_num_pairs.distances = cue_num_pairs.distances %>%  left_join(lexicon.number.pairs %>% select(nPair,Sing),by=c("cue_nPair"="nPair")) %>% 
  rename(cue=Sing)

cue_num_pairs.distances.normalized = 
  X.cue_num_pairs.normalized%>% 
  ungroup() %>% 
  select(cue_nPair,response,R123,cue_number,cue_POS) %>% 
  group_by(cue_nPair,response,cue_POS) %>% 
  pivot_wider(names_from = cue_number,values_from=R123,values_fill=0) %>%
  #filter(response %in% cues$cue) %>% #keep only responses that are in the cue list
  filter(Sing>2 | Plur>2) %>% ##keep only responses that appear at least two times in one cue
  group_by(cue_nPair,cue_POS) %>% 
  summarize(tanimoto=philentropy::distance(rbind(Sing,Plur),method="tanimoto"),
            cosine=philentropy::distance(rbind(Sing,Plur),method="cosine"),
            jensen_shannon=philentropy::distance(rbind(Sing,Plur),method="jensen-shannon")) %>% 
  filter(!is.nan(cosine))

cue_num_pairs.distances.normalized = cue_num_pairs.distances.normalized %>%  left_join(lexicon.number.pairs %>% select(nPair,Sing),by=c("cue_nPair"="nPair")) %>% 
  rename(cue=Sing)

cue_num_pairs.distances = cue_num_pairs.distances %>% 
  inner_join(cue_num_pairs.distances.normalized %>%  select(cue_nPair,cosine),by="cue_nPair") %>% 
  rename(cosine=cosine.x,cosine.normalized=cosine.y)

rm(cue_num_pairs.distances.normalized,cue_pairs.distances.normalized,X.cue_num_pairs,X.cue_num_pairs.normalized,X.cue_pairs,X.cue_pairs.normalized,X.pairs,X.R123)


source('scripts/genderAnalysis/cue_pair_merge_and_plots.r')


## -- compare cosines from graph------

# 
# load("data/SWOW/output/similarity/S_pairs_distancies.RData")
# S.pairs.distances
# cue_pairs.distances %>% inner_join(S.pairs.distances %>% select(nPair,starts_with("cos")),by=c("cue_nPair"="nPair"))
