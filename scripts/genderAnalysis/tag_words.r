# compare masc. vs. fem cues 
# TODO: solve issue with multiword response.
# e.g. View(X.R123 %>% filter(response=='amigos felicidad trabajajo'))
library(tidyverse)
library(udpipe)


source('./settings.R')
source('../functions/importDataFunctions.R')

tagged.lexicon.file = paste0('../data/SWOW/processed/SWOW-RP-tagged-lexicon-',release,'.csv')
lexicon.pairs.file = paste0('../data/SWOW/processed/SWOW-RP-lexicon-pairs-',release,'.csv')
lexicon.number.pairs.file = paste0('../data/SWOW/processed/SWOW-RP-lexicon-number-pairs-',release,'.csv')


getGender <- function(words) {
  result = udpipe_annotate(ancora_model,words,parser = "none")  %>% as_tibble() %>%
    select(word = sentence,token,lemma,POS = upos,feats) %>% 
    mutate(gender = sub(".*Gender=([FM][^|]*).*",replacement = "\\1",
                        feats))
  result = result %>% filter(gender %in% c('Masc','Fem'))
  return(result)
}

getNumber <- function(words) {
  result = udpipe_annotate(ancora_model,words,parser = "none")  %>% as_tibble() %>%
    select(word = sentence,token,lemma,POS = upos,feats) %>% 
    mutate(number = sub(".*Number=([PS][^|]*).*",replacement = "\\1",
                        feats))
  result = result %>% filter(number %in% c('Plur','Sing'))
  return(result)
}

# gender with udpipe
ancora_model_file = "../data/NLP/spanish-ancora-ud-2.4-190531.udpipe"
ancora_model = udpipe_load_model(ancora_model_file)

file.swow = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
X.R123 = importDataSWOW(file.swow,'R123')

#POS with EsPal
#source('./scripts/genderAnalysis/export_lexicon_to_ESPAL.r') #only run if dataset has changed // need to manually use EsPal with output files from this script to be effectiva (and replace data files obtained from the EsPal page)
source('./genderAnalysis/load_ESPAL_data.r') #TO-DO

X.freq = X.R123 %>% group_by(cue,response) %>% tally() 
X.lexicon = data.frame(word = unique(c(X.freq$cue,X.freq$response))) %>% 
  as_tibble() %>% mutate(word = as.character(word))


# Speedup
#X.freq = X.freq %>% filter(n>1)
#X.gender = X.lexicon[1:100,] %>% rowwise() %>% mutate(gender = getGender(word))
gender = getGender(X.lexicon %>% pull(word))
number = getNumber(X.lexicon %>% pull(word))

X.lex = inner_join(gender,number %>% select(word,number),by = 'word') %>% 
  select(word,POS,lemma,gender,number)

X.lex.pos = left_join(X.lex %>% select(word,gender,number),esPal.data,by="word")

#remove duplicates
X.lex.pos = X.lex.pos %>%  distinct(word,.keep_all = T)

# fem/masc pairs tend to have the same number of letters ... irregular exceptions should be handled separately
X.lex.pos$nChars = stringr::str_length(X.lex.pos$word)


# get gendered Pairs
lexicon.pairs = X.lex.pos %>% filter(!is.na(gender),!is.na(POS)) %>% group_by(number,lemma,POS,nChars) %>% 
  mutate(hasMasc=ifelse(sum(gender %in% "Masc")>0,T,F),
         hasFem=ifelse(sum(gender %in% "Fem")>0,T,F),
         isPair = hasMasc & hasFem) %>% 
  filter(isPair,POS %in% c("ADJECTIVE","NOUN","DETERMINER"),n()==2) %>% 
  pivot_wider(names_from = gender,values_from = word) %>% 
  ungroup() %>% 
  select(number,POS,lemma,Masc,Fem)

lexicon.pairs$nPair = 1:nrow(lexicon.pairs)


# get number Pairs (sing/plural form of same word)
lexicon.number.pairs = X.lex.pos %>% filter(!is.na(number),!is.na(POS),!is.na(gender)) %>% group_by(lemma,POS,gender) %>% 
  select(-nChars) %>% 
  mutate(hasSing=ifelse(sum(number %in% "Sing")>0,T,F),
         hasPlur=ifelse(sum(number %in% "Plur")>0,T,F),
         isPair = hasSing & hasPlur) %>% 
  filter(isPair,POS %in% c("ADJECTIVE","NOUN"),n()==2) %>% 
  pivot_wider(names_from = number,values_from = word) %>% 
  ungroup() %>% 
  select(gender,POS,lemma,Sing,Plur)

lexicon.number.pairs$nPair = 1:nrow(lexicon.number.pairs)




write.csv(X.lex.pos,file=tagged.lexicon.file)
write.csv(lexicon.pairs,file=lexicon.pairs.file)
write.csv(lexicon.number.pairs,file=lexicon.number.pairs.file)

# 
# # 
# # # Plot the results
# X.R123 = X.R123 %>% rename(gender.pp = gender)
# X.R123 = left_join(X.R123,X.lex.pos,by = c('cue'='word'),suffix =c('.cue','.cue'))
# X.R123 = left_join(X.R123,X.lex.pos,by = c('response'='word'),suffix =c('','.response'))
# # 
#  X.g = X.R123 %>% filter(!is.na(gender),!is.na(gender.response),!is.na(number),
#                        !is.na(number.response),!is.na(POS),!is.na(POS.response))
# # 
# X.g = X.g %>% filter(POS %in% c('ADJECTIVE','NOUN'),
#                       POS.response %in% c('ADJECTIVE','NOUN'))
# # 
# X.pos = X.R123 %>% filter(POS %in% c('ADJECTIVE','NOUN'),
#                       POS.response %in% c('ADJECTIVE','NOUN'))
# # 
# 
# library('vcd')
# library('vcdExtra')
#  mosaic(~ gender + gender.response, data = X.g,shade = TRUE,legend = TRUE)
#  mosaic(~ number + number.response, data = X.g,shade = TRUE,legend = TRUE)
#  mosaic(~ POS + POS.response, data = X.pos,shade = TRUE,legend = TRUE)
# #


 # # Issue with verbs: few of them seem to be recognized...
# #categorize each cue by POS tag and grammatical gender
# 
# # Identify cues with alternate genders
# cues  = X.R123 %>% filter(!is.na(gender),POS %in% c('NOUN','ADJECTIVE')) %>%  #this excludes plurals! and adjectives!
#   group_by(cue,lemma,gender,number,POS,nChars) %>% tally() %>% group_by(lemma,number,POS,nChars) %>% nest()
# 
# # Stereotypical example: 
# 
# X.multi = gender %>% filter(POS =='NOUN',word %in% cues) %>% group_by(lemma) %>% tally() %>% filter(n>1)
# 
# X.R1 %>% filter(cue %in% X.multi$lemma) %>% group_by(cue,gender) %>% tally() %>% arrange(cue)
# 
# actor = X.R123 %>% filter(cue %in% c('actor','actriz')) %>% 
#   group_by(cue,response,POS.response) %>% tally() %>% filter(n>1) %>% arrange(cue,-n)
# 
# X.R123 = X.R123 %>% unique.data.frame()
# friend = X.R123 %>% filter(cue %in% c('amigo','amiga')) %>% 
#   group_by(cue,response,POS.response) %>% tally() %>% filter(n>1) %>% arrange(cue,-n)
# 
# dog = X.R123 %>% filter(cue %in% c('perro','perra')) %>% 
#   group_by(cue,response,POS.response) %>% tally() %>% filter(n>1) %>% arrange(cue,-n)

