# creates a graph with balanced cue pairs (as responses)


library(tidyverse)
library(tidygraph)
library(Matrix)

source('scripts/settings.R')
source('functions/importDataFunctions.R')

source('functions/networkFunctions.R')
source('functions/similarityFunctions.R')



# Load the data
file.swow = paste0('data/SWOW/output/strength.SWOW-RP.R123.',release,'.csv')
X.R123 = read.delim(file.swow,stringsAsFactors = F,sep = '\t')
G.R123 = importGraph(X.R123,"R123")
#G.R123 = extractComponent(G.R123,'strong')


# load cue pairs

lexicon.pairs.file = paste0('data/SWOW/processed/SWOW-RP-lexicon-pairs-',release,'.csv')
lexicon.pairs = read.csv(lexicon.pairs.file)
lexicon.long = lexicon.pairs %>% pivot_longer(c(Masc,Fem),values_to="word",names_to="gender")

cues = G.R123 %>% activate(nodes) %>% pull(name)
#filter words not in cue set
lexicon.long = lexicon.long %>% filter( (word %in% cues))
lexicon.cue.pairs = lexicon.long %>% select(-X) %>% 
  pivot_wider(names_from = gender,values_from = word) %>% 
  ungroup() %>% 
  filter(!is.na(Fem), !is.na(Masc),POS %in% c("DETERMINER","ADJECTIVE"))



lexicon.long = lexicon.cue.pairs %>% pivot_longer(c(Masc,Fem),names_to = "gender",values_to="word")


cues = tibble(word=cues,n=1:length(cues))

lexicon.long = lexicon.long %>% inner_join(cues)
# get index of cue pairs
cue.pairs.index = lexicon.long%>% pivot_wider(names_from = gender,values_from = c(word,n)) %>% 
  select(n_Fem,n_Masc)



#Adjacency matrix
P = igraph::as_adjacency_matrix(G.R123,attr='weight',names = TRUE)
# Identity matrix
I   = Matrix::Diagonal(dim(P)[1])


# cue pair normalization
for (i in 1:nrow(cue.pairs.index)) {
  ii = cue.pairs.index$n_Fem[i]
  jj = cue.pairs.index$n_Masc[i]
  I[ii,jj]<-.5
  I[jj,ii]<-.5
  I[ii,ii]<-.5
  I[jj,jj]<-.5
}

P=P%*%I
#P=normalize(P)
P=PPMI(P)
P=normalize(P)
class(P)
#P=Matrix(P,sparse = F)
#class(P)
#random graph
alpha = 0.75 
G.rw = katzWalk(P,alpha)
P=PPMI(G.rw)
P=normalize(P)

#words to keep
keep=cue.pairs.index %>% pivot_longer(c(n_Fem,n_Masc)) %>% pull(value)
S_no_balance=cosineMatrix(P)
#S=cosineMatrix(P[keep,])
S2=cosineMatrix(P[keep,])

save(S,file="data/SWOW/output/similarity/gendered_cues_S.RData")
save(S2,file="data/SWOW/output/similarity/gendered_cues_S_nonorm.RData")
#save(S_balance,file="data/SWOW/output/similarity/gendered_cues_S_full.RData")
save(S_no_balance,file="data/SWOW/output/similarity/gendered_cues_S_full_nobalance.RData")
# load S cosine matrix without gender normalization

S2[1,2]
S2[2]
S[1,2]

i=seq(1,length(keep),by = 2)
cos_pre = rep(NA,length(i))
cos_pos = cos_pre
for (j in 1:length(i)) {
  ix = i[j]
  cos_pos[j]=S[ix,ix+1]
  cos_pre[j]=S2[ix,ix+1]
}


S.pairs.distances = lexicon.long%>% pivot_wider(names_from = gender,values_from = c(word,n)) 
S.pairs.distances$cos_pre = cos_pre
S.pairs.distances$cos_pos = cos_pos
save(S.pairs.distances,file="data/SWOW/output/similarity/S_pairs_distancies.RData")
