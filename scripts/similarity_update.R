## re-calculate similarity ratings for moldovan, simlex, and other experiments

source('settings.R')

require(tidyverse)
file.swow = c('../data/similarity/swow.sim.moldovan.es.csv',
              '../data/similarity/swow.sim.concrete.es.csv',
              '../data/similarity/swow.sim.abstract.es.csv',
              '../data/similarity/swow.sim.simlex.es.csv')

output.fixed = './data/similarity/swow.sim.'
output.names = c("moldovan","concrete","abstract","simlex")

#cosine matrices
matrix.files = c('../output/S  S _SWOWES-UY.RW.A75.R123.csv')


# read full matrix
full.matrix = read.csv(matrix.files[1],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = T,
                       row.names = 1)#
#full.matrix = read.csv("./output/s_s123.csv",strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = T,
#row.names = 1)
#catch colnames
indexes = colnames(full.matrix)
#indexes = colnames(S_no_balance)

#balance, with S_balance in memory!
for (i in 1:length(file.swow)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  words = X.swow %>% select(wordA,wordB,rating)
  swow_sims=words %>% rowwise() %>% mutate(in.A=wordA %in% indexes,in.B=wordB %in% indexes,
                                           swow_rw75=ifelse(in.A & in.B, S_no_balance[wordA,wordB],NA))
  out.file = paste0(output.fixed,output.names[i],".es.rw75_no_balance.csv")
  write.csv(swow_sims,file=out.file)
}




for (i in 1:length(file.swow)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  words = X.swow %>% select(wordA,wordB,rating)
  swow_sims=words %>% rowwise() %>% mutate(in.A=wordA %in% indexes,in.B=wordB %in% indexes,
                 swow_rw75=ifelse(in.A & in.B, full.matrix[wordA,wordB],NA))
  out.file = paste0(output.fixed,output.names[i],".es.rw75.csv")
  write.csv(swow_sims,file=out.file)
}

# for this part, you should have the Strength cosine matrix (S) in memory, as in S = cosineMatrix(G$R123$Strength). see graphRandomWalk.R for details
indexes = colnames(S)

for (i in 1:length(file.swow)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  words = X.swow %>% select(wordA,wordB,rating)
  swow_sims=words %>% rowwise() %>% mutate(in.A=wordA %in% indexes,in.B=wordB %in% indexes,
                                           swow_strength=ifelse(in.A & in.B, S[wordA,wordB],NA))
  out.file = paste0("./data/processed/similarity/swow.sim.",output.names[i],".es.Strength.csv")
  write.csv(swow_sims,file=out.file)
}


# for this part, you should have the PPMI cosine matrix (S) in memory, as in S = cosineMatrix(G$R123$PPMI). see graphRandomWalk.R for details
indexes = colnames(S)

for (i in 1:length(output.names)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  words = X.swow %>% select(wordA,wordB,rating)
  swow_sims=words %>% rowwise() %>% mutate(in.A=wordA %in% indexes,in.B=wordB %in% indexes,
                                           swow_ppmi=ifelse(in.A & in.B, S[wordA,wordB],NA))
  out.file = paste0(output.fixed[i],output.names[i],".es.PPMI.csv")
  write.csv(swow_sims,file=out.file)
}


for (i in 1:length(output.names)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  measures = c("Strength","PPMI","rw75")
  updated_filenames = paste0('./data/processed/similarity/swow.sim.',output.names[i],".es.",measures,".csv")
  swow=lapply(updated_filenames,read.csv) %>%
    bind_cols() %>% select(wordA...2,wordB...3,swow_strength,swow_ppmi,swow_rw75)
  colnames(swow)[1:3]=c("wordA","wordB","swow_raw")
  swow$rating = X.swow$rating
  write.csv(swow,file=paste0(output.fixed,output.names[i],'.es_2021.csv'))
}

## embeddings: XA. need S matrix and indexes array
for (i in 1:length(output.names)) {
  X.swow = read.csv(file.swow[i],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = TRUE)
  words = X.swow %>% select(wordA,wordB,rating)
  swow_sims=words %>% rowwise() %>% mutate(in.A=wordA %in% indexes,in.B=wordB %in% indexes,
                                           swow_xa=ifelse(in.A & in.B, S[wordA,wordB],NA))
  out.file = paste0(output.fixed,output.names[i],".es.X.csv")
  write.csv(swow_sims,file=out.file)
}
