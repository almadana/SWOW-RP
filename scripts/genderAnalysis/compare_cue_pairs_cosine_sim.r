#calculate cosine similarities between cue pairs, based on the full graph
require(tidyverse)

#cue_pairs
cue_pairs_wide = read.csv(file="data/cue_pairs_wide.csv")

#outfile
out.file = paste0(file="data/cue_pairs_wide_sem.csv")

#cosine matrices
matrix.files = c('../SWOWES-UY-2018/output/S_SWOWES-UY.RW.A75.R123.csv')


# read full matrix
full.matrix = read.csv(matrix.files[1],strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = T,
                       row.names = 1)#
#full.matrix = read.csv("./output/s_s123.csv",strip.white = TRUE,stringsAsFactors = FALSE,fileEncoding='UTF-8',encoding = 'UTF-8',header = T,
#row.names = 1)
#catch colnames
indexes = colnames(full.matrix)

cue_pairs_wide = cue_pairs_wide %>% rowwise() %>% mutate(swow_rw75=full.matrix[Fem,Masc])

write.csv(cue_pairs_wide,file=out.file)
