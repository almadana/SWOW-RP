# Compile list of words in lexicon. First cues, then responses (not included as cues) (sorted by frequency)
# excluding multi-word expressions
# 
# Export files that can be used in ESPAL to extract POS info

library(tidyverse)
library(stringi)

#source('../settings.R')
#source('../../functions/importDataFunctions.R')


#output files
outputPrefix = "../../data/ESPAL/lexicon_"

# load Cue stats
file.cueStats = paste0('../../data/SWOW/output/centrality/cueStats.SWOW-RP.R123.',release,'.csv')
cues = read.csv(file.cueStats) %>% pull(cue)
file.ResponseStats = paste0('../../data/SWOW/output/centrality/responseStats.',release,'.csv')
responses = read.csv(file.ResponseStats) %>% arrange(desc(Freq.R123)) %>% pull(response)

X.lexicon = tibble(word=unique(append(cues,responses)))

X.lexicon = X.lexicon %>% mutate(nWords = str_count(word,"\\S+")) %>% filter(nWords==1) %>% 
  pull(word)


nFiles = length(X.lexicon) %/% 10000


for (i in (1:(nFiles+1))) {
  maxRow = ifelse(i==(nFiles+1),length(X.lexicon),i*10000)
  write.table(X.lexicon[(1+(i-1)*10000):(maxRow)],file=paste0(outputPrefix,i,".txt"),row.names = F,quote = F,
              col.names = F)
}
