# Compile a list of potential spelling mistakes
# https://github.com/ropensci/hunspell

require(tidyverse)
library(hunspell)
require(stringi)
require(stringr)

source('settings.R')

# Read complete data
file.wordlist = paste0('../data/dictionaries/wordlist.',release,'.txt')
file.data = paste0('../data/SWOW/raw/SWOW-RP.complete.',release,'.csv')
file.mistakes = paste0('../data/SWOW/output/spellingMistakes/spellingMistakes.',release,'.csv')
X = read.csv(file.data, stringsAsFactors = FALSE) %>% as_tibble()

# Convert to long
X = X %>% pivot_longer(names_to = 'RPOS',cols = c('R1','R2','R3'),
                   values_to = 'response') %>% select(cue,RPOS,response)

# Tally
X = X %>% group_by(response) %>% tally()
X = X %>% filter(!response %in% unknown.Token,!response %in% missing.Token)

# Basic preprocessing
##  Replace whitespace
X = X %>% mutate(response_clean = 
                   stri_trim(stri_replace_all(response,NA,regex="^\\s*$")))

## Replace everything between brackets
X = X %>% mutate(response_clean = 
                   stri_trim(gsub("\\s*\\([^\\)]+\\)","",response_clean)))
## Replace punctuation
X = X %>% mutate(response_clean = 
                   stri_replace_all(response_clean,"",regex="[*«»_¿?¡!ç}.:\"]"))

## Replace whitespace again
X = X %>% mutate(response_clean = 
                   stri_trim(stri_replace_all(response_clean,NA,regex="^\\s*$")))
X = X %>% filter(complete.cases(.))

message('Responses affected by preprocessing: ',
        X  %>% filter(response!=response_clean) %>% nrow())

# Code words in the word list
X.lexicon  = read.csv(file.wordlist, 
                      header = TRUE,stringsAsFactors = FALSE, 
                      strip.white = T,fileEncoding = 'UTF-8') %>%
                      as_tibble()

# Code known misspellings
file.corrections = '../data/dictionaries/responseCorrections.csv'
X.corrections =  read.csv(file.corrections, 
                          header = TRUE,stringsAsFactors = FALSE, 
                          strip.white = T,fileEncoding = 'UTF-8') %>%
                          as_tibble()

# Confirm that spelling corrections are consistent with X.lexicon
nLexicon = X.lexicon %>% nrow()
X.lexicon = X.lexicon %>% filter(!Word %in% X.corrections$response)
if(nLexicon != X.lexicon %>% nrow()){
  warning('Lexicon is not consistent with response corrections. Please remove misspelled words from lexicon')
}

X = left_join(X,X.corrections,by='response')
X = X %>% mutate(response_clean = 
                   ifelse(is.na(substitution),
                          response_clean,substitution)) 
X = X %>% select(-substitution)

#X = X %>% mutate(response_clean = ifelse(response_clean))
X = X %>% mutate(inLexicon = ifelse(response_clean %in% X.lexicon$Word,1,0))


# Only keep responses not in the lexicon
X = X %>% filter(inLexicon == 0)
X = X %>% select(response = response_clean,n)

# Indicate whether multiresponse or not: Count spaces in responses, 
# but ignore missing responses (Unknown word, No more responses")
X = X %>% mutate(nWords = str_count(response,"\\S+") + str_count(response,",|;"))

rioplatenseDic = dictionary('../data/dictionaries/Spanish_Rioplatense.dic')
X = X %>% mutate(correct = hunspell_check(response,dict = rioplatenseDic))
X.mistakes = X %>% filter(!correct,nWords < 2) %>% 
                arrange(-n) %>% select(response,n)

write.csv(X.mistakes,file =file.mistakes,row.names = F)
