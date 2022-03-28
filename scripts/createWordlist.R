# Create a word list from spelling corrections, SUBTLEX and VARCON project
# Custom corrections are included as well as a custom list with proper nouns and
# acronyms.
#
# If spelling corrections are inconsistent with the source files, then 
# words in the source files are excluded.
#
# Author: Simon De Deyne
# Last modified: 17/10/2021

source('settings.R')

# Word list with SUBTLEX-ESP (min freq = 2) and CUSTOM DICTIONARY EXTENSIONS,
# some generic Acronyms as well (PC, CD, etc)
file.lexicon = '../data/dictionaries/rioplatenseWordlist.txt'

# Proper nouns, Acronyms
file.proper = '../data/dictionaries/rioplatenseProperNames.txt'

# Multiword expressions
file.multi = '../data/dictionaries/multiWordExpressions.txt'

# Spelling corrections
file.spelling   = paste0('../data/dictionaries/responseCorrections.csv')

# Output file that combines the various list of valid lexical entries
file.output = paste0('../data/dictionaries/wordlist.',release,'.txt')

X.dict = read.csv(file.lexicon, stringsAsFactors = FALSE,
                             fileEncoding = 'UTF-8') %>% as_tibble()

# Proper names should include capitalized cues as a minimum
X.proper = read.csv(file.proper, stringsAsFactors = FALSE,
                      fileEncoding = 'UTF-8') %>% as_tibble()

# Multi word list includes several multi word cues as well
X.multi = read.csv(file.multi, stringsAsFactors = FALSE,
                      fileEncoding = 'UTF-8') %>% as_tibble()

file.data = paste0('../data/SWOW/raw/SWOW-RP.complete.',release,'.csv')
X.swow = read.csv(file.data, stringsAsFactors = FALSE) %>% as_tibble()
X.cues = X.swow %>% group_by(cue) %>% tally()
X.cues = X.cues %>% filter(tolower(cue)!=cue)

# Added to multiWordExpressions.txt
# X.cue.mw = X.swow %>% group_by(cue) %>% tally() %>% 
#             mutate(nSplits = stri_count(cue, regex = "[\\s,-]+[^\\s,-]")) %>%
#             filter(nSplits > 0)

X.proper = full_join(X.proper,X.cues %>% select(Word = cue),by = 'Word')

X.spelling  = read.csv(file.spelling, stringsAsFactors =FALSE, 
                       fileEncoding = 'UTF-8') %>% as_tibble()

# Exclude response corrections
X.dict = anti_join(X.dict,X.spelling %>% select(Word = response),by='Word')
X.proper = anti_join(X.proper,X.spelling %>% select(Word = response),by='Word')
X.multi =  anti_join(X.multi,X.spelling %>% select(Word = response),by='Word')

X           = bind_rows(X.dict,X.proper,
                        X.spelling %>% select(Word = correction)) %>% 
                        distinct() %>% arrange(Word) %>% filter(Word!='')

write.csv(X,file.output,fileEncoding = 'UTF-8',row.names = FALSE)

