# Preprocessing pipeline for SWOWES-UY project
#
# The script writes a fixed number of participants per cue (here set to 60)
# to an output file used for further processing
# To determine which data to retain, a number of checks are performed to see if 
# participants are fluent speakers (knowing most cues, responding with words 
# part of the language' lexicon), etc. 
#
# TODO: multiword expressions not in the list of exceptions are currently not
# spell-checked
#
# Author: Simon De Deyne, simon2d@gmail.com
# Adaptation for RP data: Álvaro Cabana, almadana@gmail.com
# Last changed: 17/10/2021


require(stringi)
require(stringr)
require(hunspell)
require(tidyverse)

source('./settings.R')
source('../functions/preprocessing.R')

# stats
stats = c()

# Read data
file.data = paste0('../data/SWOW/raw/SWOW-RP.complete.',release,'.csv')
X = read.csv(file.data, stringsAsFactors = FALSE,encoding = 'UTF-8') %>% 
      as_tibble()

# Fix repeated responses
# e.g. cue: 'sea', R1 = 'swim',R2 = 'swim'
# replaces R2 as NA
result = recodeRepeats(X,missing.Token,unknown.Token)
stats = c(stats,'repeatedResponses' = result$nDoubles)

# Replace inconsistent missing responses and code as NA
result = normalizeMissingResponses(result$X,missing.Token)
stats = c(stats,'inconsistentMissing' = result$nInconsistent)

# Replace R1 multiword responses and discard R2 and R3
result = parseConcatenatedResponses(result$X,missing.Token,unknown.Token)
stats = c(stats,'multiWordMissing' = result$nMultiResponsesChanged)

# Manually check any remaining responses that might indicate concatenated R1/2/3
X.multi = result$X%>% filter(grepl('.*\\W.*\\W.*',R1)) %>% 
              select(participantID,cue,R1)
write.csv(X.multi,paste0('../data/SWOW/output/spellingMistakes/multiResponses.',
                         release,'.csv'),row.names = F)

# Convert to long format to facilitate further processing
result$X = result$X %>% ungroup %>% 
            pivot_longer(names_to = 'RPOS',
                         cols = c('R1','R2','R3'),
                         values_to = 'response')

# Clean responses by removing white-space and punctuation
result = cleanResponse(result$X)
stats = c(stats,'nResponseCleaning' = result$nReplacements)

# Convert responses for participants who mostly use uppercase
result = checkCapitalization(result$X)
stats = c(stats,'nCapitalizationReplaced' = result$nResponsesConverted)

# Correct misspelled responses and write a list of corrections to a 
# separate file
file.lexicon = '../data/dictionaries/wordlist.txt'
file.cueResponseCorrections = '../data/dictionaries/cueResponseCorrections.csv'
file.responseCorrections = '../data/dictionaries/responseCorrections.csv'
file.correctionOutput = paste0('../data/SWOW/output/spellingMistakes/',
                               'cueResponseCorrections.',release,'.csv')

# Correct spelling responses
tmp  = X
X = result$X
result  = spellCheckResponse(result$X,file.lexicon,file.dictionary,
                              file.cueResponseCorrections,
                              file.responseCorrections)
stats = c(stats,'nSpellingCorrections' = result$nCorrected)

# Write a list of corrections to be used for verification/server upload
write.csv(result$corrections,file.correctionOutput,row.names = F)  
  

# Normalize cues (move to separate script)
file.cueCorrections = '../data/dictionaries/cueCorrections.txt'
result = normalizeCues(result$X,file.cueCorrections)
stats = c(stats,'nCuesNormalized' = result$nNormalizedCues)

# Write result
file.correctedSWOW = paste0('../data/SWOW/processed/SWOW-RP.spellchecked.',
                            release,'.csv')
# Exclude columns
result$X = result$X %>% select(-nSplits,-isMissing,-isUnknown,-nWords,
                    -serialResponder,-splitResponse,-correctSpelling,
                    -isAllCaps,-isFirstCaps,-prop.isAllCaps,-prop.isFirstCaps)

# Write to wide format to save space
result$X = result$X %>% mutate(RPOS = as.factor(RPOS))
result$X = result$X %>% pivot_wider(names_from = 'RPOS',values_from = 'response')
write.csv(result$X,file.correctedSWOW,row.names = F)

# Write the stats
file.stats = paste0('../data/SWOW/output/preprocessData.stats.',release,'.csv')
stats = data.frame(label = names(stats),value = as_tibble_col(stats))
write.csv(stats,file.stats,row.names = F)