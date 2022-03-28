# Compute cue-level statistics either on the cue-by-response or
# the cue-by-cue matrix
#
# This script computes:
# 1. % known cues
# 2. % Missing responses (for R2 or R3)
# 2. Total number of cues
# 3. Coverage of cues (i.e. how many of the responses are also cues) based on R1 or R123
# 4. Entropy of the responses given the cues: E(R|C)
#
# Author: Simon De Deyne simon2d@gmail.com
# Date: 2 October 2021

require(tidyverse)
require(Matrix)
require(pbapply)
require(entropy)

source('settings.R')
source('../functions/importDataFunctions.R')
source('../functions/networkFunctions.R')

file.swow = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
file.R1  = paste0('../data/SWOW/output/centrality/cueStats.SWOW-RP.R1.',release,'.csv')
file.R123 = paste0('../data/SWOW/output/centrality/cueStats.SWOW-RP.R123.',release,'.csv')
file.comp = paste0('../data/SWOW/output/centrality/components.removedV.SWOW-RP.',release,'.csv')

# Load the removed vertices from the strongly connected components
X.comp = read.csv(file.comp, stringsAsFactors = F) %>% as_tibble()

## SWOW-R1 statistics
X.R1              = importDataSWOW(file.swow,'R1')

# Remove cues not in the component
v  = X.comp %>% filter(RPOS=='R1') %>% 
        mutate(vertices = as.character(vertices)) %>% 
        pull(vertices)

X.R1  = X.R1 %>% filter(!cue %in% v)

# Calculate coverage
Cues.known        = X.R1 %>% filter(complete.cases(response)) %>% 
                      group_by(cue) %>% summarise(cue.Tokens = n())
Cues.N            = X.R1 %>% group_by(cue) %>% summarise(N = n())
Cues.covered      = X.R1 %>% filter(response %in% cue) %>% group_by(cue) %>% 
  summarise(cue.Covered = n())
coverage.R1       = left_join(Cues.known,Cues.covered, by = 'cue') %>% 
              mutate(coverage = cue.Covered / cue.Tokens  * 100) %>% 
              select(cue,coverage)

# Calculate entropy H
message('Calculating entropy R1')
H.R1 = getEntropy(X.R1,'cues')

# Calculate unknown
xR1        = X.R1 %>% group_by(cue) %>% 
                        summarise(unknown = sum(is.na(response)))
cueStats.R1 = as.data.frame(left_join(coverage.R1,H.R1,by = 'cue') %>% 
                                    left_join(.,xR1,by = 'cue') %>% 
                                    left_join(.,Cues.N,by = 'cue'))


## SWOW-R123 statistics
X.R123    = importDataSWOW(file.swow,'R123')

# Remove cues not in the component
v  = X.comp %>% filter(RPOS=='R123') %>% 
  mutate(vertices = as.character(vertices)) %>% 
  pull(vertices)

X.R123  = X.R123 %>% filter(!cue %in% v)

# Calculate coverage
Cues.known        = X.R123 %>% filter(complete.cases(response)) %>% 
          group_by(cue) %>% summarise(cue.Tokens = n())
Cues.N            = X.R123 %>% group_by(cue) %>% summarise(N = n())
Cues.covered      = X.R123 %>% filter(response %in% cue) %>% group_by(cue) %>% 
  summarise(cue.Covered = n())
coverage.R123     = left_join(Cues.known,Cues.covered, by = 'cue') %>% 
  mutate(coverage = cue.Covered / cue.Tokens  * 100) %>% select(cue,coverage)

# Calculate entropy H
message('Calculating entropy R123')
H.R123           = getEntropy(X.R123,'cues')

# Calculate unknown
xR1   = X.R123 %>% group_by(cue) %>%
  summarise(unknown = sum(is.na(response[RPOS=='R1'])))

# Calculate missing (R2,R3)
xR2               = X.R123 %>% group_by(cue) %>% 
  summarise(xR2 = sum(is.na(response[RPOS=='R2'])))
xR2$xR2           = xR2$xR2 - xR1$unknown
xR3               = X.R123 %>% group_by(cue) %>% 
  summarise(xR3 = sum(is.na(response[RPOS=='R3'])))
xR3$xR3           = xR3$xR3 - xR2$xR2 - xR1$unknown

cueStats.R123       = as.data.frame(left_join(coverage.R123,H.R123,by = 'cue') %>% 
                                      left_join(.,xR1,by = 'cue') %>%
                        left_join(.,xR2, by = 'cue') %>% 
                        left_join(.,xR3, by = 'cue') %>% 
                        left_join(.,Cues.N, by = 'cue'))


# Write the results to file
write.csv(cueStats.R1,file = file.R1)
write.csv(cueStats.R123,file = file.R123)
