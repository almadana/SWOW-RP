# Description:
#
# Generate a cue x response table with strengths
#
# Note: the data included with this script cannot be distributed without prior consent
# Author: Simon De Deyne simon2d@gmail.com
# Last changed: 16 October 2021

library(tidyverse)
#require(igraph)
#require(Matrix)
source('settings.R')
source('../functions/importDataFunctions.R')

file.R1    = paste0('../data/SWOW/output/strength.SWOW-RP.R1.',release,'.csv')
file.R123  = paste0('../data/SWOW/output/strength.SWOW-RP.R123.',release,'.csv')

# Import the dataset for R1
file.swow     = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
response      = 'R1' # Options: R1, R2, R3 or R123
X.R1          = importDataSWOW(file.swow,response)

strength.R1   = X.R1 %>% filter(!is.na(response)) %>% group_by(cue,response) %>%
                    summarise(R1 = n(),.groups  = 'drop') %>% select(cue,response,R1)
total.R1      = strength.R1 %>% group_by(cue) %>% summarise(N = sum(R1))
strength.R1   = left_join(strength.R1,total.R1)
strength.R1   = strength.R1 %>% mutate(R1.Strength = R1 / N)
strength.R1   = strength.R1 %>% arrange(cue,-R1.Strength)

# Write cue - asso strength R1
write.table(strength.R1,file.R1,row.names = FALSE,sep = '\t',quote = FALSE)


# Import the dataset for R123
response        = 'R123'
X.R123          = importDataSWOW(file.swow,response)
strength.R123   = X.R123 %>% filter(!is.na(response)) %>%  group_by(cue,response) %>%
                      summarise(R123 = n(),.groups  = 'drop') %>% select(cue,response,R123)
total.R123      = strength.R123 %>% group_by(cue) %>% summarise(N = sum(R123))
strength.R123   = left_join(strength.R123,total.R123)
strength.R123   = strength.R123 %>% mutate(R123.Strength = R123 / N)
strength.R123   = strength.R123 %>% arrange(cue,-R123.Strength)

# Write cue - asso strength R1
write.table(strength.R123,file.R123,row.names = FALSE,sep = '\t',quote = FALSE)

