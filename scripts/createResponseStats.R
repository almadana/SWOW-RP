# Description:
# Calculate response statistics for SWOW-RP
# Obtain number of types, tokens and Hapax legoma and write to an output file.
#
# Author: Simon De Deyne simon2d@gmail.com
# Date: 26 April 2022
library(tidyverse)
source('settings.R')
source('../functions/importDataFunctions.R')

file.swow     = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
file.output   = paste0('../data/SWOW/output/centrality/responseStats.',release,'.csv')
file.stats = paste0("../data/SWOW/output/centrality/responseStatsReport.",release,'.csv')

stats=c()

# Response frequencies for SWOW-RP R123
response            = 'R123'
X.swow              = importDataSWOW(file.swow,response) %>% as_tibble()
X.swow              = X.swow %>% filter(complete.cases(response))
Freq.SWOW.R123      = X.swow %>% group_by(response) %>%
                      summarise(Freq.R123 = n(),.groups  = 'drop') %>% arrange(desc(Freq.R123))

Types.SWOW.R123     = X.swow %>% group_by(response,cue) %>%
                      summarise(Types.Cue.R123 = n()) %>%
                      arrange(desc(Types.Cue.R123),.groups = 'drop')

Types.SWOW.R123     = Types.SWOW.R123 %>% group_by(response) %>% 
                      summarise(H.R123 = entropy::entropy(Types.Cue.R123),
                      Types.R123 = n(),.groups  = 'drop') %>% arrange(desc(Types.R123))

Freq.SWOW.R123      = left_join(Freq.SWOW.R123,Types.SWOW.R123, by = 'response')

stats=c(stats,"R123.types"=nrow(Types.SWOW.R123))
stats=c(stats,"R123.hapax"=nrow(Freq.SWOW.R123 %>% filter(Freq.R123 == 1)))



# Response frequencies for SWOW-RP R1
response            = 'R1'
X.swow              = importDataSWOW(file.swow,response) %>% as_tibble()
Freq.SWOW.R1      = X.swow %>% group_by(response) %>%
  summarise(Freq.R1 = n()) %>% arrange(desc(Freq.R1))

Types.SWOW.R1     = X.swow %>% group_by(response,cue) %>%
  summarise(Types.Cue.R1 = n(),.groups  = 'drop') %>%
  arrange(desc(Types.Cue.R1),.groups = 'drop')

Types.SWOW.R1     = Types.SWOW.R1 %>% group_by(response) %>% 
  summarise(H.R1 = entropy::entropy(Types.Cue.R1),
            Types.R1 = n(),.groups  = 'drop') %>% arrange(desc(Types.R1))

Freq.SWOW.R1  = left_join(Freq.SWOW.R1,Types.SWOW.R1, by = 'response')

stats=c(stats,"R1.types"=nrow(Types.SWOW.R1))
stats=c(stats,"R1.hapax"=nrow(Freq.SWOW.R1 %>% filter(Freq.R1 == 1)))


# Combine R1 and R123
Freq.SWOW = right_join(Freq.SWOW.R1,Freq.SWOW.R123,by = 'response',
                 suffix = c('.R1','.R123'))

# Write the response statistics for both R1 and R123
write.csv(Freq.SWOW,file.output,row.names = FALSE)

# Write the type and hapax totals
stats = data.frame(label = names(stats),value = as_tibble_col(stats))
write.csv(stats,file.stats)

