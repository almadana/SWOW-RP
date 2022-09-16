# Description:
# Calculate response statistics for SWOW-RP
# Obtain number of types, tokens and Hapax legoma and write to an output file.
#
# Author: Simon De Deyne simon2d@gmail.com
# Date: 15 September 2022
library(tidyverse)
source('settings.R')

file.swow     = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
file.output   = paste0('../data/SWOW/output/centrality/responseStats.',release,'.csv')
file.stats = paste0("../data/SWOW/output/centrality/responseStatsReport.",release,'.csv')

Freq = c(); Types = c();stats=c();

X = readr::read_csv(file.swow,show_col_types = FALSE,progress = FALSE)
X = X %>% pivot_longer(names_to = 'RPOS',cols = c(R1,R2,R3),values_to = 'response') %>% 
  filter(complete.cases(response))

# Response frequencies R123
Freq$R123      = X %>% group_by(response) %>%
  summarise(Freq.R123 = n(),.groups  = 'drop') %>%
  arrange(desc(Freq.R123))

Types$R123     = X %>% group_by(response,cue) %>%
  summarise(Types.Cue.R123 = n(),.groups = 'drop') %>%
  arrange(desc(Types.Cue.R123))

Types$R123     = Types$R123 %>% group_by(response) %>%
  summarise(H.R123 = entropy::entropy(Types.Cue.R123),
            Types.R123 = n(),.groups  = 'drop') %>% arrange(desc(Types.R123))

Freq$R123      = left_join(Freq$R123,Types$R123, by = 'response')  %>% filter(!is.na(response))

# Response frequencies R1
Freq$R1      = X  %>% filter(RPOS == 'R1') %>% group_by(response) %>%
  summarise(Freq.R1 = n(),.groups  = 'drop') %>%
  arrange(desc(Freq.R1))

Types$R1     = X %>% filter(RPOS == 'R1') %>% group_by(response,cue) %>%
  summarise(Types.Cue.R1 = n(),.groups = 'drop') %>%
  arrange(desc(Types.Cue.R1))

Types$R1     = Types$R1 %>% group_by(response) %>%
  summarise(H.R1 = entropy::entropy(Types.Cue.R1),
            Types.R1 = n(),.groups  = 'drop') %>% arrange(desc(Types.R1))

Freq$R1      = left_join(Freq$R1,Types$R1, by = 'response') %>% filter(!is.na(response))


# Combine R1 and R123
Freq.SWOW = right_join(Freq$R1,Freq$R123,by = 'response', suffix = c('.R1','.R123'))

# Write the response statistics for both R1 and R123
write.csv(Freq.SWOW,file.output,row.names = FALSE)

# Write the type and hapax totals
stats = c(stats,"R1.types"=nrow(Types$R1))
stats = c(stats,"R1.hapax"=nrow(Freq$R1 %>% filter(Freq.R1 == 1)))
stats = data.frame(label = names(stats),value = as_tibble_col(stats))
write.csv(stats,file.stats)

