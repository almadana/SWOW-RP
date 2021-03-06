# Merge raw data from SQL database
# This file is not part of the SWOW-RP distribution!
#
# Author: Simon De Deyne & Alvaro Cabana
# Last modified 16/10/2021
library(tidyverse)
library(here)
source('settings.R')
file.participants = paste0('../data/SWOW/raw/participants.',release,'.csv') #replace with participants_raw.csv if you use downloaded data from the project webpage
file.responses = paste0('../data/SWOW/raw/responses.',release,'.csv') #replace with responses_raw.csv if you use downloaded data from the project webpage
file.output = paste0('../data/SWOW/raw/SWOW-RP.complete.',release,'.csv')

participants = read.csv(file.participants,
                        stringsAsFactors = F,
                        strip.white = T,
                        na.strings = c("NULL","NAN"),
                        fileEncoding = 'UTF-8')

responses = read.csv(file.responses,
                     stringsAsFactors = F,
                     strip.white = T,
                     na.strings = "NULL")

# Filter variables
participants = participants %>% select(participantID = id,age,nativeLanguage,
                                       gender,education,city,country)

responses = responses %>% select(participantID,responseID = id,section,cue,
                                 R1 = response1Clean,R2 = response2Clean,
                                 R3 = response3Clean)

# Join
X = right_join(participants,responses,by ='participantID')
write.csv(X,file = file.output,row.names = F)
