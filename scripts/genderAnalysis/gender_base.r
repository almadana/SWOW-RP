response_properties = response_parse %>% inner_join(swowResponseStats,by="response")
message(paste("Proportion of R123 types that were assigned gender:", nrow(response_properties) / nrow(swowResponseStats)))
message(paste("Proportion of R123 tokens that were assigned gender:", 
  sum(response_properties$Freq.R123,na.rm = T)/sum(swowResponseStats$Freq.R123,na.rm = T)
  )
)

resp.enR1 = swowResponseStats %>% filter(!is.na(Freq.R1)) %>% select(response,Freq.R1)
resp.enR1_properties = response_properties %>% inner_join(resp.enR1,by="response")

message(paste("Proportion of R1 types that were assigned gender:",
              nrow(response_properties) / nrow(swowResponseStats)
    )
)
message(paste("Proportion of R1 tokens that were assigned gender:",
              sum(resp.enR1_properties$Freq.R1.x,na.rm = T)/sum(resp.enR1$Freq.R1,na.rm = T)
  )
)


response_properties %>% 
  filter(Freq.R123>100) %>% 
  ggplot(aes(x=log(Freq.R123),fill=gender))+geom_histogram(position=position_identity(),alpha=.5)


response_properties %>% 
  filter(Freq.R123>5) %>% 
  group_by(gender) %>% 
  summarize(types.R123=n(),
            tokens.R123 = sum(Freq.R123,na.rm = T)
            ) %>% 
  ungroup() %>% 
  mutate(types.R123 = types.R123 / sum(types.R123),tokens.R123 = tokens.R123/sum(tokens.R123))



resp.enR1_properties %>% 
  filter(Freq.R1.x>5) %>% 
  group_by(gender) %>% 
    summarize(types.R1=n(),
              tokens.R1 = sum(Freq.R1.x,na.rm = T)
    ) %>% 
  ungroup() %>% 
  mutate(types.R1 = types.R1 / sum(types.R1),tokens.R1 = tokens.R1/sum(tokens.R1))


message(paste("Proportion of gendered cues",nrow(cue_parse)/nrow(swowCueStats1)))
cue_parse %>% group_by(gender) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(prop=n/sum(n))
