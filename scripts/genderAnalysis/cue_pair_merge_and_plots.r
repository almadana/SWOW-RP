# plot cue pair (raw) distances before and after normalization 
#this is before building the graph (and hence restricting the number of pairs to only responses that appear as cues)
source('scripts/plot_themes/customTheme.r')
set.seed(8015)
cue_pairs.distances %>%
  group_by(cue_POS) %>% 
  arrange(cosine) %>% 
  mutate(proportion=7/n(),p=runif(n())<proportion,label=ifelse(p,cue,""),rnum=row_number(),
         lavbel=ifelse((rnum %% floor(1/proportion))==1,cue,"" ),
         alfa=ifelse(cue_POS %in% "ADJECTIVE",0.1,0.12)) %>% 
  ungroup() %>% 
  #cuePair_distances %>% 
  drop_na() %>% 
  pivot_longer(cols = starts_with("cosine"),names_to = "pre.pos",values_to = "cosine") %>% 
  filter(cue_POS %in% c("ADJECTIVE","NOUN")) %>% 
  mutate(pre.pos=relevel(factor(pre.pos,labels=c("pre","pos")),ref="pre"),
         lavbel=ifelse(pre.pos %in% "pre","",lavbel))  %>% 
  ggplot(aes(x=pre.pos,y=cosine))+geom_boxplot(width=.3,notch = T)+
  geom_point(aes(alpha=alfa),position = position_jitter(width=.05))+geom_line(aes(group=cue,alpha=alfa))+
  facet_wrap(~cue_POS)+#,labeller=labeller(cue_pos=pos.labeller))+
  labs(x="Gendered responses",y="Cosine similarity between cue pairs",col="Randomization test significance")+
  scale_x_discrete(labels=c("original","normalized"))+
  geom_text(aes(label=lavbel),hjust=-.2)+
  scale_alpha_continuous(range = c(.1,.3),guide=F)+
  theme_Publication()+
  scale_color_manual(values = c("#013D57","#FF8700"))
 


cue_num_pairs.distances %>%
  group_by(cue_POS) %>% 
  arrange(cosine) %>% 
  mutate(proportion=7/n(),p=runif(n())<proportion,label=ifelse(p,cue,""),rnum=row_number(),
         lavbel=ifelse((rnum %% floor(1/proportion))==1,cue,"" ),
         alfa=ifelse(cue_POS %in% "ADJECTIVE",0.01,0.001)) %>% 
  ungroup() %>% 
  #cuePair_distances %>% 
  drop_na() %>% 
  pivot_longer(cols = starts_with("cosine"),names_to = "pre.pos",values_to = "cosine") %>% 
  filter(cue_POS %in% c("ADJECTIVE","NOUN")) %>% 
  mutate(pre.pos=relevel(factor(pre.pos,labels=c("pre","pos")),ref="pre"),
         lavbel=ifelse(pre.pos %in% "pre","",lavbel))  %>% 
  ggplot(aes(x=pre.pos,y=cosine))+geom_boxplot(width=.3,notch = T)+
  geom_point(aes(alpha=alfa),position = position_jitter(width=.05))+geom_line(aes(group=cue,alpha=alfa))+
  facet_wrap(~cue_POS)+#,labeller=labeller(cue_pos=pos.labeller))+
  labs(x="Numbered responses",y="Cosine similarity between cue pairs",col="Randomization test significance")+
  scale_x_discrete(labels=c("original","normalized"))+
  geom_text(aes(label=lavbel),hjust=-.4)+
  scale_alpha_continuous(range = c(.1,.3),guide=F)+
  theme_Publication()+
  scale_color_manual(values = c("#013D57","#FF8700"))
