# obtain a mosaic plot for number and gender 

require(psych)

source('./plot_themes/customTheme.r')

# - fig 4----

strength.R1.file = paste0('../data/SWOW/output/strength.SWOW-RP.R1.',release,'.csv')
strength.R123.file = paste0('../data/SWOW/output/strength.SWOW-RP.R123.',release,'.csv')

X.R1 = read.csv(strength.R1.file,sep="\t")
X.R123 = read.csv(strength.R123.file,sep="\t")

X.R1 = X.R1 %>% inner_join(X.lex,by=c("cue"="word")) %>% rename(cue.gender = gender,cue.lemma=lemma,cue.number=number) %>% 
  inner_join(X.lex,by=c("response"="word")) %>% rename(response.gender = gender,response.lemma=lemma,response.number=number)

X.R1.Types = X.R1 %>% group_by(cue.gender,response.gender) %>% tally() %>% group_by(cue.gender) %>% 
  mutate(prop = 100*n/sum(n),R="R1",tt="Types")
X.R1.Types.total = X.R1 %>% group_by(response.gender) %>% tally() %>% ungroup() %>% 
  mutate(prop = 100*n/sum(n),R="R1",tt="Types",cue.gender="Total")

X.R1.Tokens = X.R1 %>% group_by(cue.gender,response.gender) %>% summarize(n=sum(R1)) %>% group_by(cue.gender) %>% 
  mutate(prop = 100*n/sum(n),R="R1",tt="Tokens")
X.R1.Tokens.total = X.R1 %>% group_by(response.gender) %>% summarize(n=sum(R1)) %>% ungroup() %>% 
  mutate(prop = 100*n/sum(n),R="R1",tt="Tokens",cue.gender="Total")


X.R123 = X.R123 %>% inner_join(X.lex,by=c("cue"="word")) %>% rename(cue.gender = gender,cue.lemma=lemma,cue.number=number) %>% 
  inner_join(X.lex,by=c("response"="word")) %>% rename(response.gender = gender,response.lemma=lemma,response.number=number)

X.R123.Types = X.R123 %>% group_by(cue.gender,response.gender) %>% tally() %>% group_by(cue.gender) %>% 
  mutate(prop = 100*n/sum(n),R="R123",tt="Types")
X.R123.Types.total = X.R123 %>% group_by(response.gender) %>% tally() %>% ungroup() %>% 
  mutate(prop = 100*n/sum(n),R="R123",tt="Types",cue.gender="Total")

X.R123.Tokens = X.R123 %>% group_by(cue.gender,response.gender) %>% summarize(n=sum(R123)) %>% group_by(cue.gender) %>% 
  mutate(prop = 100*n/sum(n),R="R123",tt="Tokens")
X.R123.Tokens.total = X.R123 %>% group_by(response.gender) %>% summarize(n=sum(R123)) %>% ungroup() %>% 
  mutate(prop = 100*n/sum(n),R="R123",tt="Tokens",cue.gender="Total")

X.tokens = rbind(X.R1.Tokens,X.R1.Tokens.total,
                 X.R123.Tokens,X.R123.Tokens.total)


X.tokens %>%  filter(R %in% "R1", ! (cue.gender %in% "Total")) %>% pull(n) %>%   phi()
X.tokens %>%  filter(R %in% "R1", ! (cue.gender %in% "Total")) %>% select(response.gender,cue.gender,n) %>% 
  pivot_wider(names_from = cue.gender,values_from = n) %>% 
  column_to_rownames("response.gender") %>% 
  as.matrix() %>% chisq.test()



plot.gender.assortativity = X.tokens %>% 
  
  ggplot(aes(x=cue.gender,fill=response.gender,y=prop))+geom_bar(stat = "identity")+facet_grid(~R) +
    labs(x="Cue gender",y="Proportion",fill="Response gender")+
    geom_text(aes(label=paste0(round(prop,2),"%")),vjust=.5,hjust=1.5,color="white", size=4,position = position_stack())+
    theme_Publication()+
    scale_fill_brewer(palette = "Set1")+
    theme(legend.key.size = unit(.5,"cm"),legend.position = "top")+
    coord_flip()

plot.gender.assortativity
ggsave(plot.gender.assortativity,file="../figures/fig.gender.assortativity.pdf",width=6)
# 
# library(ggmosaic)
# library(ggpubr)
# 
# 
# 
# # # Plot the results
# X.R123 = X.R123 %>% rename(gender.pp = gender)
# X.R123 = left_join(X.R123,X.lex.pos,by = c('cue'='word'),suffix =c('.cue','.cue'))
# X.R123 = left_join(X.R123,X.lex.pos,by = c('response'='word'),suffix =c('','.response'))
# # 
# X.g = X.R123 %>% filter(!is.na(gender),!is.na(gender.response),!is.na(number),
#                         !is.na(number.response),!is.na(POS),!is.na(POS.response))
# # 
# X.g = X.R123 %>% filter(POS %in% c('ADJECTIVE','NOUN'),
#                      POS.response %in% c('ADJECTIVE','NOUN'))
# # 
# X.pos = X.R123 %>% filter(POS %in% c('ADJECTIVE','NOUN'),
#                           POS.response %in% c('ADJECTIVE','NOUN'))
# # 
# 
# plot.GENDER = X.g %>% 
#   mutate(gender.response.f=factor(gender.response),
#          gender.response.f = relevel(gender.response.f,"Masc")) %>% 
#   ggplot()+geom_mosaic(aes(x=product(gender),fill=gender.response.f))+
#   #facet_wrap(~POS) +
#   geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__gender) %>% 
#                mutate(p = scales::percent ( .wt/sum(.wt))),
#              aes(x = (xmin + xmax) / 2,
#                  y = (ymin + ymax) / 2,
#                  label = p))+
#   theme_Publication()+
#   scale_fill_brewer(palette = "Set1")+
#   labs(x="Cue gender",y="Response gender",fill="",title="Gender")+
#   theme(legend.position="none",text=element_text(size=16))+
#   coord_flip()
# 
# # Assortativity
# #calculate phi-index 
# gender.table = X.g %>% select(gender.response,gender) %>% table()
# chisq.test(gender.table)
# psych::phi(gender.table)
# 
# # plot.POS = 
# # X.g %>% 
# #   ggplot()+geom_mosaic(aes(x=product(POS),fill=POS.response))+
# #   geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__POS) %>% 
# #                mutate(p = scales::percent ( .wt/sum(.wt))),
# #              aes(x = (xmin + xmax) / 2,
# #                  y = (ymin + ymax) / 2,
# #                  label = p))+
# #   theme_Publication()+
# #   scale_fill_brewer(palette = "Set1")+
# #   labs(x="Cue POS",y="Response POS",fill="")+
# #   theme(legend.position="none",text=element_text(size=14))+
# #   coord_flip()
# 
# plot.number = 
#   X.g %>% 
#   mutate(number.response.f=factor(number.response),
#          number.response.f = relevel(number.response.f,"Sing")) %>% 
#   ggplot()+geom_mosaic(aes(x=product(number),fill=number.response.f))+
#   geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__number) %>% 
#                mutate(p = scales::percent ( .wt/sum(.wt))),
#              aes(x = (xmin + xmax) / 2,
#                  y = (ymin + ymax) / 2,
#                  label = p))+
#   theme_Publication()+
#   scale_fill_brewer(palette = "Set1")+
#   labs(x="Cue number",y="Response number",fill="",title="Number")+
#   theme(legend.position="none",text=element_text(size=16))+
#   coord_flip()
# 
# 
# #plot POS
# plot.POS = 
#   X.pos %>% 
#   mutate(POS.response.f=factor(POS.response,labels = c("Adj","Noun")),
#          POS=factor(POS,labels = c("Adj","Noun")),
#          POS.response.f = relevel(POS.response.f,"Noun")) %>% 
#   ggplot()+geom_mosaic(aes(x=product(POS),fill=POS.response.f))+
#   geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__POS) %>% 
#                mutate(p = scales::percent ( .wt/sum(.wt))),
#              aes(x = (xmin + xmax) / 2,
#                  y = (ymin + ymax) / 2,
#                  label = p))+
#   theme_Publication()+
#   scale_fill_brewer(palette = "Set1")+
#   labs(x="Cue POS",y="Response POS",fill="",title="POS")+
#   theme(legend.position="none",text=element_text(size=16))+
#   coord_flip()
# 
# 
# mosaic.plots = ggarrange(plot.GENDER,plot.number,labels = "AUTO",font.label = list(size=18))
