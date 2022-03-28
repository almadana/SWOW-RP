# obtain a mosaic plot for number and gender 

source('scripts/plot_themes/customTheme.r')

library(ggmosaic)
library(ggpubr)



# # Plot the results
X.R123 = X.R123 %>% rename(gender.pp = gender)
X.R123 = left_join(X.R123,X.lex.pos,by = c('cue'='word'),suffix =c('.cue','.cue'))
X.R123 = left_join(X.R123,X.lex.pos,by = c('response'='word'),suffix =c('','.response'))
# 
X.g = X.R123 %>% filter(!is.na(gender),!is.na(gender.response),!is.na(number),
                        !is.na(number.response),!is.na(POS),!is.na(POS.response))
# 
X.g = X.R123 %>% filter(POS %in% c('ADJECTIVE','NOUN'),
                     POS.response %in% c('ADJECTIVE','NOUN'))
# 
X.pos = X.R123 %>% filter(POS %in% c('ADJECTIVE','NOUN'),
                          POS.response %in% c('ADJECTIVE','NOUN'))
# 

plot.GENDER = X.g %>% 
  mutate(gender.response.f=factor(gender.response),
         gender.response.f = relevel(gender.response.f,"Masc")) %>% 
  ggplot()+geom_mosaic(aes(x=product(gender),fill=gender.response.f))+
  #facet_wrap(~POS) +
  geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__gender) %>% 
               mutate(p = scales::percent ( .wt/sum(.wt))),
             aes(x = (xmin + xmax) / 2,
                 y = (ymin + ymax) / 2,
                 label = p))+
  theme_Publication()+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Cue gender",y="Response gender",fill="",title="Gender")+
  theme(legend.position="none",text=element_text(size=16))+
  coord_flip()


# plot.POS = 
# X.g %>% 
#   ggplot()+geom_mosaic(aes(x=product(POS),fill=POS.response))+
#   geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__POS) %>% 
#                mutate(p = scales::percent ( .wt/sum(.wt))),
#              aes(x = (xmin + xmax) / 2,
#                  y = (ymin + ymax) / 2,
#                  label = p))+
#   theme_Publication()+
#   scale_fill_brewer(palette = "Set1")+
#   labs(x="Cue POS",y="Response POS",fill="")+
#   theme(legend.position="none",text=element_text(size=14))+
#   coord_flip()

plot.number = 
  X.g %>% 
  mutate(number.response.f=factor(number.response),
         number.response.f = relevel(number.response.f,"Sing")) %>% 
  ggplot()+geom_mosaic(aes(x=product(number),fill=number.response.f))+
  geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__number) %>% 
               mutate(p = scales::percent ( .wt/sum(.wt))),
             aes(x = (xmin + xmax) / 2,
                 y = (ymin + ymax) / 2,
                 label = p))+
  theme_Publication()+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Cue number",y="Response number",fill="",title="Number")+
  theme(legend.position="none",text=element_text(size=16))+
  coord_flip()


#plot POS
plot.POS = 
  X.pos %>% 
  mutate(POS.response.f=factor(POS.response,labels = c("Adj","Noun")),
         POS=factor(POS,labels = c("Adj","Noun")),
         POS.response.f = relevel(POS.response.f,"Noun")) %>% 
  ggplot()+geom_mosaic(aes(x=product(POS),fill=POS.response.f))+
  geom_label(data = layer_data(last_plot(), 1) %>% group_by(x__POS) %>% 
               mutate(p = scales::percent ( .wt/sum(.wt))),
             aes(x = (xmin + xmax) / 2,
                 y = (ymin + ymax) / 2,
                 label = p))+
  theme_Publication()+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Cue POS",y="Response POS",fill="",title="POS")+
  theme(legend.position="none",text=element_text(size=16))+
  coord_flip()


mosaic.plots = ggarrange(plot.GENDER,plot.number,labels = "AUTO",font.label = list(size=18))
