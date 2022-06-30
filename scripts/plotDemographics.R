library(tidyverse)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(sf)
library(maps)
library(mapdata)
library(tmap)

# Theme settings
theme_set(theme_bw())
percent_labeller = scales::label_percent()
numberer = scales::label_number(accuracy=1)

source('settings.R')

file.pp = paste0('../data/SWOW/processed/SWOW-RP.PP.',release,'.csv')
PP = read_csv(file.pp,show_col_types = FALSE) %>% as_tibble()
file.participants = paste0('../data/SWOW/raw/participants.',release,'.csv')
PPraw = read.csv(file.participants,
                 stringsAsFactors = F,
                 strip.white = T,
                 na.strings = c("NULL","NAN"),
                 fileEncoding = 'UTF-8')

PP = left_join(PP,PPraw %>% select(id,latitude,longitude),
               by = c('participantID'='id'))
educationLabels =  c("Incomplete primary","Primary","Secondary",
                     "University degree","Postgraduate")

PP = PP %>% mutate(Education = factor(education,labels=educationLabels)) 
PP = PP %>% mutate(Education = fct_rev(Education))

X.demo = PP %>% filter(age >= 16,gender %in% c("Fe","Ma")) %>% 
  group_by(gender,Education,ageGroup = age %/% 5) %>% 
  summarize(n=n(),.groups = 'drop') %>% 
  mutate(age = ageGroup * 5) %>%
  mutate(n=n*(-1)^( 2 - (gender %in% "Fe")))

# https://community.rstudio.com/t/ggplot2-is-it-possible-to-combine-color-fill-and-size-legends/17072
# https://stackoverflow.com/questions/40612971/alpha-and-fill-legends-in-ggplot2-boxplots
#install.packages(c("pal","paletteer))
result = paletteer::paletteer_c("pals::kovesi.diverging_bwr_40_95_c42", 10)
mypal = c(rev(result[1:5]),result[6:10]) # unlist(as.list(result))

educ.gend = with(X.demo,interaction(Education,gender,sep="-"))
educ.gend.levels  = levels(educ.gend)
educ.gend.levels  = setNames(educ.gend.levels,educ.gend.levels)

fig.demo =  ggplot(X.demo,aes(x=age,y=n,fill=interaction(Education,gender,sep="-")))+ 
  geom_bar(position = 'stack',stat = "identity") +
  scale_y_continuous(breaks = seq(-5000,1000,1000), 
                     labels = as.character(c(seq(5000,0,-1000),1000))) +
  labs(x="Age",y="Frequency",fill="Gender") +
  coord_flip() +
  #scale_fill_brewer(name = 'Education & Gender',palette = 'RdYlGn',direction = -1) +
  scale_fill_manual(name =  'Education & Gender',values  = mypal) +
  new_scale_fill()+
  scale_fill_manual(name =  'Females',values  = mypal[1:5],labels = educ.gend.levels[1:5]) +
  theme_minimal() +
  theme(text=element_text(size = 10))
fig.demo

file.fig.demo = '../figures/participantDemographics'
ggsave(fig.demo,file=paste0(file.fig.demo,'.png'),width = 6,height=4)
ggsave(fig.demo,file=paste0(file.fig.demo,'.svg'),width = 6,height=4)


## Maps
#load participants data
languageLabels = c("Cordobés (ARG)","Nor-oriental guaraní (ARG)",
                   "Rioplatense (ARG)","Rioplatense (URU)")
PP = PP %>% filter(nativeLanguage %in% c("ARG_C","ARG_N","ARG_R","URU_R")) %>%
  mutate(nativeLanguage = factor(nativeLanguage,labels = languageLabels))

world_lr = map_data("world")

world.map = ggplot(data = world_lr) +
  geom_polygon(fill="white",aes(x=long,y=lat,group=group),col="gray") +
  coord_sf(xlim = c(-90, -30), ylim = c(-63, -15), expand = FALSE)

# Todo: add major capitals
PP.location = PP %>% filter(complete.cases(latitude),
                            latitude!=-1 & longitude !=-1) %>%
              group_by(nativeLanguage,latitude = round(latitude,5),
                       longitude = round(longitude,5)) %>% tally()
fig.map1 =  ggplot(data=world_lr) +
  geom_polygon(fill="white",aes(x=long,y=lat,group=group),col="gray50",size=0.2) +
  geom_point(data=PP.location,
             aes(x=longitude,y=latitude,col=nativeLanguage,alpha=n),size= 0.9) +
  coord_sf(ylim = c(-63, 80), expand = FALSE) +
  theme_void() +
  scale_alpha(guide="none",range = c(0.2,0.6)) +
  scale_color_brewer(palette = 'Dark2',guide = guide_legend(override.aes = list(alpha=1))) +
  labs(x="",y="",col="Spanish variant")+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),panel.background = element_blank(),
        panel.spacing = unit(0,units = "cm"))

fig.map2 = world.map +
    geom_point(data = PP.location, 
               aes(x = longitude,y = latitude,col = nativeLanguage,alpha = n),
               size = 0.9) +
    scale_alpha(guide="none",range = c(0.2,0.6)) +
    coord_sf(xlim = c(-80, -5), ylim = c(-63, -20), expand = FALSE) +
    scale_color_brewer(palette = 'Dark2',guide = guide_legend(override.aes = list(alpha=1))) + 
    labs(x="Longitude",y = "Latitude",col = "Spanish variant") +
  theme(legend.position = c(0.87,0.86),legend.background = element_blank())
fig.map2


fig.map3 = ggdraw() + draw_plot(fig.map2) +
  draw_plot(fig.map1,height=.5,hjust = -.76,vjust=-.26,width = .5) 

file.fig.geo = '../figures/participantGeolocation'
file.fig.demo = '../figures/participantDemographics'
ggsave(fig.map3,file=paste0(file.fig.geo,'.png'),width = 10,height=5)
ggsave(fig.map3,file=paste0(file.fig.geo,'.svg'),width = 10,height=5)
       


#PPraw %>% group_by(osFamily) %>% tally()
#View(PPraw %>% group_by(osFamily,deviceType) %>% tally())
#View(PPraw %>% filter(osFamily == 'AndroidOS',deviceType =='WebKit'))