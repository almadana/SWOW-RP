library(htmlTable)
library(gridExtra)


dataFolder='../../paperSwow1/data/'
swowDataFolder='../data/SWOW/'
scriptsFolder='./'
release ='16-10-2021'
source(paste0(scriptsFolder,"plot_themes/customTheme.r"))



swowN=1

swowResponseStats = read.csv(paste0(swowDataFolder,paste0('output/centrality/responseStats.',release,'.csv')),stringsAsFactors = F)
load(paste0(dataFolder,"fernandezResponseStats.RData"))


## compare frequencies

fernandez.swow.merge = fernandezResponseStats %>% inner_join(swowResponseStats,by="response") %>% 
  mutate(totalFreq = 10000*totalFreq/sum(totalFreq,na.rm = T),Freq.R1 = 10000*Freq.R1 / sum(Freq.R1,na.rm = T),dif_log_freq=log10(Freq.R1)-log10(totalFreq))
fernandez.swow.merge$dif_log_freq


top.swow = fernandez.swow.merge %>% 
  slice_max(dif_log_freq,n=10) %>% 
  select(response,dif_log_freq)

top.fernandez=fernandez.swow.merge %>% 
  slice_min(dif_log_freq,n=10) %>% 
  select(response,dif_log_freq)
  
top.freq.table = rbind(top.swow,top.fernandez)
top.freq.table$top = c(rep("swow",10),rep("nalc",10))
top.freq.table$n=rep(1:10,2)
top.freq.table = top.freq.table %>% pivot_wider(names_from = top,values_from=c(response,dif_log_freq))

colnames(top.freq.table)<-c("n","response","response","&Delta;logF","logF")
top.freq.table = top.freq.table[,c(2,4,3,5)]


top.freq.table  %>%txtRound(2) %>% htmlTable(cgroup = c("SWOW>NALC","NALC>SWOW"),n.cgroup = 2)


colnames(top.freq.table)<-c("response","\DeltalogF","response","logF")



colnames(top.fernandez)=c("NALC>SWOW","\U394logF")
colnames(top.swow)=c("SWOW>NALC","\U394logF")

#

ttm = ttheme_minimal(core=list(bg_params=list(fill = "#f8f8f8", col=NA)),
                    colhead=list(bg_params=list(fill="#dadada",col=NA)))
fernandez.swow.merge %>% 
  ggplot(aes(x=dif_log_freq))+geom_histogram(fill="#A83F8E",col="#C16173")+
  annotation_custom(tableGrob(top.fernandez, rows=NULL,theme = ttm), 
                    xmin=-2, xmax=-1.5, ymin=10, ymax=4500)+
  annotation_custom(tableGrob(top.swow, rows=NULL,theme = ttm), 
                    xmin=1, xmax=3, ymin=10, ymax=4500)+
  labs(x="Difference in log frequency (\U394logF)",y="Response count")+
  theme_Publication()


