
# makeBootSamples=function(df) {
#   nReps=50
#   listBoots=paste0("bootSample",1:nReps)
#   bigDF = do.call(rbind,
#                   lapply(listBoots, function(x) {
#                     df[sample(1:nrow(df),75,prob = df$p,replace = T) ,"response"] %>% 
#                       group_by(response)%>% 
#                       summarize(R1=n(),
#                                 id=x) %>% 
#                       group_by(id) %>% 
#                       nest()
#                   })
#   )
# 
# }

replicaCosines = function(p,N) {
  nReps=10000
  nTypes = length(p)
  listBoots=paste0("bootSample",1:nReps)
  #bigDF = do.call(rbind,
  replicas=
  lapply(listBoots, function(x) {
                    suppressMessages( philentropy::distance(
                      rbind(rbinom(nTypes,N,p),rbinom(nTypes,N,p)),
                      "cosine"))
                  }) %>% unlist()
  sort(replicas,decreasing = T)
  #data.frame(replicas=replicas,sample=listBoots)
}


#sampling the multinomial distribution from swowCue

randomization.distribution=swowCuePairStrength1_fullpairs %>%  
  filter(response %in% cues$cue) %>% #keep only responses that are in the cue list
  group_by(nPair,response) %>% 
  summarize(sR1=sum(R1)) %>% 
  group_by(nPair) %>% 
  mutate(nR1=sum(sR1),p=sR1/nR1) %>%
  summarize(cosine=replicaCosines(p,nR1[1]))
  #summarize(cosine=philentropy::distance(
  #  rbind(rbinom(n(),nR1[1],p),rbinom(n(),nR1[1],p)),
   # "cosine"))

#randomization distribution. 99th percentile
rand.thresholds = randomization.distribution %>% slice_tail(n = 100) %>% summarize(p001 = cosine[91],p01=cosine[1])

cuePair_distances=cuePair_distances %>% left_join(rand.thresholds,by="nPair") %>% 
  mutate(sign01=cosine<p01,sign001=cosine<p001)

# 
# bootSamplesCuePairStrength1 = swowCuePairStrength1 %>% head(1000) %>% group_by(cue) %>% mutate(p=R1/N) %>% nest %>%  mutate(replica=map(data, makeBootSamples) )
# 
# bb1 = swowCuePairStrength1_fullpairs %>% group_by(nPair) %>% mutate(N=sum(R1)) %>% 
#   group_by(nPair,response) %>% 
#   summarize(R1=sum(R1),p=R1/N)%>% View()
#   %>% group_by(nPair) %>% 
#   nest()
# View(bb1)
# mutate(replica=map(data, makeBootSamples) )
# 
# 
# ff=bootSamplesCuePairStrength1 %>% head()
# 
# compareBootAssociates <- function(df1,df2) {
#   df2 %>% mutate(dummy=map(data,function(x) {
#     rbind(cbind(cue="data",df1[,c("response","R1")]),cbind(cue="boot",x)) %>% 
#       pivot_wider(names_from = cue,values_from=R1,values_fill=0) %>%  
#       summarize(tanimoto=philentropy::distance(rbind(data,boot),method="tanimoto"),
#                 cosine=philentropy::distance(rbind(data,boot),method="cosine"),
#                 jensen_shannon=philentropy::distance(rbind(data,boot),method="jensen-shannon"))
#       }
#     ) ) %>% 
#   unnest(dummy)%>% 
#   ungroup() %>% 
#   summarise(across(.cols = c("tanimoto","cosine","jensen_shannon"),
#                    list(median=median,mean=mean,perc2.5=~quantile(.x,.025))))
# }
# 
# ffd= ff %>% mutate(stats=map2(data,replica,compareBootAssociates)) %>% unnest(stats)
# View(ffd)
