# Old matlab code
# %% weighting function (raw, ppmi, or rw)
#   w       = 'ppmi';
# D       = spdiags(ones(size(G.raw,1),1),0,size(G.raw,1),size(G.raw,2));
# S_1     = D + G.(w) + G.(w)'; % undirected paths of length 1
# S_2     = S_1 * S_1;  % undirected (mediated) paths of length 2
# S_2     = rowNormal(S_2 + S_1);
# 
# %% Set up the structure
# ks = 400;
# G.emb.resp      = 'R123';
# G.emb.graph     = 'Undirected';
# G.emb.weight    = 'ppmi';
# G.emb.ks        = ks;
# 
# %%
# for i=1:numel(ks)
#     fprintf('.');
#     k = ks(i);
#     [U,S,V] = svds(S_2,k);
#     col = ['k' int2str(k)];
#     G.emb.(col) = U*S; % this seems to be key, but is not indicated as such in Steyvers
# end
# 
# save('SWOWEN_Undirected','G','-v7.3');
# 
# 
library(tictoc)
library(irlba)
library(tidyverse)
source('../functions/importDataFunctions.R')
source('../functions/networkFunctions.R')
source('../functions/similarityFunctions.R')
source('settings.R')


# Load the data
#file.swow = paste0('../data/SWOW/output/strength.SWOW-RP.R1.16-10-2021.csv')
#X.swow = read.delim(file.swow,stringsAsFactors = F,sep = '\t')
response = 'R123'
file.swow  = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
X.swow = countResponses(importDataSWOW(file.swow,response))
G.R123 = importGraph(X.swow)

G.strength = igraph::as_adjacency_matrix(G.R123,attr='weight',names = TRUE)
G.strength = normalize(G.strength,'l1')
G.ppmi = normalize(PPMI(G.strength),'l1')



D   = Matrix::Diagonal(dim(G.ppmi)[1])
S_1 = D + G.ppmi + Matrix::t(G.ppmi) # undirected paths of length 1
S_2 = normalize(S_1 + S_1 %*% S_1,'l1') # undirected (mediated) paths of length 2

ks = c(100,200,300,400,500,600)
for(k in ks){
  message('Calculating SVD with k = ',k)
  S2_svd = irlba::irlba(S_2,k)
  G.d = S2_svd$u %*% Matrix::Diagonal(x = S2_svd$d)
  rownames(G.d) = igraph::V(G.R123)$name
  file.output = paste0('../data/SWOW/output/similarity/embeddings/',
                       'SWOW-RP.',response,'.undir.ppmi.k',k,
                       '.',release,'.csv')
  headerLine = paste0(dim(G.ppmi)[1],' ',k)
  write(headerLine,file.output)
  
  write.table(as.matrix(G.d),
              file = file.output,
              quote=TRUE,sep=' ',row.names = TRUE, append = TRUE, 
              col.names = FALSE)
  rm(S2_svd,G.d)
}



# Random walk graphs
alpha = 0.75
tic()
G_rw  = katzIndex(G.ppmi,alpha)
toc()

file.output = paste0('../data/SWOW/output/similarity/embeddings/',
                     'SWOW-RP.',response,'.G.ppmi.alpha75',
                     '.',release,'.csv')


save(G_rw,file = file.output,compress=TRUE,compression_level = 9)


#load k=300
G.d300.file =   paste0('../data/SWOW/output/similarity/embeddings/',
                       'SWOW-RP.',response,'.undir.ppmi.k',300,
                       '.',release,'.csv')
G.d300= read.csv(G.d300.file,header=F,sep=" ",skip = 1) %>% column_to_rownames(var = "V1")

#add dimnames to G_rw
#G_rw = igraph::as_adjacency_matrix(G_rw,attr='weight',names = TRUE)
dimnames(G_rw) = dimnames(G.ppmi)



source("similarity_update_2.R")
# 
# #G = G.d300
# #Gn = normalize(G,norm = 'l2')
# #Gn[1:10,1:10]
# #S = tcrossprod(Gn)
# #S = as.array(S)
# # result = sort(S.d300['bananas',],decreasing = T,index.return = T)
# # result$x[1:10]
# # 
# # igraph::get.all.shortest.paths(G.R1,'banana',to = 'bananas')$res[[1]]
# # 
# 
# # alpha = 0.4
# # P = igraph::as_adjacency_matrix(G.ppmi,attr='weight',names = TRUE)
# # I = diag(1, dim(P)[1]);
# # K = solve(I - alpha*P);
# # P = PPMI(K)
# # diag(P) = 0
# # P = normalize(P,'l1')
# # G.rw = igraph::graph_from_adjacency_matrix(Matrix::as.matrix(P),weighted = TRUE)
# 
# # Generate the weighted graphs
# #G                   = list()
# #G$R1$Strength       = weightMatrix(X.R1,'strength')
# 
# # Evaluate
# X.ratings = read.delim('../data/similarity/simlex_es.csv',stringsAsFactors = F,
#                        encoding='UTF8') %>% as_tibble()
# 
# colnames(X.ratings)=c("wordA","wordB","rating")
# 
# # Extract unique word list from the ratings that are also in the list of vertices 
# wordlist = unique(c(as.character(X.ratings$wordA),as.character(X.ratings$wordB)))
# 
# #print(X.ratings %>% group_by(dataset) %>% summarise(n = n()))
# vNames = igraph::V(G.R123)$name
# wordlist = intersect(wordlist,vNames)
# 
# pairs = X.ratings %>% 
#   mutate(wordA = as.character(wordA),wordB = as.character(wordB)) %>%
#   filter(wordA %in% vNames,wordB %in% vNames) %>% 
#   group_by(wordA,wordB) %>% tally() 
# 
# 
# 
# 
# # Calculate strength similarity
# S.strength = cosineMatrix(G.R123[wordlist,])
# S.d300b  = cosineMatrix(G.d300[wordlist,])
# # S.d100   = cosineMatrix(G.d100[wordlist,])
# # S.d500   = cosineMatrix(G.d500[wordlist,])
# S.ppmi = cosineMatrix(G.ppmi[wordlist,])
# S.rw = cosineMatrix(G_rw[wordlist,])
# 
# X.sim = tibble('wordA' =pairs$wordA, 'wordB' = pairs$wordB,
#                'cos_strength' = S.strength[cbind(pairs$wordA,pairs$wordB)],
#                'cos_d300' = S.d300b[cbind(pairs$wordA,pairs$wordB)],
#                'cos_ppmi' = S.ppmi[cbind(pairs$wordA,pairs$wordB)],
#                'cos_rw' = S.rw[cbind(pairs$wordA,pairs$wordB)]
#                )
#                
# 
# 
# X.results = inner_join(X.ratings,X.sim, by = c('wordA','wordB'))
# X.results$dataset = 'Simlex-ES'
# 
# X.r = X.results %>% nest(data = -dataset) %>% 
#   mutate(r_strength = map(data, ~ cor.test(.x$rating,.x$cos_strength,method = 'spearman')),
#          r_strength = map(r_strength, tidy)) %>% 
#   mutate(r_d300 = map(data, ~ cor.test(.x$rating,.x$cos_d300,method = 'spearman')),
#          r_d300 = map(r_d300,tidy)) %>% 
#   mutate(r_d100 = map(data, ~ cor.test(.x$rating,.x$cos_d100,method = 'spearman')),
#          r_d100 = map(r_d100,tidy)) %>%
#   mutate(r_d500 = map(data, ~ cor.test(.x$rating,.x$cos_d500,method = 'spearman')),
#        r_d500 = map(r_d500,tidy))
# 
#   
# X.r = X.r %>% unnest(cols = c(r_strength,r_d300,r_d100,r_d500), names_sep = '.')
# 
# print(X.r %>% mutate(r.strength = round(r_strength.estimate,3),
#                      r.d100 = round(r_d100.estimate,3),
#                      r.d300 = round(r_d300.estimate,3),
#                      r.d500 = round(r_d500.estimate,3),
#                      ) %>% 
#         select(dataset,r.strength,r.d100,r.d300,r.d500))
# 
# 
# 
# # 'cos_ppmi' = S.ppmi[cbind(pairs$WordA,pairs$WordB)],
#              # 'cos_rw' = S.rw[cbind(pairs$WordA,pairs$WordB)]
#              # ')
