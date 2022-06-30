# Generates a WAS embedding (following Steyvers et al., 200?), using the PPMI matrix P as input.
library(tidyverse)

source('settings.R')
source('../functions/importDataFunctions.R')
source('../functions/networkFunctions.R')
source('../functions/similarityFunctions.R')

# Load the data
dataFile.SWOW     = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
SWOW.R123         = importDataSWOW(dataFile.SWOW,'R123')

# Generate the weighted graphs
G                 = list()
#G$R123$Strength   = weightMatrix(SWOW.R123,'strength')
G$R123$PPMI       = weightMatrix(SWOW.R123,'PPMI')
#make symmetric matrix
#Add diagonal?

#Make symmetric/unidrected graph
S1 = diag(nrow(G$R123$PPMI))+ G$R123$PPMI + t(G$R123$PPMI)
S1 = G$R123$PPMI + t(G$R123$PPMI)
S2 = S1 * S1
S = S1 + S2

S=normalize(S)
#computes SVD
S.svd = svd(S,400)
XS=with(S.svd,u * d)
#load embedding matrix

write.csv(XS,"./data/processed/similarity/swow.embedding.was_nodiag.csv")
#S=Matrix(as.matrix())
S=Matrix(XS)
indexes = read.csv(file = "labels.csv",header=F) %>% pull()
S=cosineMatrix(S)
colnames(S)=indexes
rownames(S)=indexes
with(swow_sims,cor(rating,swow_xa,use="complete.obs"))
i=1
