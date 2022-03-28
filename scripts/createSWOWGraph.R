# Description:
#
# Load English Small world of words data (https://smallworldofwords.org/)
# The input file consists of responses after spell checking and normalizing the tokens (Americanized)
# This script also removes cues that are British variants when an American one is available
#
# For each cue a total of 300 responses are available, consisting of 100 first, 100 second and 100 third responses
#
# The edge weights in the unimodal graph G.raw
# correspond to associative strength p(response|cue) after removing missing and unknown word responses
#
# Note: the data included with this script cannot be distributed without prior consent
# Author: Simon De Deyne simon2d@gmail.com
# Last changed: 20 February 2021

library(tidyverse)
library(igraph)
library(Matrix)

source('settings.R')
source('../functions/importDataFunctions.R')
source('../functions/networkFunctions.R')


file.swow  = paste0('../data/SWOW/processed/SWOW-RP.R70.',release,'.csv')
file.output = paste0('../data/SWOW/output/adjacencyMatrices/SWOW-RP')
file.comp = paste0('../data/SWOW/output/centrality/components.removedV.SWOW-RP.',release,'.csv')



# Import the dataset for R1
response          = 'R1' # Options: R1, R2, R3 or R123
X.R1              = importDataSWOW(file.swow,response)

# Extract unimodal graph (strong component)
G.R1 = createGraph(X.R1)
compStrong.R1 = extractComponent(G.R1,'strong')
G.R1.strong = compStrong.R1$subGraph
X.comp.R1 = data.frame(RPOS = 'R1',type = 'strong',
            vertices = compStrong.R1$removedVertices) %>% as_tibble()

# Write adjacency and label files for G.raw
writeAdjacency(G.R1.strong, paste(file.output,'.',response,'.',
                                release,  '.',sep=''))

# Import the dataset for R123
response          = 'R123' # Options: R1, R2, R3 or R123
X.R123            = importDataSWOW(file.swow,response)

# Extract unimodal graph (strong component)
G.R123 = createGraph(X.R123)
compStrong.R123 = extractComponent(G.R123,'strong')
G.R123.strong = compStrong.R123$subGraph
X.comp.R123 = data.frame(RPOS = 'R123',type = 'strong',
                    vertices = compStrong.R123$removedVertices) %>% as_tibble()


# Write weighted adjacency file
writeAdjacency(G.R123.strong, paste(file.output,'.',response,'.',
                                  release,  '.',sep=''))


# Write removed vertices
X.comp = rbind(X.comp.R1,X.comp.R123)
write.csv(X.comp,file.comp,row.names = F)
