# Preprocessing pipeline for the English Small World of Words project (SWOWEN-2018)
# Author: Simon De Deyne (simon2d@gmail.com), Alvaro Cabana (almadana@gmail.com)
#
# Each file is self-contained, but the entire pipeline can be executed here
#
# Last changed: 16 October 2021

# Create complete data table from responses.csv and participants.csv
source('importRawData.R')

# Compile a Spanish word list to use for participant language checks
source('createWordlist.R')

# Spelling Check - create word lists to manually verify - spell-check
source('createSpellingLists.R')

# Preprocess the data
source('preprocessData.R')

# Sample a balanced dataset
source('balanceParticipants.R')

# Create datafiles with response statistics (types, tokens)
# to avoid inflating the counts
source('createResponseStats.R')

# Create cue- response associative strength table (optional)
source('createAssoStrengthTable.R')

# Create the SWOW-RP graph which will inform us on the strongly connected
# component, which will be considered when calculating cue stats coverage
source('createSWOWGraph.R')

# Create datafiles with cue statistics (# responses, unknown, missing, H)
source('createCueStats.R')

# Generate coverage plot
source('plotCoverage.R')

# Generate vocabulary growth plot by fitting a Zipf Mandbrot model
# (Might get stuck depending on default options, see script)
#source('plotVocabularyGrowth.R')

# Predict chaining (warning: slow)
#source('./R/calculateR12ResponseChaining.R')

# Compute random walk similarity matrix
#source('./R/graphRandomWalk.R')

#-- Similarity correlations ----

# Compute Graph Embeddings
#source("createGraphEmbedding2.R")

# Update similarity judgments in experimental datasets
#source("similarity_update_2.R")