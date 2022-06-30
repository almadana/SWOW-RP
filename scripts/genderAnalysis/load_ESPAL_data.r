# Load EsPal cue and lemma data file

esPal.data.file = '../data/ESPAL/espal_lexicon_pos.tsv'
esPal.data = read.csv(esPal.data.file,sep="\t") %>% distinct()
colnames(esPal.data)[2:3] = c("POS","lemma")
