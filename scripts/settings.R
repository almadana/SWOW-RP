
# Release versioning by date in format(Sys.Date(),'%d-%m-%Y')
release = '26-04-2022'


# Determine unknown and missing response tokens
unknown.Token = c('No conozco la palabra','No conozco la Palabra' ,'Unknown word', 
                  'unknown word', 'Unknown Word')
missing.Token = c("No más respuestas","No more responses",'..',
                  'no hay respuestas mas',"")

listlength.Min = 14
listlength.Max = 18
listlength.default = 14
age.Min        = 16

# Participants who tested the experiments (will be excluded)
#testsubjects = c(1,2,71,7334,7336,36869,60804,76083,76308,83324,
#89552,89569,99569,100429,112713,122019,122857)

# Languages considered native
nativeLanguages = c('Argentina Cordobés','Argentina Rioplatense',
                    'Uruguay Rioplatense','Argentina Nor-oriental')

responseCountTreshold = 70*3


# Criteria for removing participants with
# 1. over 70% missing or unknown responses
criteria.X = 0.7

# 2. less than 60% of responses in Spanish lexicon
criteria.Spanish = 0.6

# 3. more than 20% of responses not unique (sex,sex,sex)
criteria.Repeat = 0.2

# 4. more than 30% of responses are multi-word
criteria.Ngram = 0.3

# More than 15% of responses are ALL CAPS -> serial ALL-CAPPER
criteria.allCaps = 0.15

# More than 30% of responses are three words for R1 -> serial three-responder
criteria.nWords = 0.3
