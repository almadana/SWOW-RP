

# If participants repeat the same response to a specific cue, recode as 
# "No more responses"
recodeRepeats = function(X,missing.Token,unknown.Token){
  nDoubles = X %>% filter(R1 == R2, ! (R1 %in% unknown.Token)) %>% 
                select(participantID,cue,R1,R2) %>% nrow()

  nDoubles = nDoubles + X %>% 
      filter(R2 == R3, !(R2 %in% missing.Token),
             !(R2 %in% unknown.Token)) %>%
            select(participantID,cue,R2,R3)  %>% nrow()
  
  X = X %>% mutate(R2 = ifelse(R1 == R2 & ! (R1 %in% unknown.Token), 
                               missing.Token[1],R2))
  
  X = X %>% mutate(R3 = ifelse(R2 == R3 & ! (R2  %in% missing.Token) & 
                                 !(R2 %in% unknown.Token), 
                               missing.Token[1],R3))
  
  result = list('message' = paste0('Removed ',nDoubles, ' double responses'),
                'X' = X,
                'nDoubles' = nDoubles)
 return(result) 
}

# Mark both unknown and missing responses. replace them by NA's for the 
# remaining data
recodeMissingResponses = function(X,missing.Token,unknown.Token){
  X = X %>% mutate(isMissing = as.numeric(response %in% missing.Token))
  X = X %>% mutate(isUnknown = as.numeric(response %in% unknown.Token))
  X = X %>% mutate(response = ifelse(isMissing | isUnknown ,NA,response))
  return(X)
}

normalizeMissingResponses = function(X,missing.Token){

  # First response is missing (e.g. blank or other item in missing responses)
  # but R2 and R3 are valid
  # e.g.   # X %>% filter(R1 %in% missing.Token, 
  # !R2 %in% missing.Token, !R3 %in% missing.Token)
  inconsistent = ( (X$R1  %in% missing.Token)) & (! (X$R2 %in% missing.Token)) &
                (! (X$R3 %in% missing.Token))
  X$R1[inconsistent] = X$R2[inconsistent]
  X$R2[inconsistent] = X$R3[inconsistent]
  X$R3[inconsistent] = NA

  # Swap inconsistent missing responses coded in R2 but not present in R3
  inconsistent  = ( (X$R2  %in% missing.Token)) & (! (X$R3 %in% missing.Token))
  X$R2[inconsistent]  = X$R3[inconsistent]
  X$R3[inconsistent]  = missing.Token[1]
 
  nInconsistent       = sum(inconsistent)
  
  # Swap inconsistent missing responses coded in R1 but not present in R2
  inconsistent  = ( (X$R1  %in% missing.Token)) & (! (X$R2 %in% missing.Token))
  nInconsistent       = nInconsistent + sum(inconsistent)
  X$R1[inconsistent]  = X$R2[inconsistent]
  X$R2[inconsistent]  = missing.Token[1]
  
  # Recode all unknown and missing responses as NA
  X = X %>% mutate(R1 = ifelse(R1 %in% c(missing.Token,unknown.Token),NA,R1),
               R2 = ifelse(R2 %in% c(missing.Token,unknown.Token),NA,R2),
               R3 = ifelse(R3 %in% c(missing.Token,unknown.Token),NA,R3))
  
  result = list('message' = paste0('Replaced ',
                     nInconsistent, ' missing responses'),
                'X' = X,
                'nInconsistent' = nInconsistent)
  return(result)
   
}

# Helper function for parseConcatenatedResponses
getMulti <- function(response1,number) {
  result=unlist(stri_split(response1,regex="[\\s,-]+"))[number]
  return( ifelse(result=="",NA,result))
}


# If the responses consist of a list of comma- (or other symbol) 
# separated values, or if the participant has a serial pattern of coming up with 
# three words in the first response slot (usually with no responses on the other 
# slots) ONLY THEN split up
parseConcatenatedResponses = function(X,missing.Token,unknown.Token){
  # are responses separated by a blank, a comma, or a hyphen?
  X = X %>% mutate(nSplits = stri_count(R1, regex = "[\\s,-]+[^\\s,-]") + 1,
                   isUnknown = (R1 %in% unknown.Token),
                   isMissing = R1 %in% missing.Token,
                   nWords = ifelse((isMissing | isUnknown) >  0, 
                                   NA, ifelse(nSplits > 0, nSplits ,0)))
  
  # Proportion of first responses that have three words
  X = X %>% 
    group_by(participantID) %>% 
    mutate(propThreeResponses = sum(nWords %in% c(3,4),na.rm=T)/n(), 
           serialResponder = propThreeResponses > criteria.nWords) 
  
  # If it is a serial responder's response R it has a comma, 
  # AND it is a multi-word response, replace R1,R2 and R3 accordingly
  # Beware, if you mutate R1 first, this won't work!
  X = X %>% 
    #group_by(X) %>% 
    mutate(splitResponse = ( (grepl(",",R1) & nWords>1) | 
                               (serialResponder & nWords>1 & 
                                  !(R1 %in% unknown.Token) & 
                                  !(R1 %in% missing.Token))),
           R2 = ifelse( splitResponse, getMulti(R1,2),R2), 
           R3 = ifelse( splitResponse, getMulti(R1,3),R3),
           R1 = ifelse( splitResponse, getMulti(R1,1),R1))   
  
  X = X %>% ungroup %>% select(-propThreeResponses)
  nMultiResponsesChanged = sum(X$splitResponse,na.rm = T)
  result = list('message' = paste0("Number of multiple responses split: ",
                                    nMultiResponsesChanged),
                 'X' = X,
                'nMultiResponsesChanged' = nMultiResponsesChanged)
  return(result)
}



cleanResponse = function(X){
  ## Replace everything between brackets
  X = X %>% mutate(response_clean = 
                     stri_trim(gsub("\\s*\\([^\\)]+\\)","",response)))
  ## Replace punctuation
  X = X %>% mutate(response_clean = 
                 stri_replace_all(response_clean,"",regex = "[*«»_¿?¡!ç}.:\"]"))
  
  ## Replace whitespace again
  X = X %>% mutate(response_clean = 
                stri_trim(stri_replace_all(response_clean,NA,regex = "^\\s*$")))
  
  # Replace empty responses
  X = X %>% mutate(response_clean = ifelse(response_clean=='',NA,response_clean))  

  # Count number of changed responses
  nReplacements = X %>% filter(response != response_clean) %>% nrow()
  
  # Replace original response
  X = X %>% mutate(response = response_clean) %>% select(-response_clean)
  result = list('message' = paste0('Cleaned ',nReplacements, ' responses.'),
                'X' = X,
                'nReplacements' = nReplacements)
  
  return(result)
}

# Find out which proportion of an individual's responses are capitalized
# Participants either capitalize all responses, or do not capitalize responses 
# at all. The value of 0.15 seems a a good threshold to assume serial 
# capitalization -> responses need to be converted to lowercase
checkCapitalization = function(X){
  X =  X %>% mutate(
          isAllCaps = !is.na( stri_match(response,regex = "^[A-ZÁÑÉÍÚÓ\\s]+$")), 
          isFirstCaps = !is.na(stri_match(response,
                                    regex = "^[A-ZÁÑÉÍÚÓ][^A-ZÁÑÉÍÚÓ]+$"))) %>% 
          group_by(participantID) %>% 
            mutate(prop.isAllCaps = sum(isAllCaps/  sum(!is.na(response)) ), 
                   prop.isFirstCaps = sum(isFirstCaps/  sum(!is.na(response)))) 

  X =  X %>%  mutate(response = ifelse((prop.isAllCaps > criteria.allCaps) | 
                                       (prop.isFirstCaps > criteria.allCaps),
                                       stri_trans_tolower(response), response))
  X = X %>% ungroup
  
  responsesConverted = sum((X$prop.isAllCaps & X$isAllCaps ) > criteria.allCaps,
                           na.rm = T)
  
  result = list('message' = 
                  paste0("Number of responses converted to lowercase: ", 
                         responsesConverted),
                'X' = X,
                'nResponsesConverted' = responsesConverted)
  return(result)
}

# Fix alternative spellings for cues
normalizeCues = function(X,file.cueCorrections){
  X.cuecor  = read.csv(file.cueCorrections,stringsAsFactors = FALSE, 
                             encoding = 'UTF-8') %>% as_tibble()
  
  X = left_join(X,X.cuecor,by = 'cue')
  nCues = X %>% filter(cue!=cueCorrection) %>% tally() %>% nrow()
  X = X %>% mutate(cue = ifelse(!is.na(cueCorrection),cueCorrection,cue)) %>%
              select(-cueCorrection)
  
  result = list('message' = paste0('Normalized ',nCues, ' cue words'),
                'X' = X,
                'nNormalizedCues' = nCues)
  return(result)  
}

# Create a list of contextual spelling corrections for cue - response pairs.
# Output: cue, response, correction
# This list can be used to correct items on the server as well.
spellCheckResponse = function(X,file.lexicon,file.dictionary,
                              file.cueResponseCorrections,
                              file.responseCorrections){

  X.lexicon  = read.csv(file.lexicon, stringsAsFactors = FALSE, 
                          strip.white = TRUE, encoding = 'UTF-8') %>% 
              as_tibble()
  
  
  X.cueResp = read.csv(file.cueResponseCorrections, stringsAsFactors = FALSE,
                     strip.white = TRUE,encoding = 'UTF-8') %>%
              as_tibble()
  
  X.resp = read.csv(file.responseCorrections, stringsAsFactors = FALSE,
                     strip.white = TRUE,encoding = 'UTF-8') %>%
              as_tibble() %>% select(response,correction)
  
  # Make sure responses are consistent (i.e. only a single correction per word)
  X.inconsistent = X.resp %>% group_by(response) %>% tally() %>% filter(n>1)
  if(X.inconsistent %>% nrow() > 0){
    warning('Response corrections are inconsistent')
    X.inconsistent = X.resp %>% 
          filter(response %in% X.inconsistent$response) %>% arrange(response)
    write.csv(X.inconsistent, paste0('../data/SWOW/output/spellingMistakes/',
                'inconsistentResponseCorrections.csv'),row.names = F)
  }

  X  = X %>% mutate(correctSpelling= ifelse(response %in% X.lexicon$Word,
              TRUE,FALSE))
  
  # Generic response corrections
  X = left_join(X,X.resp, by = c('response'))
  
  # Context specific corrections
  X = left_join(X,X.cueResp, by = c('cue','response'))
  
  # Override generic correction
  X = X %>% mutate(correction.x = ifelse(!is.na(correction.y),
                                         correction.y,correction.x)) %>% 
            select(-correction.y) %>% rename(correction = correction.x)
  

  nCorrected = X %>% filter(!is.na(correction)) %>% nrow()
  
  # Extract corrections
  X.corrections = X %>% filter(response != correction & !is.na(correction)) %>%
                    select(cue,response,correction,correctSpelling) %>% group_by(
                      cue,response,correction) %>% tally()
  
  # Replace original response
  X = X %>% mutate(response = ifelse(is.na(correction),response,correction)) %>%
      select(-correction)

  # Multi-word response corrections should be dealt with through cue-response
  # corrections or response corrections explicitly.
 result = list('message' = paste0('A total of ',nCorrected,
                                  ' words were spell-corrected'),
               'X' = X,
               'corrections' = X.corrections,
               'nCorrected' = nCorrected)
 
 return(result)
}

scoreParticipant = function(){
  
}