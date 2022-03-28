
# mulitiple relplacement function
#source('./R/functions/mgsub.R')

# Convert to long format and translate British to American or remove British ones if American exists
importDataSWOW <- function(dataFile,response) {
  X       = read.csv(dataFile, header = TRUE, sep=",", dec=".",quote = "\"",encoding = "UTF-8",stringsAsFactors = FALSE)

  # Convert to a long (tall) format
  X       = gather(X,RPOS,response,R1,R2,R3,factor_key = FALSE)

  # Remove the brexit words
  #X       = brexitWords(X)

  # Decide which responses to keep
  switch(response,
         R1 = { X = filter(X,RPOS =='R1') },
         R2 = { X = filter(X,RPOS =='R2') },
         R3 = { X = filter(X,RPOS =='R3') },
         R12 = { X = filter(X,RPOS %in% c('R1','R2')) },
         R123 = { X = X })

  return(X %>% as_tibble())
}


#' Import association data as unipartite graph
#'
#' The graph should have two columns, cue, and response
#'
#' @param X
#'
#' @return
#' @export
#'
#' @examples G = importGraph(SWen,response = 'R1')
importGraph <- function(X,Freq = "R1"){
  # Only keep responses that are also cues
  X = X %>% filter(response %in% X$cue) %>% select(from = cue,to = response, weight = Freq)

  # Convert to graph
  return(tidygraph::as_tbl_graph(X))
}



countResponses <- function(X){
  C     = X %>% select(cue,response) %>% group_by(cue,response)  %>% summarise (Freq = n())
  return(C)
}



