

# normalized term frequency
ntf <- function(){
  
}

# term frequency inverse document frequency
tfidf <- function(tf,idf){
  # dot product of term frequency and inverse document frequency
  
}

# count number of times a citation appears
tf <- function(citationVector, citationVocab){
  m <- length(citationVocab)
  tf <- rep(0,m)

  for(i in 1:length(citationVector)){
    b <- str_detect(citationVocab, pattern = citationVector[i])
    tf[b] <- tf[b]+1
  }
  return(tf)
}

idf <- function(){
  
}