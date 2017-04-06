

# normalized term frequency
ntf <- function(){
  
}

readTextFile <- function(filePath){
  library(readr)
  text <- read_csv(filePath)
  return(text)
}


# create tibble from data frame
createDocumentTermMatrix <- function(text){
  library(tidytext)
  library(tm)
  docs <- as.vector(text$Paper)
  source <- VectorSource(docs)
  stop_words = stopwords(kind = "en")
  corpus <- SimpleCorpus(source)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stop_words)
  dtm <- DocumentTermMatrix(corpus)
  return(dtm)
}

createCitationMatrix <- function(text){
  library(tidytext)
  library(tibble)
  tb <- tibble(coauthor = text$Coauthor, paper = text$Paper, journal = text$Journal)
  return(tb)
}

countDocsUsingWord <- function(tdm){
  require(dplyr)
  ctdm <- tdm %>%
    group_by(term) %>%
    summarise(totwc = count(term))
  return(ctdm)
}

# term frequency inverse document frequency
tfidf <- function(tf,idf){
  # dot product of term frequency and inverse document frequency
  return(tf %*% idf)
}

# inverse document frequency
idf <- function(docsCount, docsUsingWord){
  idf <- log10(docsCount/(1+docsUsingWord))
  return(idf)
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