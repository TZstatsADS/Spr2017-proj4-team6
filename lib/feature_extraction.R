

# normalized term frequency
ntf <- function(){
  
}

# create tibble from data frame
createDocumentTermMatrix <- function(filePath){
  require(tm,tidytext)
  text <- read_csv(filePath)
  docs <- as.vector(text$Paper)
  source <- VectorSource(docs)
  stop_words = stopwords(kind = "en")
  corpus <- SimpleCorpus(source, control = list(language = "en",
                                                removeWords = stop_words,
                                                removeNumbers = TRUE,
                                                stemming = TRUE))
  dtm <- DocumentTermMatrix(corpus)
  return(tidy(dtm))
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