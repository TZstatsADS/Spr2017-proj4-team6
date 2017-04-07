# reads in text file given file path
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

# citation matrix using the method described in paper #3
createCitationMatrix <- function(text){
  library(tidytext)
  library(tibble)
  library(readr)
  tb<- createCitationMatrix(text)
  tb <- tb %>%
    mutate(coauthor = str_replace_all(coauthor, " ", "")) %>%
    mutate(coauthor = str_replace_all(coauthor, ";", " ")) %>%
    mutate(journal = str_replace_all(journal, " ", "")) %>%
    mutate(term_col = paste(coauthor, journal, paper))
  docs <- as.vector(tb$term_col)
  source <- VectorSource(docs)
  stop_words = stopwords(kind = "en")
  corpus <- SimpleCorpus(source)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stop_words)
  dtm <- DocumentTermMatrix(corpus)
  return(dtm)
}
