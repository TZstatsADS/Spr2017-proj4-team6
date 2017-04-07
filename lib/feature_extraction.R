# reads in text file given file path
readTextFile <- function(filePath){
  text <- read_csv(filePath)
  return(text)
}

# create tibble from data frame
createDocumentTermMatrix <- function(text){
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
  tb<- as_tibble(text)
  tb <- tb %>%
    mutate(x = str_replace_all(Coauthor, " ", "")) %>%
    mutate(x = str_replace_all(Coauthor, ";", " ")) %>%
    mutate(y = str_replace_all(Journal, " ", "")) %>%
    mutate(term_col = paste(x, y, Paper))
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
