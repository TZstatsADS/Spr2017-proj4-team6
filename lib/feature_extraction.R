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

# wrapper function for teacher's clustering method
runTeacherClusteringMethod <- function(tfidf, numAuthors){
  # specc method from kernlab
  r <- specc(as.matrix(tfidf), centers=numAuthors)
  return(r)
}

# wrapper function for our clustering method (Paper #3)
runPaperThreeClusteringMethod <- function(tfidf, numAuthors){
  # cosSparse method relies on the qlcMatrix library
  docsdissim <- cosSparse(t(as.matrix(dtm_train_tfidf)))
  
  ## apply k-way spectral clustering
  r <- specClustering(as.matrix(docsdissim), numAuthors)
  return(r)
}

runAuthorStudy <- function(filePath){
  text <- read_csv(filePath)
  dtm <- createCitationMatrix(text)
  tfidf <- weightTfIdf(dtm,normalize = FALSE)
  
  num_authors <- length(unique(text$AuthorID))
  
  start.time <- Sys.time()
  result0 <- runTeacherClusteringMethod(tfidf, num_authors)
  end.time <- Sys.time()
  t1 <- end.time - start.time
  
  return()
}

runAuthorStudyDummy <- function(filePath){
  accuracy <- runif(1, 1.0, 3.0)
  precision <- runif(1, 0.0, 1.0)
  recall <- runif(1, 20, 50)
  return(c(accuracy,precision,recall))
}