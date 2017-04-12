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
    mutate(x = str_replace_all(x, ";", " ")) %>%
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
  return(as.vector(r))
}

# wrapper function for our clustering method (Paper #3)
runSpectralClusteringQR <- function(tfidf, numAuthors){
  # cosSparse method relies on the qlcMatrix library
  docsdissim <- cosSparse(t(as.matrix(tfidf)))
  
  ## apply k-way spectral clustering
  r <- specClusteringQR(as.matrix(docsdissim), numAuthors)
  return(as.vector(r))
}

# wrapper function for spectral clustering with K-means
runSpectralClusteringKMeans <- function(tfidf, numAuthors){
  r <- specClusteringKM(as.matrix(tfidf), numAuthors)
  return(as.vector(r))
}

runStudy <- function(filePath, fun_name){
  text <- read_csv(filePath)
  dtm <- createCitationMatrix(text)
  tfidf <- weightTfIdf(dtm,normalize = FALSE)
  
  # we need author_id for each paper to make dis-similarity matrix
  author_name <- text$QuestAuthor[1]
  author_id <- text$AuthorID
  num_authors <- length(unique(author_id))
  
  # run our spectral clustering method with QR decomposition
  start.time <- Sys.time()
  f <- match.fun(fun_name)
  r1 <- f(tfidf, num_authors)
  end.time <- Sys.time()
  t1 <- end.time - start.time
  start.time <- NULL
  end.time <- NULL
  
  m1 <- matching_matrix(author_id,r1)
  p1 <- performance_statistics(m1)
  
  return(c(author_name, fun_name, p1$precision, p1$recall, p1$f1, p1$accuracy, p1$mcc, p1$mcc, t1))
}


runAuthorStudy <- function(filePath){
  text <- read_csv(filePath)
  dtm <- createCitationMatrix(text)
  tfidf <- weightTfIdf(dtm,normalize = FALSE)
  
  # we need author_id for each paper to make dis-similarity matrix
  author_name <- text$QuestAuthor[1]
  author_id <- text$AuthorID
  num_authors <- length(unique(author_id))
  
  # run our spectral clustering method with QR decomposition
  start.time <- Sys.time()
  r1 <- runPaperThreeClusteringMethod(tfidf, num_authors)
  end.time <- Sys.time()
  t1 <- end.time - start.time
  start.time <- NULL
  end.time <- NULL
  
  m1 <- matching_matrix(author_id,r1)
  p1 <- performance_statistics(m1)
  
  return(c(author_name, "QR Spectral Clustering", p1$precision, p1$recall, p1$f1, p1$accuracy, p1$mcc, p1$mcc))
}

runAuthorStudyDeprecated <- function(filePath){
  text <- read_csv(filePath)
  dtm <- createCitationMatrix(text)
  tfidf <- weightTfIdf(dtm,normalize = FALSE)
  
  # we need author_id for each paper to make dis-similarity matrix
  author_name <- text$QuestAuthor[1]
  author_id <- text$AuthorID
  num_authors <- length(unique(author_id))
  
  # run teacher's method
  start.time <- Sys.time()
  r0 <- runTeacherClusteringMethod(tfidf, num_authors)
  end.time <- Sys.time()
  t0 <- end.time - start.time
  
  # run our spectral clustering method with QR decomposition
  start.time <- Sys.time()
  r1 <- runPaperThreeClusteringMethod(tfidf, num_authors)
  end.time <- Sys.time()
  t1 <- end.time - start.time
  
  # run our spectral clustering method with K-Means
  start.time <- Sys.time()
  r2 <- runSpectralClusteringKMeans(tfidf, num_authors)
  end.time <- Sys.time()
  t2 <- end.time - start.time
  
  # get matching matrices
  m0 <- matching_matrix(author_id,r0)
  m1 <- matching_matrix(author_id,r1)
  m2 <- matching_matrix(author_id,r2)
  
  # calculate pormance statistics
  p0 <- performance_statistics(m0)
  p1 <- performance_statistics(m1)
  p2 <- performance_statistics(m2)
  
  # method	precision	recall	f1	accuracy	mcc	time
  cdf <- data.frame(
    study=rep(author_name, 3), # must change according to number of studies performed
    method=c("Teacher Kmeans","QR Spectral Clustering", "Kmeans Spectral Clustering"),
    precision=c(p0$precision, p1$precision, p2$precision),
    recall=c(p0$recall, p1$recall, p2$recall),
    f1=c(p0$f1, p1$f1, p2$f1),
    accuracy=c(p0$accuracy, p1$accuracy, p2$accuracy),
    mcc=c(p0$mcc, p1$mcc, p2$mcc),
    time=c(t0, t1, t2)
  )
  return(cdf)
}