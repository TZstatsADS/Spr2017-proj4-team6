# Constraint Function:
#---
# p_i is the ith paper tagged to Principle author a 
# p_j is the jth paper tagged to Principle author a
# df contains all information about Principle author a
#---

c_2 <- function(p_i, p_j, df) {
  a <- strsplit(as.character(df$Coauthor[p_i]), " ; ")[[1]]
  b <- strsplit(as.character(df$Coauthor[p_j]), " ; ")[[1]]
  return(ifelse(any(a %in% b)==T, 1, 0))
}


# Take AGupta for example
AGupta <- read.csv("../output/Agupta.csv")

c_2(6,7,AGupta)
c_2(6,10,AGupta)



# part 4.2 
# Union set of all pi.authors with author named a.
#---
# input: dataframe of author named a
#---
union_author <- function(df) {
  return(unique(unlist(strsplit(df$Coauthor, split = ";"))))
}

# Take AGupta for example, get union of unique values of authors.
uni <- unique(unlist(strsplit(AGupta$Coauthor, split = ";")))

# Matrix Mp
M_p <- diag(1, nrow = nrow(df))
M_pa <- function(df) {
  authors <- union_author(df)
  mat_pa <- matrix(NA, nrow = nrow(df), ncol = length(authors))
  for (i in 1:nrow(mat_pa)) {
    for (j in 1:ncol(mat_pa)) {
      mat_pa[i, j] <- ifelse(grepl(authors[j], df$Coauthor[i])==T, 1, 0)
    }
  }
  return(mat_pa)
}

x <- M_pa(AGupta)
