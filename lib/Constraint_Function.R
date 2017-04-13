# Constraint Function:
#---
# p_i is the ith paper tagged to Principle author a 
# p_j is the jth paper tagged to Principle author a
# df contains all information about Principle author a
#---

c_2 <- function(p_i, p_j, df) {
  a <- strsplit(as.character(df$Coauthor[p_i]), "; ")[[1]]
  b <- strsplit(as.character(df$Coauthor[p_j]), "; ")[[1]]
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
  return(unique(unlist(strsplit(as.character(df$Coauthor), split = ";"))))
}

# Take AGupta for example, get union of unique values of authors.
uni <- union_author(AGupta)

# Matrix Mp
M_p <- diag(1, nrow = nrow(df))

# Matrix Mpa
M_pa <- function(a) {
  authors <- union_author(a)
  mat_pa <- matrix(NA, nrow = nrow(a), ncol = length(authors))
  for (i in 1:nrow(mat_pa)) {
    for (j in 1:ncol(mat_pa)) {
      mat_pa[i, j] <- ifelse(grepl(authors[j], a$Coauthor[i])==T, 1, 0)
    }
  }
  return(mat_pa)
}

x <- M_pa(AGupta)

# Matrix Map
M_ap <- function(a) {
  return(t(M_pa(a)))
}

# Matrix Ma
#---
# Input a is the author name
# First get all publications with an author named a.
# Then use union_author() to get union set of all pi.authors

## here we also need a database (called db) that has all publication information including coauthors. 
## ( basically it's the rbind of all 14 datasets, with 6 columns. What we care is the coauthor column)
M_a <- function(a) {
  all_author <- union_author(a)
  mat_a <- matrix(NA, nrow = length(all_author), ncol = length(all_author))
  for (i in all_author) {
    for (j in all_author) {
      a_index <- grep(i, db, value = F)
      b_index <- grep(j, db, value = F)
      mat_a[i, j] <- ifelse(any(a_index %in% b_index), 1, 0)
    }
  }
  return(mat_a)
}


# For illustration, run following code:
# a <- grep(uni[50], AGupta$Coauthor, value = F)
# b <- grep(uni[55], AGupta$Coauthor, value = F)
# any(a %in% b)


# Now write up matrix M!
M <- function(a) {
  m_p <- M_p(a)
  m_pa <- M_pa(a)
  m_ap <- t(m_pa)
  m_a <- M_a(a)
  upper <- cbind(m_p, m_pa)
  lower <- cbind(m_ap, m_a)
  return(m = rbind(upper, lower))
}

#constraint 6

c_6<-function(i,j,M){
  k<-0
  while((M[i,j]!=1)&(k<20)){
    M<-M%*%M
    k<-k+1
  }
  return(0.7^k)
}