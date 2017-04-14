# All functions used for paper 6

#differential function

dif_dist<-function(x_i,x_j,A,m){
  #take derivative of a_mm
  part1<-x_i[m]*x_j[m]*sqrt(t(x_i)%*%A%*%x_i)*sqrt(t(x_j)%*%A%*%x_j)
  part2<-(t(x_i)%*%A%*%x_j)*((x_i[m]^2)*(t(x_i)%*%A%*%x_i)
                             +(x_j[m]^2)*(t(x_j)%*%A%*%x_j))/(2*sqrt(t(x_i)%*%A%*%x_i)*sqrt(t(x_j)%*%A%*%x_j))
  part3<-(t(x_i)%*%A%*%x_i)*(t(x_j)%*%A%*%x_j)
  return((part1-part2)/part3)
}

dif_func<-function(X,Y,A,df,m){
  n<-nrow(X)
  sum<-0
  for (i in 1:n){
    for (j in i:n-1){
      sum<-sum+dif_dist(X[i,],X[j+1,],A,m)*
        #(0.7*c_2(i,j,df)+c_6(i,j,M))+
        (0.7*c_2(i,j+1,df))+
        dif_dist(X[i,],Y[j+1,],A,m)
    }
  }
  return(sum)
}


#-----------------------------------------------------------------
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
# AGupta <- read.csv("../output/Agupta.csv")
# 
# c_2(6,7,AGupta)
# c_2(6,10,AGupta)



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
M_p <- function(df){
  return(diag(1, nrow = nrow(df)))
}
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

#-------------------------
# Distance Function

# First write a function to calculate norm of a given vector xi
# Since in the partical derivative part we repeatly use the norm
# and matrix A updates in each iteration

#---
# Input : x: feature vector subtracted from the data. (Generated use the paper title, journal name, coauthor) 
#         A: Parameter matrix. Initial value if A is identity matrix, updated along with algorithm
#---
norm_feature <- function(x, A) {
  return(sqrt(t(x)%*%A%*%x))
}


dist_feature <- function(x_i, x_j, A) {
  a <- t(x_i) %*% A %*% x_j
  b <- norm_feature(x_i, A) * norm_feature(x_j, A)
  return(1-a/b)
}

# -------------------------------------------------------------
#objective function

obj_func<-function(X,Y,df){
  #X is the matrix of feature of all papers.
  #df is the list of all articles.
  #Y is the matrix of researchers for all papers.
  n<-nrow(X)
  sum<-0
  for (i in 1:n){
    for (j in i:n){
      if (any(Y[i,]!=Y[j,])){
        sum<-sum+dist_feature(X[i,],X[j,],A)*
          #(0.7*c_2(i,j,df)+c_6(i,j,df))
          (0.7*c_2(i,j,df))
      }
    }
    sum<-sum+dist_feature(X[i,],Y[i,],A)
  }
  return(sum)
}

#----------------------------------------------------------------------------------------
# EM framework

# Initialization of the EM Framework:
# First cluster publications into disjoint groups based on the constraints over them. 
#i.e.if two publications have a constraint, then they are assigned to the same researcher.

# After consideration, we randomly assign tags for papers as initial values.

init_tag <- function(df) {
  return(sample(1:length(unique(df$AuthorID)), nrow(df), replace = T))
}


# init_tag(AKumar)


#-------------
# The input of EM algorithm is the tags we assign to every paper.
# And we try to use EM steps to minimize the objective function

#-----------

#EM algorithm
#l is the matrix of all author
#Y is the matrix of each paper's author

# x_agu <- docsdissim(AGupta)
# y_ini <- sample(1:26, 577, replace = T)
# l_agu <- matrix(NA, ncol = ncol(x_agu), nrow = 26)
# for(i in unique(AGupta$AuthorID)) {
#   l_agu[i,] <- mean(x_agu[which(i==AGupta$AuthorID), ])
# }
# 
# y_agu <- matrix(NA, ncol = ncol(x_agu), nrow = 577)
# for (i in 1:577) {
#   y_agu[i, ] <- l_agu[y_ini[i], ]
#   
# }
# 
# 
# X <- x_agu
# Y <- y_agu
# l <- l_agu
# A <- diag(1, nrow = 577, ncol = 577)

EM_algorithm <- function(X, Y, l, df) {
  k<-max(df$AuthorID)
  n<-nrow(X)
  numb<-0
  while(numb<1000){
    Y_stop<-Y
    #E-step
    for(i in 1:n){
      Y_tem<-Y
      tem<-NULL
      for (j in 1:k){
        Y_tem[i,]<-l[j,]
        tem[j]<-obj_func(X,Y_tem,df)
      }
      Y[i,]<-l[which.min(tem),]
      numb<-numb+1
    }
    if(obj_func(X,Y,df)==obj_func(X,Y_stop,df)) break
    #M-step
    for (i in 1:k){
      sum_x<-0
      c<-NULL
      for(j in 1:n){
        if (all(Y[j,]==l[i,])) {
          sum_x<-sum_x+X[j,]
          rbind(c,j)
        }
      }
      l[i,]<-sum_x/sqrt(t(sum_x)%*%A%*%sum_x)
      Y[c,]<-l[i,]
    }
    for(m in 1:n){
      A[m,m]<-A[m,m]+0.75*dif_func(X,Y,A,m)
    }
  }
  
}



