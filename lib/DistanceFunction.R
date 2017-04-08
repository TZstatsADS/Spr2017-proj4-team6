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