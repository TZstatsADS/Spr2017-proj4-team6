##########################################################
######## Function to apply Spectral clustering on  #######
######## cosine similarities matrix/ Gram matrix   #######
##########################################################

specClustering <- function(x, k) {
  # x: matrix ->  matrix of cosine similarities/ Gram matrix
  # k: integer -> number of clusters
  
  # 1. Compute X, the n-by-k matrix consisting of the largest 
  #    eigenvectors of the Gram matrix.
  #    Each row of Xk corresponds to a citation vector.
  X <- eigen(x)$vectors[,1:k]
  
  # 2. Perform Pivoted QR Decomposition of X transpose
  #    Then determine matrices Q, R and P
  QR <- qr(t(X))
  P <- diag(length(QR$pivot))[QR$pivot,] # build permutation matrix from pivot provided in QR decomp
  Q <- qr.Q(QR);  R <- qr.R(QR);
  
  # 3. Compute Rhat = Rkk^(-1) x R x P
  Rkk <- R[1:k,1:k]
  Rhat <- solve(Rkk) %*% R
  ## optional: verify that X' x P = Q x R 
  print("Verify that X' x P = Q x R:")
  print(sum(round(t(X)%*%P,7)==round(Q%*%R,7))==dim(t(X)%*%P)[1]*dim(t(X)%*%P)[2])
  
  # 4. Cluster association is row index of the largest element in 
  #    absolute value of the corresponding column of Rhat.
  return (apply(abs(Rhat),2,FUN=function(c)which.max(c)))
}


# test with random normal matrix:
# N <- 10; M <- 10;
# A <- matrix( rnorm(N*M,mean=0,sd=1), N, M) 