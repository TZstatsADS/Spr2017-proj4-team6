##########################################################
######## Function to apply Spectral clustering on  #######
######## cosine similarities matrix/ Gram matrix   #######
##########################################################

simFunction <- function(x1, x2, alpha=1) {
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}



similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(my.data[i,], my.data[j,])
    }
  }
  return(S)
}

# matrix power operator: computes M^power (M must be diagonalizable)
"%^%" <- function(M, power){
  return (with(eigen(M), vectors %*% (values^power * solve(vectors))))
}
  
affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  return(A)  
}

library(stats)

specClusteringKM <- function(data, k, plotGraph=F) {
  # compute affinity matrix (positive values and symetric)
  A <- affinity(similarity(data, simFunction), 3)
  # degree matrix DD where each diagonal value is the degree of the respective vertex and all other positions are zero
  D <- diag(apply(A, 1, sum)) # sum rows
  # unnormalized graph Laplacian U
  U <- D - A
  # find the k smallest eigenvectors
  eig <- eigen(U, symmetric=TRUE)
  Z   <- eig$vectors[,(ncol(eig$vectors)-k+1):ncol(eig$vectors)]
  # kmeans
  km <- kmeans(Z, centers=k, nstart=5)
  return (km$cluster)
}


specClusteringQR <- function(x, k) {
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
  #print("Verify that X' x P = Q x R:")
  #print(sum(round(t(X)%*%P,7)==round(Q%*%R,7))==dim(t(X)%*%P)[1]*dim(t(X)%*%P)[2])
  
  # 4. Cluster association is row index of the largest element in 
  #    absolute value of the corresponding column of Rhat.
  return (apply(abs(Rhat),2,FUN=function(c) which.max(c)))
}


# test with random normal matrix:
# N <- 10; M <- 10;
# A <- matrix( rnorm(N*M,mean=0,sd=1), N, M) 
