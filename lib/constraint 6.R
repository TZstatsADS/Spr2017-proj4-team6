#constraint 6

c_6<-function(i,j,M){
  k<-0
  while(M[i,j]!=1){
    M<-M%*%M
    k<-k+1
  }
  return(0.7^k)
}