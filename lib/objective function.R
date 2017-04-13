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
          (0.7*c_2(i,j,df)+c_6(i,j,df))
      }
    }
    sum<-sum+dist_feature(X[i,],Y[i,],A)
  }
  return(sum)
}