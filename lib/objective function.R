#objective function

obj_func<-function(X,Y,df,l){
  #X is the matrix of feature of all papers.
  #df is the list of all articles.
  #l is the vector of current tags for all papers.
  #Y is the matrix of researchers for all papers.
  n<-nrow(X)
  sum<-0
  for (i in 1:n){
    for (j in i:n){
      if (l[i]!=l[j]){
        sum<-sum+dist_feature(X[i,],X[j,],A)*
          (0.7*c_2(X[i,],X[j,],df)+c_6(x[i,],x[j,],df))
      }
    }
    sum<-sum+dist_feature(X[i,],Y[i,],A)
  }
}