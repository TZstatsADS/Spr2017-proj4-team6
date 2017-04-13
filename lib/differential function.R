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
    for (j in i+1:n){
      sum<-sum+dif_dist(X[i,],X[j,],A,m)*
        (0.7*c_2(X[i,],X[j,],df)+c_6(x[i,],x[j,],df))+
        dif_dist(X[i,],Y[j,],A,m)
    }
    sum<-sum+dist_feature(X[i,],Y[i,],A)  
  }
  return(sum)
}