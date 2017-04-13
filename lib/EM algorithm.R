#EM algorithm
#l is the matrix of all author
#Y is the matrix of each paper's author
df<-AGupta
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
    if (Y[j,]==l[i,]) sum_x<-sum_x+X[j,]
    rbind(c,j)
  }
  l[i,]<-sum_x/sqrt(t(sum_x)%*%A%*%sum_x)
  Y[c,]<-l[i,]
}
for(m in 1:n){
  A[m,m]<-A[m,m]+0.75*dif_func(X,Y,A,m)
}
}