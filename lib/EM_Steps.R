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
X <- x_agu
Y <- y_agu
l <- l_agu
A <- diag(1, nrow = 577, ncol = 577)
EM_algorithm <- function(X, Y, l, df) {
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



x_agu <- docsdissim(AGupta)
y_ini <- sample(1:26, 577, replace = T)
l_agu <- matrix(NA, ncol = ncol(x_agu), nrow = 26)
for(i in unique(AGupta$AuthorID)) {
  l_agu[i,] <- mean(x_agu[which(i==AGupta$AuthorID), ])
}

y_agu <- matrix(NA, ncol = ncol(x_agu), nrow = 577)
for (i in 1:577) {
  y_agu[i, ] <- l_agu[y_ini[i], ]
  
}
