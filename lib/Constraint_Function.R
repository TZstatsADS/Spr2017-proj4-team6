# Constraint Function:
#---
# p_i is the ith paper tagged to Principle author a 
# p_j is the jth paper tagged to Principle author a
# df contains all information about Principle author a
#---

c_2 <- function(p_i, p_j, df) {
  a <- strsplit(as.character(df$Coauthor[p_i]), " ; ")[[1]]
  b <- strsplit(as.character(df$Coauthor[p_j]), " ; ")[[1]]
  return(ifelse(any(a %in% b)==T, 1, 0))
}


# Take AGupta for example
AGupta <- read.csv("../output/Agupta.csv")

c_2(6,7,AGupta)
c_2(6,10,AGupta)