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
