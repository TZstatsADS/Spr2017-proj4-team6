

# filename <- c("AGupta.txt", "AKumar.txt", "CChen.txt", "DJohnson.txt", "JLee.txt", "JMartin.txt", 
#               "JRobinson.txt", "JSmith.txt", "KTanaka.txt", "MBrown.txt", "MJones.txt", 
#               "MMiller.txt", "SLee.txt", "YChen.txt")

library(dplyr)

# -----------------------------------------------------------
# write a function to convert coauthor names into first initial combined with last name
#---
# the input parameter author is a vector contains all authors related to main author name
# the name parameter is the name of main author
# the return of the function is a list with main author's name removed from coauthor
#---
conv_initial <- function(author, name) {
  for(j in 1:length(author)){
    if(nchar(author[j])>0){
      nam = strsplit(author[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste(first.ini, last.name)
    author[j]=nam.str
  }
  match_ind = charmatch(name, author, nomatch=-1)
  if(match_ind>0){
    author=author[-match_ind]
  }
  return(author)
}
#-------------------------------------------------------------


AKumar <- data.frame(
  scan(
    file.path("..","data","nameset","AKumar.txt", fsep = .Platform$file.sep),
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quiet=TRUE),
  stringsAsFactors=FALSE)
AKumar$AuthorID <- sub("_.*","",AKumar$Coauthor)
AKumar$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor)
AKumar$Coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
AKumar$Paper <- gsub("<","",AKumar$Paper)
AKumar$PaperID <- rownames(AKumar)

AKumar$QuestAuthor <- "A Kumar"
x_list <- strsplit(AKumar$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "A Kumar")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
AKumar$Coauthor <- coauthor_vec

# AKumar$Coauthor <- gsub(";*A Kumar[[:blank:]]*;*", "", AKumar$Coauthor)
AKumar  <- AKumar[,c(4, 5, 7, 1, 2, 3)]
AKumar  <- arrange(AKumar , as.numeric(AuthorID), as.numeric(PaperNO))

# For AGupta (577 lines), there're several issues, "EOF within quoted string"
# "number of items read is not a multiple of the number of columns" 
# 4_48 for example:  
# "4_48 A Gupta;Damon Kaller ; Thomas C Shermer<>Linear-Time Algorithms for Partial <i>k</i>-Tree Complements <>Algorithmica"
#6_44 A Gupta;Alistair Sinclair ; Ilan Newman ; Yuri Rabinovich<>Cuts, Trees and l<sub> </sub>-Embeddings of Graphs <>FOCS IEEE Symposium Foundations of Computer Science
a <- readLines("../data/nameset/AGupta.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
AGupta <- data.frame(
  scan(
    text = a,
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quiet=TRUE,
    quote = NULL),
  stringsAsFactors=FALSE)
AGupta$AuthorID <- sub("_.*","",AGupta$Coauthor)
AGupta$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AGupta$Coauthor)
AGupta$Coauthor <- gsub("<","",sub("^.*?\\s","", AGupta$Coauthor))
AGupta$Paper <- gsub("<","",AGupta$Paper)
AGupta$PaperID <- rownames(AGupta)

AGupta$QuestAuthor <- "A Gupta"
x_list <- strsplit(AGupta$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "A Gupta")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
AGupta$Coauthor <- coauthor_vec
# AGupta$Coauthor <- gsub(";*A Gupta[[:blank:]]*;*", "", AGupta$Coauthor)
AGupta <- AGupta[,c(4, 5, 7, 1, 2, 3)]
AGupta <- arrange(AGupta, as.numeric(AuthorID), as.numeric(PaperNO))



# CChen 800 lines. By looking at dataset, 38_0 should be deleted (title and jornal empty), 39_1 has different format than others.
a <- readLines("../data/nameset/CChen.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
CChen <- data.frame(
  scan(
    text = a,
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quiet=TRUE,
    quote = NULL),
  stringsAsFactors=FALSE)
CChen <- CChen[-599, ]
CChen[602, ] <- c("39_1 C Chen;", "The Complexity of Propositional Modal Theories and the Complexity of Consistency of Propositional Modal Theories <"," LFCS" )
CChen$AuthorID <- sub("_.*","",CChen$Coauthor)
CChen$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", CChen$Coauthor)
CChen$Coauthor <- gsub("<","",sub("^.*?\\s","", CChen$Coauthor))
CChen$Paper <- gsub("<","",CChen$Paper)
CChen$PaperID <- rownames(CChen)

CChen$QuestAuthor <- "C Chen"
x_list <- strsplit(CChen$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "C Chen")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
CChen$Coauthor <- coauthor_vec

# CChen$Coauthor <- gsub(";*C Chen[[:blank:]]*;*", "", CChen$Coauthor)
CChen <- CChen[,c(4, 5, 7, 1, 2, 3)]
CChen <- arrange(CChen, as.numeric(AuthorID), as.numeric(PaperNO))

# 368 for DJohnson
a <- readLines("../data/nameset/DJohnson.txt", encoding = "latin1")
DJohnson <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
DJohnson$AuthorID <- sub("_.*","",DJohnson$Coauthor)
DJohnson$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", DJohnson$Coauthor)
DJohnson$Coauthor <- gsub("<","",sub("^.*?\\s","", DJohnson$Coauthor))
DJohnson$Paper <- gsub("<","",DJohnson$Paper)
DJohnson$PaperID <- rownames(DJohnson)

DJohnson$QuestAuthor <- "D Johnson"

x_list <- strsplit(DJohnson$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "D Johnson")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
DJohnson$Coauthor <- coauthor_vec
# DJohnson$Coauthor <- gsub(";*D Johnson[[:blank:]]*;*", "", DJohnson$Coauthor)
DJohnson <- DJohnson[,c(4, 5, 7, 1, 2, 3)]
DJohnson <- arrange(DJohnson, as.numeric(AuthorID), as.numeric(PaperNO))


# 1419 lines for JLee
a <- readLines("../data/nameset/JLee.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
JLee <- data.frame(
  scan(
    text = a,
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
JLee$AuthorID <- sub("_.*","",JLee$Coauthor)
JLee$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JLee$Coauthor)
JLee$Coauthor <- gsub("<","",sub("^.*?\\s","", JLee$Coauthor))
JLee$Coauthor <- gsub(";[[:blank:]]*", "; ", JLee$Coauthor)
JLee$Paper <- gsub("<","",JLee$Paper)
JLee$PaperID <- rownames(JLee)

JLee$QuestAuthor <- "J Lee"

# x_list <- strsplit(JLee$Coauthor, "; ")
# coauthor_list <- sapply(x_list, conv_initial, name = "J Lee")
# coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
# JLee$Coauthor <- coauthor_vec
JLee$Coauthor <- gsub(";*J Lee[[:blank:]]*;*", "", JLee$Coauthor)
JLee <- JLee[,c(4, 5, 7, 1, 2, 3)]
JLee <- arrange(JLee, as.numeric(AuthorID), as.numeric(PaperNO))

# 112 lines for JMartin
JMartin <- data.frame(
  scan(
    file.path("..","data","nameset","JMartin.txt", fsep = .Platform$file.sep),
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    encoding = "latin1",
    quiet=TRUE),
  stringsAsFactors=FALSE)
JMartin$AuthorID <- sub("_.*","",JMartin$Coauthor)
JMartin$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JMartin$Coauthor)
JMartin$Coauthor <- gsub("<","",sub("^.*?\\s","", JMartin$Coauthor))
JMartin$Paper <- gsub("<","",JMartin$Paper)
JMartin$PaperID <- rownames(JMartin)

JMartin$QuestAuthor <- "J Martin"

x_list <- strsplit(JMartin$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "J Martin")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
JMartin$Coauthor <- coauthor_vec
# JMartin$Coauthor <- gsub(";*J Martin[[:blank:]]*;*", "", JMartin$Coauthor)
JMartin  <- JMartin[,c(4, 5, 7, 1, 2, 3)]
JMartin  <- arrange(JMartin , as.numeric(AuthorID), as.numeric(PaperNO))

# 171 lines with <i> k </i> issue
a <- readLines("../data/nameset/JRobinson.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
JRobinson <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
JRobinson$AuthorID <- sub("_.*","",JRobinson$Coauthor)
JRobinson$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JRobinson$Coauthor)
JRobinson$Coauthor <- gsub("<","",sub("^.*?\\s","", JRobinson$Coauthor))
JRobinson$Paper <- gsub("<","",JRobinson$Paper)
JRobinson$PaperID <- rownames(JRobinson)

JRobinson$QuestAuthor <- "J Robinson"

x_list <- strsplit(JRobinson$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "J Robinson")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
JRobinson$Coauthor <- coauthor_vec
# JRobinson$Coauthor <- gsub(";*J Robinson[[:blank:]]*;*", "", JRobinson$Coauthor)
JRobinson  <- JRobinson[,c(4, 5, 7, 1, 2, 3)]
JRobinson  <- arrange(JRobinson , as.numeric(AuthorID), as.numeric(PaperNO))

# 927 for JSmith
a <- readLines("../data/nameset/JSmith.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
JSmith <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
JSmith$AuthorID <- sub("_.*","",JSmith$Coauthor)
JSmith$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JSmith$Coauthor)
JSmith$Coauthor <- gsub("<","",sub("^.*?\\s","", JSmith$Coauthor))
JSmith$Paper <- gsub("<","",JSmith$Paper)
JSmith$PaperID <- rownames(JSmith)

JSmith$QuestAuthor <- "J Smith"

x_list <- strsplit(JSmith$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "J Smith")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
JSmith$Coauthor <- coauthor_vec
# JSmith$Coauthor <- gsub(";*J Smith[[:blank:]]*;*", "", JSmith$Coauthor)
JSmith  <- JSmith[,c(4, 5, 7, 1, 2, 3)]
JSmith  <- arrange(JSmith , as.numeric(AuthorID), as.numeric(PaperNO))

# 280 lines for KTanaka
a <- readLines("../data/nameset/KTanaka.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
KTanaka <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
KTanaka$AuthorID <- sub("_.*","",KTanaka$Coauthor)
KTanaka$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", KTanaka$Coauthor)
KTanaka$Coauthor <- gsub("<","",sub("^.*?\\s","", KTanaka$Coauthor))
KTanaka$Paper <- gsub("<","",KTanaka$Paper)
KTanaka$PaperID <- rownames(KTanaka)

KTanaka$QuestAuthor <- "K Tanaka"

x_list <- strsplit(KTanaka$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "K Tanaka")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
KTanaka$Coauthor <- coauthor_vec
# KTanaka$Coauthor <- gsub(";*K Tanaka[[:blank:]]*;*", "", KTanaka$Coauthor)
KTanaka  <- KTanaka[,c(4, 5, 7, 1, 2, 3)]
KTanaka  <- arrange(KTanaka , as.numeric(AuthorID), as.numeric(PaperNO))


# 153 lines for MBrown
a <- readLines("../data/nameset/MBrown.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
MBrown <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
MBrown$AuthorID <- sub("_.*","",MBrown$Coauthor)
MBrown$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MBrown$Coauthor)
MBrown$Coauthor <- gsub("<","",sub("^.*?\\s","", MBrown$Coauthor))
MBrown$Paper <- gsub("<","",MBrown$Paper)
MBrown$PaperID <- rownames(MBrown)

MBrown$QuestAuthor <- "M Brown"

x_list <- strsplit(MBrown$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "M Brown")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
MBrown$Coauthor <- coauthor_vec

# MBrown$Coauthor <- gsub(";*M Brown[[:blank:]]*;*", "", MBrown$Coauthor)
MBrown  <- MBrown[,c(4, 5, 7, 1, 2, 3)]
MBrown  <- arrange(MBrown , as.numeric(AuthorID), as.numeric(PaperNO))

#  260 lines for MJones
a <- readLines("../data/nameset/MJones.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
MJones <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
MJones$AuthorID <- sub("_.*","",MJones$Coauthor)
MJones$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MJones$Coauthor)
MJones$Coauthor <- gsub("<","",sub("^.*?\\s","", MJones$Coauthor))
MJones$Paper <- gsub("<","",MJones$Paper)
MJones$PaperID <- rownames(MJones)

MJones$QuestAuthor <- "M Jones"

x_list <- strsplit(MJones$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "M Jones")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
MJones$Coauthor <- coauthor_vec
# MJones$Coauthor <- gsub(";*M Jones[[:blank:]]*;*", "", MJones$Coauthor)
MJones  <- MJones[,c(4, 5, 7, 1, 2, 3)]
MJones  <- arrange(MJones , as.numeric(AuthorID), as.numeric(PaperNO))

# MMiller 412 lines
a <- readLines("../data/nameset/MMiller.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
MMiller <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
MMiller$AuthorID <- sub("_.*","",MMiller$Coauthor)
MMiller$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MMiller$Coauthor)
MMiller$Coauthor <- gsub("<","",sub("^.*?\\s","", MMiller$Coauthor))
MMiller$Paper <- gsub("<","",MMiller$Paper)
MMiller$PaperID <- rownames(MMiller)

MMiller$QuestAuthor <- "M Miller"

x_list <- strsplit(MMiller$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "M Miller")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
MMiller$Coauthor <- coauthor_vec
# MMiller$Coauthor <- gsub(";*M Miller[[:blank:]]*;*", "", MMiller$Coauthor)
MMiller  <- MMiller[,c(4, 5, 7, 1, 2, 3)]
MMiller  <- arrange(MMiller , as.numeric(AuthorID), as.numeric(PaperNO))

# SLee 1464

a <- readLines("../data/nameset/SLee.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
SLee <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
SLee$AuthorID <- sub("_.*","",SLee$Coauthor)
SLee$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", SLee$Coauthor)
SLee$Coauthor <- gsub("<","",sub("^.*?\\s","", SLee$Coauthor))
SLee$Paper <- gsub("<","",SLee$Paper)
SLee$PaperID <- rownames(SLee)

SLee$QuestAuthor <- "S Lee"

x_list <- strsplit(SLee$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "S Lee")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
SLee$Coauthor <- coauthor_vec
# SLee$Coauthor <- gsub(";*S Lee[[:blank:]]*;*", "", SLee$Coauthor)
SLee  <- SLee[,c(4, 5, 7, 1, 2, 3)]

SLee  <- arrange(SLee , as.numeric(AuthorID), as.numeric(PaperNO))

#YChen 1265
a <- readLines("../data/nameset/YChen.txt", encoding = "latin1")
a <- gsub("<[/|[:alpha:]]+>", "", a)
YChen <- data.frame(
  scan(
    text = a, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
YChen$AuthorID <- sub("_.*","",YChen$Coauthor)
YChen$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", YChen$Coauthor)
YChen$Coauthor <- gsub("<","",sub("^.*?\\s","", YChen$Coauthor))
YChen$Paper <- gsub("<","",YChen$Paper)
YChen$PaperID <- rownames(YChen)

YChen$QuestAuthor <- "Y Chen"

x_list <- strsplit(YChen$Coauthor, "; ")
coauthor_list <- sapply(x_list, conv_initial, name = "Y Chen")
coauthor_vec <- sapply(coauthor_list, paste, collapse = '; ')
YChen$Coauthor <- coauthor_vec
# YChen$Coauthor <- gsub(";*Y Chen[[:blank:]]*;*", "", YChen$Coauthor)
YChen  <- YChen[,c(4, 5, 7, 1, 2, 3)]
YChen  <- arrange(YChen , as.numeric(AuthorID), as.numeric(PaperNO))


write.csv(AGupta , file = "../output/Agupta.csv")
write.csv(AKumar , file = "../output/AKumar.csv")
write.csv(CChen , file = "../output/CChen.csv")
write.csv(DJohnson , file = "../output/DJohnson.csv")
write.csv(JLee , file = "../output/JLee.csv")
write.csv(JMartin , file = "../output/JMartin.csv")
write.csv(JRobinson , file = "../output/JRobinson.csv")
write.csv(JSmith , file = "../output/JSmith.csv")
write.csv(KTanaka , file = "../output/KTanaka.csv")
write.csv(MBrown , file = "../output/MBrown.csv")
write.csv(MJones , file = "../output/MJones.csv")
write.csv(MMiller , file = "../output/MMiller.csv")
write.csv(SLee , file = "../output/SLee.csv")
write.csv(YChen , file = "../output/YChen.csv")
