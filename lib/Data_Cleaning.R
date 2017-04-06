

# filename <- c("AGupta.txt", "AKumar.txt", "CChen.txt", "DJohnson.txt", "JLee.txt", "JMartin.txt", 
#               "JRobinson.txt", "JSmith.txt", "KTanaka.txt", "MBrown.txt", "MJones.txt", 
#               "MMiller.txt", "SLee.txt", "YChen.txt")



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
AKumar$Coauthor <- gsub(";*A Kumar[[:blank:]]*;*", "", AKumar$Coauthor)
AKumar_new <- AKumar[,c(4, 5, 7, 1, 2, 3)]
AKumar_new <- arrange(AKumar_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
AGupta$Coauthor <- gsub(";*A Gupta[[:blank:]]*;*", "", AGupta$Coauthor)
AGupta_new <- AGupta[,c(4, 5, 7, 1, 2, 3)]
AGupta_new <- arrange(AGupta_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
CChen$Coauthor <- gsub(";*C Chen[[:blank:]]*;*", "", CChen$Coauthor)
CChen_new <- CChen[,c(4, 5, 7, 1, 2, 3)]
CChen_new <- arrange(CChen_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
DJohnson$Coauthor <- gsub(";*D Johnson[[:blank:]]*;*", "", DJohnson$Coauthor)
DJohnson_new <- DJohnson[,c(4, 5, 7, 1, 2, 3)]
DJohnson_new <- arrange(DJohnson_new, as.numeric(AuthorID), as.numeric(PaperNO))


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
JLee$Paper <- gsub("<","",JLee$Paper)
JLee$PaperID <- rownames(JLee)

JLee$QuestAuthor <- "J Lee"
JLee$Coauthor <- gsub(";*J Lee[[:blank:]]*;*", "", JLee$Coauthor)
JLee_new <- JLee[,c(4, 5, 7, 1, 2, 3)]
JLee_new <- arrange(JLee_new, as.numeric(AuthorID), as.numeric(PaperNO))

# 122 lines for JMartin
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
JMartin$Coauthor <- gsub(";*J Martin[[:blank:]]*;*", "", JMartin$Coauthor)
JMartin_new <- JMartin[,c(4, 5, 7, 1, 2, 3)]
JMartin_new <- arrange(JMartin_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
JRobinson$Coauthor <- gsub(";*J Robinson[[:blank:]]*;*", "", JRobinson$Coauthor)
JRobinson_new <- JRobinson[,c(4, 5, 7, 1, 2, 3)]
JRobinson_new <- arrange(JRobinson_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
JSmith$Coauthor <- gsub(";*J Smith[[:blank:]]*;*", "", JSmith$Coauthor)
JSmith_new <- JSmith[,c(4, 5, 7, 1, 2, 3)]
JSmith_new <- arrange(JSmith_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
KTanaka$Coauthor <- gsub(";*K Tanaka[[:blank:]]*;*", "", KTanaka$Coauthor)
KTanaka_new <- KTanaka[,c(4, 5, 7, 1, 2, 3)]
KTanaka_new <- arrange(KTanaka_new, as.numeric(AuthorID), as.numeric(PaperNO))
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
MBrown$Coauthor <- gsub(";*M Brown[[:blank:]]*;*", "", MBrown$Coauthor)
MBrown_new <- MBrown[,c(4, 5, 7, 1, 2, 3)]
MBrown_new <- arrange(MBrown_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
MJones$Coauthor <- gsub(";*M Jones[[:blank:]]*;*", "", MJones$Coauthor)
MJones_new <- MJones[,c(4, 5, 7, 1, 2, 3)]
MJones_new <- arrange(MJones_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
MMiller$Coauthor <- gsub(";*M Miller[[:blank:]]*;*", "", MMiller$Coauthor)
MMiller_new <- MMiller[,c(4, 5, 7, 1, 2, 3)]
MMiller_new <- arrange(MMiller_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
SLee$Coauthor <- gsub(";*S Lee[[:blank:]]*;*", "", SLee$Coauthor)
SLee_new <- SLee[,c(4, 5, 7, 1, 2, 3)]

SLee_new <- arrange(SLee_new, as.numeric(AuthorID), as.numeric(PaperNO))

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
YChen$Coauthor <- gsub(";*Y Chen[[:blank:]]*;*", "", YChen$Coauthor)
YChen_new <- YChen[,c(4, 5, 7, 1, 2, 3)]
YChen_new <- arrange(YChen_new, as.numeric(AuthorID), as.numeric(PaperNO))


write.csv(AGupta_new, file = "../output/Agupta.csv")
write.csv(AKumar_new, file = "../output/AKumar.csv")
write.csv(CChen_new, file = "../output/CChen.csv")
write.csv(DJohnson_new, file = "../output/DJohnson.csv")
write.csv(JLee_new, file = "../output/JLee.csv")
write.csv(JMartin_new, file = "../output/JMartin.csv")
write.csv(JRobinson_new, file = "../output/JRobinson.csv")
write.csv(JSmith_new, file = "../output/JSmith.csv")
write.csv(KTanaka_new, file = "../output/KTanaka.csv")
write.csv(MBrown_new, file = "../output/MBrown.csv")
write.csv(MJones_new, file = "../output/MJones.csv")
write.csv(MMiller_new, file = "../output/MMiller.csv")
write.csv(SLee_new, file = "../output/SLee.csv")
write.csv(YChen_new, file = "../output/YChen.csv")
