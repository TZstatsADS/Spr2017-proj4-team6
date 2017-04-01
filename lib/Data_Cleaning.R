

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
# This need to be modified for different name set

# extract canonical author id befor "_"
AKumar$AuthorID <- sub("_.*","",AKumar$Coauthor)
# extract paper number under same author between "_" and first whitespace
AKumar$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor)
# delete "<" in AKumar$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
AKumar$Coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
# delete "<" in AKumar$Paper
AKumar$Paper <- gsub("<","",AKumar$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and 
# then assign the unique ID for all the citations
AKumar$PaperID <- rownames(AKumar)


# For AGupta (577 lines), there're several issues, "EOF within quoted string"
# "number of items read is not a multiple of the number of columns" 
# 4_48 for example:  
# "4_48 A Gupta;Damon Kaller ; Thomas C Shermer<>Linear-Time Algorithms for Partial <i>k</i>-Tree Complements <>Algorithmica"
#6_44 A Gupta;Alistair Sinclair ; Ilan Newman ; Yuri Rabinovich<>Cuts, Trees and l<sub> </sub>-Embeddings of Graphs <>FOCS IEEE Symposium Foundations of Computer Science
a <- readLines("../data/nameset/AGupta.txt", encoding = "latin1")
#a <- gsub("<i>k</i>", "k", a, perl=TRUE)
a <- gsub("<[/|[:alpha:]]+>", "", a)
# a
# a <- c("4_48 A Gupta;Damon Kaller ; Thomas C Shermer<>Linear-Time Algorithms for Partial <i>k</i>-Tree Complements <>Algorithmica")
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





# CChen 801 lines. By looking at dataset, 38_0 should be deleted, 39_1 has different format than others.
CChen <- data.frame(
  scan(
    file.path("..", "data", "nameset", "CChen.txt", fsep = .Platform$file.sep),
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quiet=TRUE,
    quote = NULL),
  stringsAsFactors=FALSE)
CChen <- CChen[-600, ]
CChen[602, ] <- c("39_1 C Chen;", "The Complexity of Propositional Modal Theories and the Complexity of Consistency of Propositional Modal Theories <"," LFCS" )
CChen$AuthorID <- sub("_.*","",CChen$Coauthor)
CChen$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", CChen$Coauthor)
CChen$Coauthor <- gsub("<","",sub("^.*?\\s","", CChen$Coauthor))
CChen$Paper <- gsub("<","",CChen$Paper)
CChen$PaperID <- rownames(CChen)

# 368 for DJohnson
d <- readLines("../data/nameset/DJohnson.txt", encoding = "latin1")
DJohnson <- data.frame(
  scan(
    text = d, 
    what = list(Coauthor = "", Paper = "", Journal = ""),
    sep=">",
    quote = NULL, 
    quiet=TRUE),
  stringsAsFactors=FALSE)
DJohnson$AuthorID <- sub("_.*","",DJohnson$Coauthor)
# extract paper number under same author between "_" and first whitespace
DJohnson$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", DJohnson$Coauthor)
# delete "<" in DJohnson$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
DJohnson$Coauthor <- gsub("<","",sub("^.*?\\s","", DJohnson$Coauthor))
# delete "<" in DJohnson$Paper
DJohnson$Paper <- gsub("<","",DJohnson$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and 
# then assign the unique ID for all the citations
DJohnson$PaperID <- rownames(DJohnson)


# 1419 lines for JLee
j <- readLines("../data/nameset/JLee.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
JLee <- data.frame(
  scan(
    text = j,
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

# 171 lines with <i> k </i> issue
j <- readLines("../data/nameset/JRobinson.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
JRobinson <- data.frame(
  scan(
    text = j, 
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

# 927 for JSmith
j <- readLines("../data/nameset/JSmith.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
JSmith <- data.frame(
  scan(
    text = j, 
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

# 280 lines for KTanaka
j <- readLines("../data/nameset/KTanaka.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
KTanaka <- data.frame(
  scan(
    text = j, 
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


# 153 lines for MBrown
j <- readLines("../data/nameset/MBrown.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
MBrown <- data.frame(
  scan(
    text = j, 
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


#  260 lines for MJones
j <- readLines("../data/nameset/MJones.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
MJones <- data.frame(
  scan(
    text = j, 
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


# MMiller 412 lines
j <- readLines("../data/nameset/MMiller.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
MMiller <- data.frame(
  scan(
    text = j, 
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


# SLee 1464

j <- readLines("../data/nameset/SLee.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
SLee <- data.frame(
  scan(
    text = j, 
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

#YChen 1265
j <- readLines("../data/nameset/YChen.txt", encoding = "latin1")
j <- gsub("<[/|[:alpha:]]+>", "", j)
YChen <- data.frame(
  scan(
    text = j, 
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
