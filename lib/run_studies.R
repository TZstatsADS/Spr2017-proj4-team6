# required libraries
library(tidyr)
library(tidytext)
library(kernlab)
library(dplyr)
library(readr)
library(tm)
library(tibble)
library(stringr)
library(qlcMatrix)

# set project directory *** note we are using ec2 instance
projDir <- file.path("/","home","ubuntu","entity-resolution", fsep = .Platform$file.sep)

# source necessary libraries
source(file.path(projDir,"lib","SpectralClustering.R"))
source(file.path(projDir,"lib","feature_extraction.R"))

# list date sources
files <- list.files(file.path(projDir,"output"), pattern = "*.csv")
filePaths <- file.path(projDir,"output",files)
n <- length(files)

# run spectral clustering with QR decomposition
tb_qr <- tibble(
  study = rep("author_name",n),
  method = rep("study_name",n),
  precision = rep(0,n),
  recall = rep(0,n),
  f1 = rep(0,n),
  accuracy = rep(0,n),
  mcc = rep(0,n)
)

for(i in 1:n){
  x <- runStudy(filePaths[i], "runSpectralClusteringQR")
  tb_qr[i,] <- x
}

save(tb_qr, file.path(projDir,"output","processed_data","resultsQR.RData"))

# run spectral clustering KMeans
n <- length(files)
tb_km <- tibble(
  study = rep("author_name",n),
  method = rep("study_name",n),
  precision = rep(0,n),
  recall = rep(0,n),
  f1 = rep(0,n),
  accuracy = rep(0,n),
  mcc = rep(0,n)
)

for(i in 1:n){
  x <- runStudy(filePaths[i], "runSpectralClusteringQR")
  tb_km[i,] <- x
}

save(tb_km, file.path(projDir,"output","processed_data","resultsKM.RData"))