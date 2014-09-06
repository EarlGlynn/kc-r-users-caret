# Download Human Activity Recognition Using Smartphones Dataset
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# efg, 2014-09-04

setwd("C:/Users/efg/Desktop/UCI/")  ##### Modify as appropriate
sink("0-Download-UCI-Dataset.txt", split=TRUE)

print(Sys.time())

library(downloader)  # platform neutral download function
library(tools)       # md5sum

URL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"

zip.filename <- "UCI-HAR-Dataset.zip"
download(URL, zip.filename, mode = "wb")

print( md5sum(zip.filename) )
unzip(zip.filename, exdir=".")

# Rename directory
file.rename("UCI HAR Dataset", "Samsung-Human-Activity")
print(Sys.time())

sink()

