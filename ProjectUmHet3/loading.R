#
# Code for Dissertation (June 12th)
# 
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

#Loading in the different files
allvcf <- read.table("all.vcf.sorted.txt")
allvcf[1:5, ]
fvcfall <- read.table("fvcfAll.txt")
fvcfall[1:5, ]
phesorted <- read.table("ind.sorted.phe.txt")
phesorted[1:5, ]
mapsorted = read.table("map.sorted.txt")
mapsorted[1:5, ]
mapsorted[ , "Chr"]
table(mapsorted[ , "Chr"])

#setup for ploting the markers across the chromomosomes
nchr <- length(table(mapsorted[ , "Chr"]))

 mapsorted[1:10,]
 maxchr = max(mapsorted[ , "Position"])
 
 onX <- which(mapsorted[, "Chr"] == "X")
 
 plot(x= c (0, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)")
 
 for(x in 1:nrow(mapsorted)){
 #Every time for each element
 }
 
 
 for(x in 1:nrow(mapsorted)){
    #Every time for each element
    chr <-  mapsorted[x, "Chr"]
	if(x %in% onX) chr <- 20
    points(x = chr, y = mapsorted[x, "Position"], pch = "-")
 }