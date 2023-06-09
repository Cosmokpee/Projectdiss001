#
# Code for Dissertation (June 12th)
# 
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")
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
