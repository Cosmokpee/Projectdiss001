#
##
###Solomon
####
##### Data Analysis part 7
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))

# pGTS is a vector of length 4

ConversionPP <- function(pGTS){

if(pGTS[1] == "1/1" && pGTS[2] == "0/0" && pGTS[3] == "0/0" && pGTS[4] == "0/0"){
  return(c("B?", "A?", "X"))
}

 if(pGTS[1] == "0/0" && pGTS[2] == "1/1" && pGTS[3] == "0/0" && pGTS[4] == "0/0"){
    return(c("A?", "B?", "X"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "0/0" && pGTS[3] == "1/1" && pGTS[4] == "0/0"){
  return(c("?D", "?C", "X"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "0/0" && pGTS[3] == "0/0" && pGTS[4] == "1/1"){
  return(c("?C", "?D", "X"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "1/1" && pGTS[3] == "0/0" && pGTS[4] == "0/0"){
  return(c("X", "??", "X"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "1/1" && pGTS[3] == "1/1" && pGTS[4] == "0/0"){
  return(c("AD", "??", "BC"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "0/0" && pGTS[3] == "1/1" && pGTS[4] == "1/1"){
  return(c("X", "??", "X"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "0/0" && pGTS[3] == "1/1" && pGTS[4] == "0/0"){
  return(c("BD", "??", "AC"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "1/1" && pGTS[3] == "0/0" && pGTS[4] == "1/1"){
    return(c("AC", "??", "BD"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "0/0" && pGTS[3] == "0/0" && pGTS[4] == "1/1"){
    return(c("BC", "BD", "AD"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "1/1" && pGTS[3] == "0/0" && pGTS[4] == "0/0"){
    return(c("A?", "B?", "X"))
}

if(pGTS[1] == "0/0" && pGTS[2] == "1/1" && pGTS[3] == "1/1" && pGTS[4] == "1/1"){
    return(c("X", "A?", "B?"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "0/0" && pGTS[3] == "1/1" && pGTS[4] == "1/1"){
    return(c("X", "B?", "A?"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "1/1" && pGTS[3] == "0/0" && pGTS[4] == "1/1"){
    return(c("X", "?C", "?D"))
}

if(pGTS[1] == "1/1" && pGTS[2] == "1/1" && pGTS[3] == "1/1" && pGTS[4] == "0/0"){
    return(c("X", "?D", "?C"))
}

}
 
ConversionPP(parents[1, 6:9])
rr <- c()
for(x in 1:nrow(parents)){
    #Every time for each element
    r <- ConversionPP(parents[x, 6:9])
  rr <- rbind(rr, r)
}

parents <- cbind(parents, rr)	
parents <- cbind(parents, "NA" = "")

colnames(parents)[10:12] <- c("0/0", "0/1", "1/1")

genotype[1, ]

m <- rownames(genotype)[1]
gt <- as.character(genotype[1, ])
gt[is.na(gt)] <- "NA"

parents[m, gt]