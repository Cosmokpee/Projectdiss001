#
## Answers to the assignments to lecture 6
###
#### Danny arends R lecture 6
#####

library(preprocessCore)
setwd("C:/Github/Projectdiss001/Assignments/Assignment 6 - Data/")

normalize <- function(x){ (x - mean(x)) / sd(x) }
apply(arraydata[, arrays[,"AtlasID"]], 2, normalize)

#1
arraydata <- read.table("Assignment 6 - Data/arraydata.txt", check.names=FALSE)
arrays <- read.table("Assignment 6 - Data/arrays.txt", header=TRUE, colClasses=c("character"))

boxplot(arraydata[, arrays[,"AtlasID"]])
arraydata[, arrays[,"AtlasID"]] <- apply(arraydata[, arrays[,"AtlasID"]], 2, log2)
boxplot(arraydata[, arrays[,"AtlasID"]])
arraydata[, arrays[,"AtlasID"]] <- normalize.quantiles(as.matrix(arraydata[, arrays[,"AtlasID"]]))
boxplot(arraydata[, arrays[,"AtlasID"]])


#2a
par(mfrow=c(4,4))
for(x in 1:length(arrays[,"AtlasID"])){
  sFrom <- arrays[x,"AtlasID"]
  correlations <- as.numeric(cor(arraydata[, sFrom], arraydata[,arrays[-x,"AtlasID"]]))
  plot(correlations, ylab="Correlation", main = sFrom, xlab="",  xaxt="n")
  axis(1, arrays[-x,"AtlasID"], at = 1:length(arrays[-x,"AtlasID"]), las=2)
}

i <- 1
apply(arraydata[, arrays[,"AtlasID"]],2, function(x){
  correlations <- as.numeric(cor(x, arraydata[, arrays[-i,"AtlasID"]]))
  plot(correlations, ylab="Correlation", main = arrays[i,"AtlasID"], xlab="",  xaxt="n")
  axis(1, arrays[-i,"AtlasID"], at = 1:length(arrays[-i,"AtlasID"]), las=2)
  i <<- i + 1
})


#2b
heatmap(cor(arraydata[,arrays[,"AtlasID"]]))

#3a
HTsamples <- arrays[arrays[,"Tissue"] == "HT", "AtlasID"]
HT <- arraydata[,HTsamples]

GFsamples <- arrays[arrays[,"Tissue"] == "GF", "AtlasID"]
GF <- arraydata[,GFsamples]

#3b
results <- NULL
for(x in rownames(arraydata)){
  ht <- as.numeric(HT[x,])
  gf <- as.numeric(GF[x,])
  values <- c(mean(ht),mean(gf),sd(ht),sd(gf), t.test(ht,gf)$p.value)
  results <- rbind(results, values)
}
rownames(results) <- rownames(arraydata)
colnames(results) <- c("meanHT","meanGF","sdHT","sdGF","Ttest")

#3c
TFvector <- results[,"Ttest"] < (0.05 / nrow(results))
sum(TFvector)
length(which(TFvector))

#3d
length(which(p.adjust(results[,"Ttest"], "BH") < 0.05))