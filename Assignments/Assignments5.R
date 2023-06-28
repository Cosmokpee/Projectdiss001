# Answers to the assignments lecture 5
##
### Danny arends
####
###

#1
setwd("C:/Github/Projectdiss001/Assignments/Assignment5-Data/")

genotypes <- read.table("Assignment 5 - Data/genotypes.txt", check.names=FALSE)
phenotypes <- read.table("Assignment 5 - Data/phenotypes.txt")
map <- read.table("Assignment 5 - Data/map.txt")

#2a
days <- paste0("d", seq(21,70,7))
onlyweights <- phenotypes[,days]
plot(apply(onlyweights, 2, mean), t='l', ylab = "Weight")

#2b
means <- apply(onlyweights, 2, mean)
sds <- apply(onlyweights, 2, sd)

plot(x = c(1,length(means)), y = c(0, max(means+sds)), ylab = "Weight", t = 'n')
points(means, t = 'l'); points(means + sds, t = 'l'); points(means - sds, t = 'l')


#2c
medians <- apply(onlyweights, 2, median)

plot(x = c(1,length(medians)), y = c(0, max(onlyweights)), ylab = "Weight", t = 'n')
for(x in 1:nrow(onlyweights)){
  points(as.numeric(onlyweights[x,]), t = 'l', col='gray')
}
points(medians, t='l', lwd=2)


#3a
plotAs2C <- function(weightsubset){
  medians <- apply(weightsubset, 2, median)

  plot(x = c(1,length(medians)), y = c(0, max(weightsubset)), ylab = "Weight", t = 'n')
  for(x in 1:nrow(weightsubset)){
    points(as.numeric(weightsubset[x,]), t = 'l', col='gray')
  }
  points(medians, t='l', lwd=2)
}

wgS <- onlyweights[phenotypes[,"WG2"] < 10,]
wgL <- onlyweights[phenotypes[,"WG2"] >= 10,]

par(mfrow = c(1,2))
plotAs2C(wgS) ; plotAs2C(wgL)

#3b
par(mfrow = c(2,4))
for(day in days){
  boxplot(wgS[,day],wgL[,day], main=day, notch=TRUE)
  axis(1, at=c(1,2), labels=c("wgS", "wgL"))
}

#4
ordering <- sort(map[,"Mb_NCBI38"],index=TRUE)
map <- map[ordering$ix,]

for(chr in unique(map[,"Chr"])){
  onChr <- rownames(map[which(map[,"Chr"] == chr),])
  numGonChr <- apply(genotypes[onChr,], 2, function(x){as.numeric(as.factor(x))})
  #png()
  image(as.matrix(numGonChr))
  #dev.off()
}

#5
chromosomes <- unique(map[,"Chr"])                                                              
# Chromosomes 
chrInfo <- matrix(NA, length(unique(map[,"Chr"])), 1, dimnames=list(chromosomes,c("Length")))   
# Chromosome info structure 
for(x in chromosomes){
  chrInfo[x, "Length"] <- max(map[which(map[,"Chr"] == x), "Mb_NCBI38"])                        
  #length of each chromosome
}
mlength <- max(chrInfo[, "Length"])                                                             
#Maximum chromosome length

# Chromosome plot 
plot(y = c(0, mlength), x = c(1, nrow(chrInfo)), t='n', yaxt="n", xlab="", ylab="", xaxt="n")   
#plot window
abline(h=seq(0, mlength, 1e7), col = "blue", lty = "dotted")                                    
#10 mb lines

cnt <- 1
aa <- apply(chrInfo,1,function(x){
  lines(c(cnt, cnt), c(0,x["Length"]), type="l", col="black", lty=1, lwd=2)                     
#Draw each chromosome
  cnt <<- cnt + 1
})

aa <- apply(map, 1, function(x){
  xloc <- match(x["Chr"], chromosomes)
  yloc <- x["Mb_NCBI38"]
  points(x=xloc, y=yloc, pch="-", col="blue", cex=1)                                            
  #Draw each marker
})

axis(1,chromosomes, at=c(1:nrow(chrInfo)), las=1, cex.axis=1.5)
axis(2, seq(0, mlength, 10000000)/1000000, at=seq(0, mlength, 10000000), cex.axis=1.2, las=1)



myobject <- list(chr = c('1' = 15, '2' = 10),
                 genotypes = matrix(round(runif(250)), 10, 25))
                 
class(myobject) <- c("list", "myclass")

print.myclass <- function(x, ...){
  cat("myclass object\n ")
  cat("Content:", sum(x$chr), "markers\n")
  cat("Content:", nrow(x$genotypes), "individuals\n")
}


myobject

plot.myclass <- function(x, ...){
  image(x$genotypes, ...)
  box()
}

plot(myobject)

par(cex=1.5, las=2)

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NY", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue",pch=19))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red",pch=19))
legend("topright", col = c("blue", "red"), legend = c("May", "Other Months"),pch=19)
arrows(3.4,140,3.4,165,col='red')

model <- with(subset(airquality, Month == 5), lm(Ozone ~ Wind))
abline(model[[1]][1],model[[1]][2], col="red",lwd=2)

model <- with(subset(airquality, Month != 5), lm(Ozone ~ Wind))
abline(model[[1]][1],model[[1]][2], col="blue",lwd=2)

slices <- c(10, 12, 4, 16, 8)
lbls   <- c("US", "UK", "Australia", "Germany", "France")
pct    <- round(slices/sum(slices)*100)
lbls   <- paste(lbls, pct) 		# Add percents to labels
lbls   <- paste(lbls,"%",sep="") 		# Add % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
   main="Pie Chart of Countries")


plot(y = c(0, mlength), x = c(1, nrow(chrInfo)), 
     t='n', yaxt="n", xlab="", ylab="", xaxt="n")
abline(h=seq(0, mlength, 1e7), col = "blue", lty = "dotted")

cnt <- 1
aa <- apply(chrInfo,1,function(x){
  lines(c(cnt, cnt), c(0,x["Length"]), type="l", col="black", lty=1, lwd=2)
  cnt <<- cnt + 1
})

aa <- apply(alldata, 1, function(x){
  xloc <- match(x["chromosome_name"], chromosomes)
  yloc <- x["start_position"]
  col <- as.numeric(x["Ratio_F1"] >= 1) + 1
  points(x=xloc, y=yloc, pch="-", col=col, cex=3)
})

axis(1,chrInfo[,1], at=c(1:nrow(chrInfo)), las=1, cex.axis=1.5)
axis(2, seq(0, mlength, 10000000)/1000000, at=seq(0, mlength, 10000000), cex.axis=1.2, las=1)
legend("topright", c("Up", "Down"), fill=c(1, 2), cex=1.2)