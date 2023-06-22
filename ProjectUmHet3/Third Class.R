#
##
# Third Class in Programming
###
##### Danny Arends 22-06-23
######
########


install.packages("emoji")
library(emoji)
plot(c(-1,1), c(-1,1), t = 'n', xaxt="n" , yaxt="n" ,xlab="", ylab="")

px <- 1
py <- 5
while (TRUE) {
	rect(-5, -5, 20, 20, col = "white")
	points(sin(px), cos(py), pch = emoji("smirk_cat"), cex = 12)
	#points (cos(px), 20+sin(py), pch = emoji("sun"), cex=12, col = "yellow")
	px <- px + 0.2
	py <- py + 0.2
	Sys.sleep(0.2)
}



plot(c(-3,3), c(-3,25), t = 'n', xlab="", ylab="")

px <- 1
py <- 5
i <- 1
emo <- "sun_with_face"
col <- "yellow"
while (TRUE) {
	rect(-5, -5, 40, 40, col = "white")
	if(i > 5) {
		if (emo == "sun_with_face") {
			emo <- "full_moon_with_face"
			col = "gray"
		} else {
				emo <- "sun_with_face"
				col = "yellow"
		}
		i <- 1
	}
	points(sin(px), cos(py), pch = emoji("smirk_cat"), cex = 8)
	points (cos(px), 20+sin(py), pch = emoji("sun_with_face"), cex=12, col = col)
	px <- px + 0.2
	py <- py + 0.2
	Sys.sleep(0.2)
	i <- i + 1
}




plot(c(-3,3), c(-3,40), t = 'n', xlab="", ylab="")

px <- 1
py <- 5
i <- 1
emo <- "sun_with_face"
col <- "yellow"
while (TRUE) {
	rect(-5, -5, 40, 40, col = "white")
	if(i > 5) {
		if (emo == "sun_with_face") {
			emo <- "full_moon_with_face"
			col = "gray"
		} else {
				emo <- "sun_with_face"
				col = "yellow"
		}
		i <- 1
	}
	points(sin(px), cos(py), pch = emoji("smirk_cat"), cex = 8)
	points (cos(px), 20+sin(py), pch = emoji(emo), cex=12, col = col)
	px <- px + 0.2
	py <- py + 0.2
	Sys.sleep(0.2)
	i <- i + 1
}


setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes = read.table ("ind.sorted.phe.txt")

Male <- which(phenotypes[, "Sex"] == "M")
Female <- which(phenotypes[, "Sex"] == "F")

length(Male)
length(Female)

phenotypes[1:5, ]
table(phenotypes[, "Cohort.Year"])
table(phenotypes[, "Site"])

table(phenotypes[which(phenotypes[, "Site"] == "UM"), "Cohort.Year"])
table(phenotypes[which(phenotypes[, "Site"] == "UT"), "Cohort.Year"])
table(phenotypes[which(phenotypes[, "Site"] == "JL"), "Cohort.Year"])

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
 
plot(x= c (0, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n")
 
for(x in 1:nrow(mapsorted)){
 #Every time for each element
 }
 
 
 for(x in 1:nrow(mapsorted)){
    #Every time for each element
    chr <-  mapsorted[x, "Chr"]
	if(x %in% onX) chr <- 20
    points(x = chr, y = mapsorted[x, "Position"], pch = "-")
 }
 
plot(x= c (0, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n")
axis(1, at = 1:30, c(1:19, "X"))

plot(x= c (0, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n")
axis(1, at = 1:20, c(1:19, "X"))

plot(x= c (1, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n")
axis(1, at = 1:20, c(1:19, "X"))

plot(x= c (1, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n", yaxt="n")
axis(1, at = 1:20, c(1:19, "X"))
axis(2, at = seq(0, 2e8, 2.5e7), seq(0, 2e8, 2.5e7) / 1e6, las = 2)

plot(x= c (1, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n", yaxt="n", main = "Markers on Chromosomes")
axis(1, at = 1:20, c(1:19, "X"))
axis(2, at = seq(0, 2e8, 2.5e7), seq(0, 2e8, 2.5e7) / 1e6, las = 2)

plot(x= c (1, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (bp)", xaxt="n", yaxt="n", main = "Markers on Chromosomes")
axis(1, at = 1:20, c(1:19, "X"))
axis(2, at = seq(0, 2e8, 2.5e7), seq(0, 2e8, 2.5e7) / 1e6, las = 2)

plot(x= c (1, nchr), y = c(0, maxchr) , t = "n", xlab = "Chromosomes", ylab = "Position (Mb)", xaxt="n", yaxt="n", main = "Markers on Chromosomes")
axis(1, at = 1:20, c(1:19, "X"))
axis(2, at = seq(0, 2e8, 2.5e7), seq(0, 2e8, 2.5e7) / 1e6, las = 2)
 for(x in 1:nrow(mapsorted)){
    #Every time for each element
    chr <-  mapsorted[x, "Chr"]
	if(x %in% onX) chr <- 20
    points(x = chr, y = mapsorted[x, "Position"], pch = "-", cex = 2)
 }

nrow(mapsorted)
table(mapsorted[, "Chr"])

boxplot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Site"])
iium <- which(phenotypes[,"Site"] == "UM")
iiut <- which(phenotypes[,"Site"] == "UT")
iijl <- which(phenotypes[,"Site"] == "JL")

hist(phenotypes[iium, "Longevity_HET3_ITP"])
hist(phenotypes[iiut, "Longevity_HET3_ITP"])
hist(phenotypes[iijl, "Longevity_HET3_ITP"])

iiumm <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "M")
iiumf <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "F")
iiutm <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "M")
iiutf <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "F")
iijlm <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "M")
iijlf <- which(phenotypes[,"Site"] == "UM", [, "Sex"] == "F")

iiumm <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
iiumf <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")
iiutm <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
iiutf <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")
iijlm <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
iijlf <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")

hist(phenotypes[iiumm, "Longevity_HET3_ITP"], col = "blue") 
hist(phenotypes[iiumf, "Longevity_HET3_ITP"], add = TRUE, col = "pink") 


hist(phenotypes[iiumf, "Longevity_HET3_ITP"], col = "pink") 
hist(phenotypes[iiumm, "Longevity_HET3_ITP"], add = TRUE, col = rgb(0,0,255,125, max = 255)) 

col=

hist(phenotypes[iiumf, "Longevity_HET3_ITP"], col = "pink", main = "Longevity at UM") 
hist(phenotypes[iiumm, "Longevity_HET3_ITP"], add = TRUE, col = rgb(0,0,255,125, max = 255)) 

hist(phenotypes[iiutf, "Longevity_HET3_ITP"], col = "pink", main = "Longevity at UT") 
hist(phenotypes[iiutm, "Longevity_HET3_ITP"], add = TRUE, col = rgb(0,0,255,125, max = 255)) 


hist(phenotypes[iijlf, "Longevity_HET3_ITP"], col = "pink", main = "Longevity at JL") 
hist(phenotypes[iijlm, "Longevity_HET3_ITP"], add = TRUE, col = rgb(0,0,255,125, max = 255)) 

plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])
plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])

boxplot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"], col = c("limegreen", "blue", "orange", "hotpink", "yellow", "gray", "green"))