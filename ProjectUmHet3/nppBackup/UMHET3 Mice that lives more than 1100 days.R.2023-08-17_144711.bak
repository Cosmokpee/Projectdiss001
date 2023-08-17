#
##
###
####
##### Code for Dissertation (July 4th)
######


setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes = read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
phenotypes[1:10,1:10]

iix <- which(phenotypes[, "Longevity_HET3_ITP"] > 1100)
phenotypes <- phenotypes[iix, ]

Y <- phenotypes[, "Longevity_HET3_ITP"]
sex <- phenotypes[, "Sex"]
site <-phenotypes[, "Site"]
cohort <- as.factor(phenotypes[, "Cohort.Year"])
treatment <- as.factor(phenotypes[, "Treatment_Effect"])

lm(Y ~ sex) ..... #null model

lm(Y ~ sex + 0)
m1 <- lm(Y ~ sex + 0)
m2 <- lm(Y ~ sex + site + 0)
AIC(m1,m2)
 # model 1 is better because it ha s a lower AIC than model 2
 
m2 <- lm(Y ~ sex + site + cohort + 0)
AIC(m1,m2)

# model 1 is still a better option

m2 <- lm(Y ~ sex + site + cohort + treatment + 0)
AIC(m1,m2)

#model 1  is still a beter options

forpie <- anova(lm(Y ~ sex))
pie(forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"]))

round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
pch <- round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
names(pch) <- c("Sex", "Unknown")
pie(pch)

m2 <- lm(Y ~ sex + site + cohort + treatment + sex:site + 0)
AIC(m1,m2)
 #model 1 is a better option
 
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
gtsPM <- read.table("gtsPM.txt", sep = "\t", check.names=FALSE)


# fix for the map not matching to the genotypes after merging markers and imputation
MingtsPM <- unique(unlist(lapply(strsplit(colnames(gtsPM), ":"), "[", 1)))
map <- map[MingtsPM, ]

# Pull out the phenotypes
Y <- phenotypes[, "Longevity_HET3_ITP"]
sex <- phenotypes[, "Sex"]
site <- phenotypes[, "Site"]
cohort <- as.factor(phenotypes[, "Cohort.Year"])
treatment <- as.factor(phenotypes[, "Treatment_Effect"])

# Check the ordering between phenotypes and gtsPM
all(rownames(phenotypes) == rownames(gtsPM))

mnull <- lm(Y ~ sex + 0)

pvals <- c()
for(marker in MingtsPM){
  iix <- grep(marker, colnames(gtsPM))
  gts <- as.matrix(gtsPM[, iix])
  mfull <- lm(Y ~ sex + gts + 0)
  pM <- as.numeric(na.omit(anova(mnull,mfull)[, "Pr(>F)"]))
  pvals <- c(pvals, pM)
}

threshold5 <- -log10(0.05 / length(MingtsPM))
threshold1 <- -log10(0.01 / length(MingtsPM))
threshold01 <- -log10(0.001 / length(MingtsPM))

map[map[, "Chr"] == "X", "Chr"] <- 20
chrI <- 2 - as.numeric(map[, "Chr"]) %% 2

chr.length <- c()
for(x in 1:20){
  iim <- which(map[, "Chr"] == x)
  chr.length <- c(chr.length, max(map[iim, "Position"]))
}

map <- cbind(map, cumPos = NA)
pos <- 0
chr.mids <- c()
for(x in 1:20){
  iim <- which(map[, "Chr"] == x)
  map[iim, "cumPos"] <- pos + map[iim, "Position"]
  chr.mids <- c(chr.mids, pos + .5 * chr.length[x])
  pos <- pos + chr.length[x] + 30000000
}

plot(x = map[, "cumPos"], y = -log10(pvals), col = c("black", "orange")[chrI], pch = 19, xaxt = "n", xlab = "Chromosome", las = 2, main = "QTL mapping on longevity in UM-HET3 Mice that live for more than 1100 days")
i <- 1
for(x in 1:20){
  iim <- which(map[, "Chr"] == x)
  points(x = map[iim, "cumPos"], y = -log10(pvals[iim]), t = 'l', col = c("black", "orange")[i])
  i <- i + 1
  if(i > 2) i <- 1
}
abline(h = threshold5, col = "red", lty = 2)
abline(h = threshold1, col = "orange", lty = 2)
abline(h = threshold01, col = "green", lty = 2)
axis(1, at = chr.mids, c(1:19, "X"))
legend("topleft", c("0.1%", "1%", "5%"), lty = 2, col =c("green", "orange", "red"), title = "Bonferonni Threshold")
