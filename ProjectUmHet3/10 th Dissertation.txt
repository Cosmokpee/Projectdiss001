#
##
###Solomon - Dissertation Session 9
####
##### QTL mapping

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

mnull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + 0)

# empty vector
adjusted <- rep(NA, length (Y))
# value after adjustment
adj <- residuals(mnull)
#values adjusted + mean longevity
adjusted[as.numeric(names(adj))] <- adj + mean(Y)


pvals <- c()
for(marker in MingtsPM){
  iix <- grep(marker, colnames(gtsPM))
  gts <- as.matrix(gtsPM[, iix])
  mfull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + gts + 0)
  pM <- as.numeric(na.omit(anova(mnull,mfull)[, "Pr(>F)"]))
  pvals <- c(pvals, pM)
}