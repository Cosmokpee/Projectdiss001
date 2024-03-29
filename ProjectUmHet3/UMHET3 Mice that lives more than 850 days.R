Code for Dissertation 
######

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes = read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
phenotypes[1:10,1:10]

iix <- which(phenotypes[, "Longevity_HET3_ITP"] > 850)
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

# model 2 is the best option because there is over -40 difference 

m3 <- lm(Y ~ sex + site + cohort + 0)
AIC(m2,m3)

#model 2 is better because there is a -5 difference, we need moe than -10 difference

m3 <- lm(Y ~ sex + site + cohort + treatment + 0)
AIC(m2,m3)

# model 3 is better because there is a -19 difference
model 

forpie <- anova(lm(Y ~ sex + site + cohort + treatment ))
pie(forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"]))

round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
pch <- round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
names(pch) <- c("Sex", "Site", "Cohort", "Treatment", "Unknown")
pie(pch)

m4 <- lm(Y ~ sex + site + cohort + treatment + sex:site + 0)
AIC(m3,m4) 

# model 3 is better than 4 but has a difference of 5

m4 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + sex:cohort + 0))
AIC(m3,m4)

#model 3 is still a better option

m4 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + 0))
AIC(m3,m4)

#model 4 is smaller with a df of 25 but a -15 difference

m5 <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + 0)
AIC(m4,m5)
 
#model 5 is better with a df of 32 but a -32 difference. model 5 is the best


setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
iix <- which(phenotypes[, "Longevity_HET3_ITP"] > 850)
phenotypes <- phenotypes[iix, ]
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
phaplo <- read.table ("pHaplo.txt",  sep = "\t")


phenotypes[1:5,1:5]
genotype[1:5,1:5]
parents[1:5,1:5]
map[1:3,1:3]


phenotypes[1:5, ]
table(phenotypes[, "Cohort.Year"])
table(phenotypes[, "Site"])

table(phenotypes[which(phenotypes[, "Site"] == "UM"), "Cohort.Year"])
table(phenotypes[which(phenotypes[, "Site"] == "UT"), "Cohort.Year"])
table(phenotypes[which(phenotypes[, "Site"] == "JL"), "Cohort.Year"])

plot(table(phenotypes[which(phenotypes[, "Site"] == "UM"), "Cohort.Year"]), xlab = "Cohort Year", ylab = "Mice Population", pch = 3, main = "Mice Population in Site UM")
plot(table(phenotypes[which(phenotypes[, "Site"] == "UT"), "Cohort.Year"]), xlab = "Cohort Year", ylab = "Mice Population", pch = 3, main = "Mice Population in Site UT")
plot(table(phenotypes[which(phenotypes[, "Site"] == "JL"), "Cohort.Year"]), xlab = "Cohort Year", ylab = "Mice Population", pch = 3, main = "Mice Population in Site JL")

Male <- which(phenotypes[, "Sex"] == "M")
Female <- which(phenotypes[, "Sex"] == "F")

length(Male)
length(Female)

UMP <- which(phenotypes[,"Site"] == "UM")
UTP <- which(phenotypes[,"Site"] == "UT")
JLP <- which(phenotypes[,"Site"] == "JL")

hist(phenotypes[UMP, "Longevity_HET3_ITP"])
hist(phenotypes[UTP, "Longevity_HET3_ITP"])
hist(phenotypes[JLP, "Longevity_HET3_ITP"])

MaleUM <- which(phenotypes[, "Site"] == "UM", phenotypes["Sex"] == "M")
FemaleUM <- which(phenotypes[,"Site"] == "UM", phenotypes[, "Sex"] == "F")
MaleUT <- which(phenotypes[,"Site"] == "UT", phenotypes[, "Sex"] == "M")
FemaleUT <- which(phenotypes[,"Site"] == "UT", phenotypes[, "Sex"] == "F")
MaleJL <- which(phenotypes[,"Site"] == "JL", phenotypes[, "Sex"] == "M")
FemaleJL <- which(phenotypes[,"Site"] == "JL", phenotypes[, "Sex"] == "F")


MaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
length(MaleUM)
FemaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")
length(FemaleUM)

length(UMP)

MaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "M")
length(MaleUT)
FemaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "F")
length(FemaleUT)

length(UTP)

MaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "M")
length(MaleJL)
FemaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "F")
length(FemaleJL)

length(JLP)

MaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
length(MaleUM)
FemaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")
length(FemaleUM)

length(UMP)

MaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "M")
length(MaleUT)
FemaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "F")
length(FemaleUT)

length(UTP)

MaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "M")
length(MaleJL)
FemaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "F")
length(FemaleJL)

length(JLP)

table(phenotypes[MaleUM, "Cohort.Year"])
hist(phenotypes[MaleUM, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at UM that lives for more than 850 days")
table(phenotypes[FemaleUM, "Cohort.Year"])
hist(phenotypes[FemaleUM, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at UM that lives for more than 850 days")
table(phenotypes[MaleUT, "Cohort.Year"])
hist(phenotypes[MaleUT, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at UT that lives for more than 850 days")
table(phenotypes[FemaleUT, "Cohort.Year"])
hist(phenotypes[FemaleUT, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at UT that lives for more than 850 days")
table(phenotypes[MaleJL, "Cohort.Year"])
hist(phenotypes[MaleJL, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at JL that lives for more than 850 days")
table(phenotypes[FemaleJL, "Cohort.Year"])
hist(phenotypes[FemaleJL, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at JL that lives for more than 850 days")

mean(MaleUM)
mode(phenotypes[MaleUM, "Cohort.Year"])
mean(phenotypes[MaleUM, "Cohort.Year"])
median(phenotypes[MaleUM, "Cohort.Year"])

plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])
plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])

boxplot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"], col = c("limegreen", "blue", "orange", "hotpink", "yellow", "gray", "green"))

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")
# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt") # map for 850 days
iix850 <- which(phenotypes[, "Longevity_HET3_ITP"] > 850)
phenotypes <- phenotypes[iix850, ]
map <- read.table ("map.sorted.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
gtsPM <- read.table("gtsPM.txt", sep = "\t", check.names=FALSE) # maps for 850 days
gtsPM <- read.table("gtsPM.txt", sep = "\t", check.names=FALSE)

gtsPM <- gtsPM[rownames(phenotypes), ] # subset using the rownames of the genotypes that are left

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

mnull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + 0)

pvals <- c()
for(marker in MingtsPM){
  iix <- grep(marker, colnames(gtsPM))
  gts <- as.matrix(gtsPM[, iix])
  mfull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + gts + 0)
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

plot(x = map[, "cumPos"], y = -log10(pvals), col = c("black", "orange")[chrI], pch = 19, xaxt = "n", xlab = "Chromosome", las = 2, main = "QTL mapping on longevity in UM-HET3 Mice that live for more than 850 days")
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

# Generating QTL Maps

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
iix850 <- which(phenotypes[, "Longevity_HET3_ITP"] > 850)
phenotypes <- phenotypes[iix850, ]


map <- read.table ("map.sorted.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
gtsPM <- read.table("gtsPM.txt", sep = "\t", check.names=FALSE)

gtsPM <- gtsPM[rownames(phenotypes), ] # subset using the rownames of the genotypes that are left


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

mnull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + 0)

# empty vector
adjusted <- rep(NA, length (Y))
# value after adjustment
adj <- residuals(mnull)
#values adjusted + mean longevity
adjusted[as.numeric(names(adj))] <- adj + mean(Y)

adjusted [1:10]


pvals <- c()
for(marker in MingtsPM){
  iix <- grep(marker, colnames(gtsPM))
  gts <- as.matrix(gtsPM[, iix])
  mfull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + gts + 0)
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

plot(x = map[, "cumPos"], y = -log10(pvals), col = c("black", "orange")[chrI], pch = 19, xaxt = "n", xlab = "Chromosome", las = 2, main = "QTL mapping on longevity in UM-HET3 Mice that lives more than 850 days")
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

cbind(map, LOD = -log10(pvals))

gtsFilled <- read.table("fille_geno.txt")

h <- unlist(gtsFilled["1_116455004",rownames(phenotypes)])

aa <-  boxplot(adjusted ~ h)
round(aa$stats[3,],0)

install.packages("vioplot")
#install a vioplot


library(vioplot)
#make a violin plot

vioplot(adjusted ~ unlist(gtsFilled["1_116455004",rownames(phenotypes)]),
	xaxt = "n", xlab = "Haplotype", las = 2, ylab = "Adjusted Longevity (days)",
	col = c("green", "blue", "orange", "hotpink"), main = "Longevity effect on Haplotypes", sub = "Markers at 1_116455004")
axis(1, at  = 1:4, c("A|C", "A|D", "B|C", "B|D"))

aa1 <-  boxplot(adjusted ~ unlist(gtsFilled["2_111645305", rownames(phenotypes)]))
round(aa1$stats[3,],0)
aa2 <-  boxplot(adjusted ~ unlist(gtsFilled["2_122522463", rownames(phenotypes)]))
round(aa2$stats[3,],0)
aa3 <-  boxplot(adjusted ~ unlist(gtsFilled["2_148990360", rownames(phenotypes)]))
round(aa3$stats[3,],0)
aa4 <-  boxplot(adjusted ~ unlist(gtsFilled["3_122855291", rownames(phenotypes)]))
round(aa4$stats[3,],0)
aa5 <-  boxplot(adjusted ~ unlist(gtsFilled["3_156416491", rownames(phenotypes)]))
round(aa5$stats[3,],0)
aa6 <-  boxplot(adjusted ~ unlist(gtsFilled["4_156147408", rownames(phenotypes)]))
round(aa6$stats[3,],0)
aa7 <-  boxplot(adjusted ~ unlist(gtsFilled["6_115619826", rownames(phenotypes)]))
round(aa7$stats[3,],0)
aa8 <-  boxplot(adjusted ~ unlist(gtsFilled["7_3162282", rownames(phenotypes)]))
round(aa8$stats[3,],0)
aa9 <-  boxplot(adjusted ~ unlist(gtsFilled["7_42613683", rownames(phenotypes)]))
round(aa9$stats[3,],0)
aa10 <-  boxplot(adjusted ~ unlist(gtsFilled["10_111300550", rownames(phenotypes)]))
round(aa10$stats[3,],0)
aa11 <-  boxplot(adjusted ~ unlist(gtsFilled["12_100838622", rownames(phenotypes)]))
round(aa11$stats[3,],0)
aa12 <-  boxplot(adjusted ~ unlist(gtsFilled["14_73006995", rownames(phenotypes)]))
round(aa12$stats[3,],0)
aa13 <-  boxplot(adjusted ~ unlist(gtsFilled["14_99384446", rownames(phenotypes)]))
round(aa13$stats[3,],0)
aa14 <-  boxplot(adjusted ~ unlist(gtsFilled["17_17733625", rownames(phenotypes)]))
round(aa14$stats[3,],0)
aa15 <-  boxplot(adjusted ~ unlist(gtsFilled["17_36854293", rownames(phenotypes)]))
round(aa15$stats[3,],0)
aa16 <-  boxplot(adjusted ~ unlist(gtsFilled["14_73006995", rownames(phenotypes)]))
round(aa16$stats[3,],0)

# Biomart 

setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

mr <- read.table("data850.txt", sep ="\t")
regions <- mr[, c(2,3,5)]
colnames(regions) <- c("Chr", "Proximal", "Distal")
regions

#Load the package and the correct version of the mouse genome
library(biomaRt)
bio.mart <- useMart("ENSEMBL_MART_ENSEMBL", host="https://nov2020.archive.ensembl.org", dataset="mmusculus_gene_ensembl")

listAttributes(bio.mart)[1:50,]
listFilters(bio.mart)[1:50,]

filter = c("chromosomal_region", "biotype")
attributes = c("ensembl_gene_id", "mgi_symbol", "mgi_description", "gene_biotype", "chromosome_name", "start_position", "end_position")

for(i in 1:nrow(regions)){
	r = paste0(regions[i, "Chr"], ":", regions[i, "Proximal"], ":", regions[i, "Distal"])
	
	value = list (r, "protein_coding")
	
	pcg <- getBM(attributes, filter, value, bio.mart)
	iix <- grep("predicted",pcg[, "mgi_description"])
	if(length(iix) > 0) pcg <- pcg[-iix, ]
	iix <- grep("RIKEN cDNA",pcg[, "mgi_description"])
	if(length(iix) > 0) pcg <- pcg[-iix, ]
	write.table(pcg, file = paste0("PCG_850_",gsub(":", "_",r), ".txt"), sep = "\t", quote=FALSE, row.names=FALSE)
}





