#
##
###Solomon - Dissertation Session 10 Update
####
##### QTL mapping summary table genration for mice that lives for more than 850 days

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
  mfull <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort +  cohort:treatment + gts + 0)
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
  pos <- pos + chr.length[x] + 50000000
}

plot(x = map[, "cumPos"], y = -log10(pvals), col = c("blue", "green")[chrI], pch = 19, xaxt = "n", xlab = "Chromosome", las = 2, main = "QTL mapping on longevity in UM-HET3 Mice over 850 days")
i <- 1
for(x in 1:20){
  iim <- which(map[, "Chr"] == x)
  points(x = map[iim, "cumPos"], y = -log10(pvals[iim]), t = 'l', col = c("blue", "green")[i])
  i <- i + 1
  if(i > 2) i <- 1
}
abline(h = threshold5, col = "black", lty = 2)
abline(h = threshold1, col = "pink", lty = 2)
abline(h = threshold01, col = "yellow", lty = 2)
axis(1, at = chr.mids, c(1:19, "X"))
legend("topleft", c("0.1%", "1%", "5%"), lty = 2, col =c("yellow", "pink", "black"), title = "Bonferonni Threshold")


cbind(map, LOD = -log10(pvals))

gtsFilled <- read.table("fille_geno.txt")
aa <-  boxplot(adjusted ~ unlist(gtsFilled["1_116455004", ]))
round(aa$stats[3,],0)

install.packages("vioplot")
#install a vioplot


library(vioplot)
#make a violin plot

vioplot(adjusted ~ unlist(gtsFilled["1_116455004", ]),
	xaxt = "n", xlab = "Haplotype", las = 2, ylab = "Adjusted Longevity (days)",
	col = c("green", "blue", "orange", "hotpink"), main = "Longevity effect on Haplotypes", sub = "Markers at 1_116455004")
axis(1, at  = 1:4, c("A|C", "A|D", "B|C", "B|D"))

aa1 <-  boxplot(adjusted ~ unlist(gtsFilled["1_116455004", ]))
round(aa1$stats[3,],0)

aa2 <-  boxplot(adjusted ~ unlist(gtsFilled["1_170885169", ]))
round(aa2$stats[3,],0)
aa3 <-  boxplot(adjusted ~ unlist(gtsFilled["4_155785470", ]))
round(aa3$stats[3,],0)
aa4 <-  boxplot(adjusted ~ unlist(gtsFilled["6_114480563", ]))
round(aa4$stats[3,],0)
aa5 <-  boxplot(adjusted ~ unlist(gtsFilled["7_42299841", ]))
round(aa5$stats[3,],0)
aa6 <-  boxplot(adjusted ~ unlist(gtsFilled["8_129129250", ]))
round(aa6$stats$stats[3,],0)
aa7 <-  boxplot(adjusted ~ unlist(gtsFilled["7_42299841", ]))
round(aa7$stats[3,],0)
aa8 <-  boxplot(adjusted ~ unlist(gtsFilled["7_139640399", ]))
round(aa8$stats[3,],0)
aa9 <-  boxplot(adjusted ~ unlist(gtsFilled["8_129072461", ]))
round(aa9$stats[3,],0)
aa10 <-  boxplot(adjusted ~ unlist(gtsFilled["10_13167814", ]))
round(aa10$stats[3,],0)
aa11 <-  boxplot(adjusted ~ unlist(gtsFilled["12_34390148", ]))
round(aa11$stats[3,],0)
aa12 <-  boxplot(adjusted ~ unlist(gtsFilled["14_73006995", ]))
round(aa12$stats[3,],0)
aa13 <-  boxplot(adjusted ~ unlist(gtsFilled["17_17733625", ]))
round(aa13$stats[3,],0)
aa14 <-  boxplot(adjusted ~ unlist(gtsFilled["18_70827264", ]))
round(aa14$stats[3,],0)
aa15 <-  boxplot(adjusted ~ unlist(gtsFilled["20_150646933", ]))
round(aa15$stats[3,],0)




