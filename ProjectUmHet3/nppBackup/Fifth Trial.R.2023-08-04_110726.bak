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

#model 3 is better but had degree of freedom at 12(df=12)  because there is a -5 difference

m4 <- lm(Y ~ sex + site + cohort + treatment + 0)
AIC(m3,m4)

# model 4 is better than 3 but has df of 13 because there is a -14 difference
model 

forpie <- anova(lm(Y ~ sex + site + cohort + treatment ))
pie(forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"]))

round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
pch <- round(100 * (forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"])), 1)
names(pch) <- c("Sex", "Site", "Cohort", "Treatment", "Unknown")
pie(pch)

m5 <- lm(Y ~ sex + site + cohort + treatment + sex:site)
AIC(m4,m5) 

# model 5 is better than 4 but has df of 15 and a difference of 5

m6 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + sex:cohort + 0))
AIC(m5,m6)

#model 5 is still a better option because m6 had a df of 22 but its higher than m5 with 7

m6 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + 0))
AIC(m5,m6)

#model 6 is smaller with a df of 25 but a -8 difference

m7 <- lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + cohort:treatment + 0)
AIC(m6,m7)