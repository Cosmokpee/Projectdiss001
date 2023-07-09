#
##
###Solomon
####
##### Data Analysis part 6
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
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
length(MaleUM)
FemaleUM <- which(phenotypes[,"Site"] == "UM", phenotypes[, "Sex"] == "F")
length(FemaleUM)
MaleUT <- which(phenotypes[,"Site"] == "UT", phenotypes[, "Sex"] == "M")
length(MaleUT)
FemaleUT <- which(phenotypes[,"Site"] == "UT", phenotypes[, "Sex"] == "F")
length(FemaleUT)
MaleJL <- which(phenotypes[,"Site"] == "JL", phenotypes[, "Sex"] == "M")
length(MaleJL)
FemaleJL <- which(phenotypes[,"Site"] == "JL", phenotypes[, "Sex"] == "F")
length(FemaleJL)

MaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "M")
FemaleUM <- which(phenotypes[,"Site"] == "UM" & phenotypes[,"Sex"] == "F")
MaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "M")
FemaleUT <- which(phenotypes[,"Site"] == "UT" & phenotypes[,"Sex"] == "F")
MaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "M")
FemaleJL <- which(phenotypes[,"Site"] == "JL" & phenotypes[,"Sex"] == "F")

plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])
plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])

boxplot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"], col = c("limegreen", "blue", "orange", "hotpink", "yellow", "gray", "green"))

