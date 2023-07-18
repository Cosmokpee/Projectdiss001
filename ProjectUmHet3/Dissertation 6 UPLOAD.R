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

map < - cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
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
hist(phenotypes[MaleUM, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at UM")
table(phenotypes[FemaleUM, "Cohort.Year"])
hist(phenotypes[FemaleUM, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at UM")
table(phenotypes[MaleUT, "Cohort.Year"])
hist(phenotypes[MaleUT, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at UT")
table(phenotypes[FemaleUT, "Cohort.Year"])
hist(phenotypes[FemaleUT, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at UT")
table(phenotypes[MaleJL, "Cohort.Year"])
hist(phenotypes[MaleJL, "Cohort.Year"], xlab = "Cohort Year", ylab = "Male Population", main = "Male Mice Population at JL")
table(phenotypes[FemaleJL, "Cohort.Year"])
hist(phenotypes[FemaleJL, "Cohort.Year"], xlab = "Cohort Year", ylab = "Female Population", main = "Female Mice Population at JL")

mean(MaleUM)
mode(phenotypes[MaleUM, "Cohort.Year"])
mean(phenotypes[MaleUM, "Cohort.Year"])
median(phenotypes[MaleUM, "Cohort.Year"])

plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])
plot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"])

boxplot(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Cohort.Year"], col = c("limegreen", "blue", "orange", "hotpink", "yellow", "gray", "green"))

