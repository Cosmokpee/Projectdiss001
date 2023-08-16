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