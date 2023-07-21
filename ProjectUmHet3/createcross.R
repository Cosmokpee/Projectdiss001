#
##
###Solomon - Imputation using R/qtl
####
##### Data Analysis part 8 continued
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes <- read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
map <- cbind(map, PosName = paste(map[, "Chr"], map [, "Position"],sep="_"))
haplo <- read.table("haplotypes_uniques.txt",  sep = "\t", na.strings = "", colClasses ="character")
haplo[1:10,1:10]


haplo[haplo == "??"] <- NA
haplo[haplo == "X"] <- NA
haplo[haplo == "AC"] <- 1
haplo[haplo == "BC"] <- 2
haplo[haplo == "AD"] <- 3
haplo[haplo == "BD"] <- 4
haplo[haplo == "A?"] <- 5
haplo[haplo == "B?"] <- 6
haplo[haplo == "?C"] <- 7
haplo[haplo == "?D"] <- 8


mArranged <- map[rownames(haplo), c("Chr", "Position")] 
mArranged[, "Position"] <- as.numeric(mArranged[, "Position"]) / 1000000
mPrep <- cbind(mArranged,haplo)
pArranged <- t(phenotypes[, c("Sex", "Site", "Cohort.Year", "Treatment_Effect", "Longevity_HET3_ITP")])
pArranged <- cbind(NA,NA,pArranged)
pArranged["Longevity_HET3_ITP", ] <- as.numeric(pArranged["Longevity_HET3_ITP", ])


write.table(pArranged, "Cross.csvr", sep = "," ,quote = FALSE, na = "" , col.names = FALSE)
write.table(mPrep, "Cross.csvr" ,sep = "," ,quote = FALSE, na = "", append = TRUE, col.names = FALSE)

library(qtl)
rQTLmap <- read.cross("csvr", file = "Cross.csvr", genotypes = NULL, estimate.map = FALSE, convertXdata= FALSE)

rQTLfilled <- fill.geno(rQTLmap, method = "maxmarginal", min.prob = 0.80)

mgeno <- pull.geno(rQTLfilled)
rownames(mgeno) <- colnames(haplo)
write.table(t(mgeno), "fille_geno.txt", sep = "\t", quote=FALSE)

genoprob <- calc.genoprob(rQTLmap)
gtsPM <- pull.genoprob(genoprob)





