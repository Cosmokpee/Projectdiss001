mlapply <- function(x, FUN,...)
find_mode <- function(matrix_data) {
  modes <- apply(matrix_data, 2, function(column) {
    # Count the occurrences of each number in the column
    count <- table(column)

    # Find the number(s) with the maximum count in the column
    max_count <- max(count)
    mode <- as.numeric(names(count[count == max_count]))

    return(mode)
  })

  return(modes)
}

# Example usage
data <- matrix(c(1, 2, 3, 2, 2, 4, 5, 4, 2,
                3, 2, 1, 4, 4, 2, 5, 2, 3), ncol = 2)

result <- find_mode(data)
print("Modes for each column:")
print(result)


#
# Code for Dissertation (June 16th)
# 
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes = read.table ("ind.sorted.phe.txt")
map <- read.table ("map.sorted.txt")
genotype <- read.table ("all.vcf.sorted.txt")
parents <- read.table ("fvcfAll.txt")
phenotypes[1:10,1:10]
iix <- which(phenotypes[, "Longevity_HET3_ITP"] > 365)
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

m3 <- lm(Y ~ sex + site + cohort + 0)
AIC(m2,m3)

m4 <- lm(Y ~ sex + site + cohort + treatment + 0)
AIC(m3,m4)

forpie <- anova(lm(Y ~ sex + site + cohort + treatment ))
pie(forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"]))

m5 <- (lm(Y ~ sex + site + cohort + treatment + sex:treatment + 0))
AIC(m4,m5)
# stay with model pove drug does not improve
 m5
 
 m5 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + 0))
 AIC(m4,m5)
 # model is the best
 
 m6 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + sex:cohort + 0))
 AIC(m5,m6)
 #model 5  is the best
 
 m6 <- (lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort + 0))
 AIC(m5,m6)
 m6 
 # model 6 is the smallest and the best


forpie <- anova(lm(Y ~ sex + site + cohort + treatment + sex:site + site:cohort))
pie(forpie[,"Sum Sq"] / sum(forpie[,"Sum Sq"]))
 