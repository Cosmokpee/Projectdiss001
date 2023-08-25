#
# Code for Dissertation (June 16th)
# 
setwd("C:/Users/Solom/OneDrive/Documents/Project Data/")

# Loading in the different files
phenotypes = read.table ("ind.sorted.phe.txt")
phenotypes[1:5,1:5]
phenotypes[1:5]
phenotypes[, "Control"]
which(phenotypes[, "Treatment_Effect"] == "Control")
ii28 <-  which(phenotypes[, "Treatment_Effect"] == "Control")
Length (ii28)



#Select Control from ii28
Control <- which(phenotypes[, "Treatment_Effect"] == "Control")
NDE <- which(phenotypes[, "Treatment_Effect"] == "NDE")

length(Control)
length(NDE)


ii28

plot(Control[, "Longevity_HET3_ITP"] ~ Control[, "BodyWeight_HET3_ITP_6m"])

#Make subsets matrices of the main phentype matrix933 
iic <- which(phenotypes[, "Treatment_Effect"] == "Control")
iin <- which(phenotypes[, "Treatment_Effect"]  == "NDE")


Control <- phenotypes[iic,]
NDE <- phenotypes[iin, ]
# creating a linear model checking the association between several parameters that is causing the effect of longetivity
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "BodyWeight_HET3_ITP_6m"]))

anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_6m"]))
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_6m"] + Control[, "BodyWeight_HET3_ITP_12m"] + Control[, "BodyWeight_HET3_ITP_18m"] + Control[, "BodyWeight_HET3_ITP_24m"]))


pie(c(10,24,50))
# ANOVA MODEL OF ASSOCIATION BETWEEN BODY WEIGHT SEX AND LONGVITY
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_6m"]))
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_12m"]))
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_18m"]))
anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_24m"]))

#BUILD A BIGGER MODEL INCLUDING 3 PREDICTOR
res <- anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_6m"]))
res1 <- anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_12m"]))
res2 <- anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_18m"]))
res3 <- anova(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_24m"]))

res[, "Sum Sq"]
res1[, "Sum Sq"]
res2[, "Sum Sq"]
res3[, "Sum Sq"]

 # COMPUTE VARIANCE EXPLAINED: sUM OF SQUARES/ sUM sUM of Squares
(res[, "Sum Sq"] / sum(res[, "Sum Sq"])) * 100
percentages <- (res[, "Sum Sq"] / sum(res[, "Sum Sq"])) * 100
pie(round(percentages, 1))


(res1[ "Sum Sq"] / sum(res1[, "Sum Sq"])) * 100
percentages <- (res1[, "Sum Sq"] / sum(res1[, "Sum Sq"])) * 100
pie(round(percentages, 1))

(res2[, "Sum Sq"] / sum(res2[, "Sum Sq"])) * 100
percentages <- (res2[, "Sum Sq"] / sum(res2[, "Sum Sq"])) * 100
pie(round(percentages, 1))

(res3[, "Sum Sq"] / sum(res3[, "Sum Sq"])) * 100
percentages <- (res3[, "Sum Sq"] / sum(res3[, "Sum Sq"])) * 100
pie(round(percentages, 1))

# 	reades as a as.factor
  
ml <- lm GER MODEL INCLUDING 3 PREDICTOR
res <-1(lm(Control[, "Longevity_HET3_ITP"] ~ Control[, "Sex"] + Control[, "BodyWeight_HET3_ITP_6m"]))

#compare variandddce 
# conclusion ml is preferred because 'it's easier / more parcimoneous and m2 is not 10 points better


toseason <- function(month) {
 if (month == 12 | month == 1 | month == 2) { return("winter") }
 if (month == 3 | month == 4 | month == 5) { return("spring") }
 if (month == 6 | month == 7 | month == 8) { return("summer") }
 if (month == 9 | month == 10 | month == 11) { return("fall") } 
) 
 }
toseason (6)

#UPDATED

toseason <- function(month) {
 if (month == 12 | month == 1 | month == 2) { return("winter") }
 if (month == 3 | month == 4 | month == 5) { return("spring") }
 if (month == 6 | month == 7 | month == 8) { return("summer") }
 if (month == 9 | month == 10 | month == 11) { return("fall") } 
return(NA) 
 }
toseason (6)


colnames(phenotypes)
# wg2 numeric
m1 <- lm(phenotypes[, "Longevity_HET3_ITP"] ~ as.factor(phenotypes[, "Sex"]))
# colnames
# "Origin"                  "Batch"                   "Site"                    "Sex"                    
# [5] "Longevity_HET3_ITP"      "Treatment_Effect"        "Cohort.Year" 

m2 <- lm(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Sex"] + phenotypes[, "Treatment_Effect"])
AIC(m1,m2)
#   df      AIC
#m1  3 88130.81
#m2  4 88099.42
#concl: m2 is preferred because of the 10 point drop

m3 <- lm(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Sex"] + phenotypes[, "Treatment_Effect"] + phenotypes[, "Cohort.Year" ])
AIC(m2, m3)
#   df      AIC
#m2  4 88099.42
#m3  5 88021.37

#concl: m3 is preferred  because of 17 point drop for m2

m4 <- lm(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Sex"]+ phenotypes[, "Treatment_Effect"] + phenotypes[, "Cohort.Year" ] + phenotypes[, "Site"])
AIC(m3,m4)
  # df      AIC
#m3  5 88021.37
#m4  7 87914.33
# concl: m4 is preferred because of the 77 point drop from m3
m5 <- lm(phenotypes[, "Longevity_HET3_ITP"] ~ phenotypes[, "Sex"] phenotypes[, "Treatment_Effect"] + phenotypes[, "Cohort.Year" ] + phenotypes[, "Site"] + phenotypes[, "Origin"])
AIC(m4,m5)
#   df      AIC
#m4  7 87914.33
#m5  4 87991.57
# concl: m4g is preferred because of the 6 point drop from m4


