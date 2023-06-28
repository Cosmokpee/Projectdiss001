# Answers to the assignments lecture 4
##
### Danny arends
####
###


data(iris)

mode <- function(x) {
  ux <- unique(x); 
  return(ux[which.max(tabulate(match(x, ux)))])
}

#1a
setosa <- subset(iris, Species == "setosa", 
                       select = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
apply(setosa, 2, mean) ; apply(setosa, 2, median) ; apply(setosa, 2, mode)

versicolor <- subset(iris, Species == "versicolor", 
                           select=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
apply(versicolor, 2, mean) ; apply(versicolor, 2, median) ; apply(versicolor, 2, mode)

virginica <- subset(iris, Species == "virginica", 
                          select = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
apply(virginica, 2, mean) ; apply(virginica, 2, median) ; apply(virginica, 2, mode)

#1b
apply(subset(iris, select=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")), 2, var)
apply(setosa, 2, var) ; apply(versicolor, 2, var) ; apply(virginica, 2, var)

#1c
geometricMean <- function(x){ 
  return(prod(x) ^ (1 / length(x))) 
}

#1d
harmonicMean <- function(x){
  return(length(x) / sum(1/x))
}

#2a
hist(setosa[,"Petal.Length"])
hist(versicolor[,"Petal.Length"])
hist(virginica[,"Petal.Length"])

#2b
par(cex.main=2)
hist(virginica[,"Petal.Length"], main= "virginica (Petal Length)")

#2c
par(mfrow = c(1,3))
hist(setosa[,"Petal.Length"], main="setosa", xlab = "Petal Length (cm)")
hist(virginica[,"Petal.Length"], main="virginica", xlab = "Petal Length (cm)")
hist(versicolor[,"Petal.Length"], main="versicolor", xlab = "Petal Length (cm)")

#Reset to a single plot window after we're done
par(mfrow = c(1,1))

#2d
getwd()
png("myplot.png", width = 1024, height = 800)
  par(cex.main=2)
  hist(virginica[,"Petal.Length"], main= "virginica (Petal Length)")
dev.off()

#3a
plotdata <- cbind(setosa[,"Sepal.Length"], versicolor[,"Sepal.Length"], virginica[,"Sepal.Length"])
boxplot(plotdata, main="Sepal Length Comparison", xaxt='n')
axis(1, at=1:3, c("Setosa", "Versicolor", "Virginica"))

#3b)
boxplot(plotdata, main="Sepal Length Comparison", xaxt='n', notch=TRUE)
axis(1, at=1:3, c("Setosa", "Versicolor", "Virginica"))
