 1:10 #One to a hundred
 seq(1,50,4) #One to fifty stepping 4
 cat("ind",1:10,sep=",")
 paste("ind",1:10,sep="")
  sqrt((1+5)/4)
  12/2 == 6
   1:10 < 7
    matrix(0,3,5)
	numbers <- seq(1,50,4)
sort(numbers,decreasing = TRUE)
 <- runif(1)*10 #Random number between 0 and 10
if(result < 5){
print("lower")
}else{
print("higher")
}
\file assignments.R
#
# Copyright (c) 2010, Danny Arends, p256802
# last modified Jan, 2014
# first written May, 2013
#
# A copy of the GNU General Public License, version 3, is available
# at http://www.r-project.org/Licenses/GPL-3
getwd()
###
# \file assignments.R
# Solomon Ademoyegun
# w22015727
# date created Jun,2023
# 1:10 #One to a hundred
# [1]  1  2  3  4  5  6  7  8  9 10
#>  seq(1,50,4) #One to fifty stepping 4
# [1]  1  5  9 13 17 21 25 29 33 37 41 45 49
#>  cat("ind",1:10,sep=",")
# ind,1,2,3,4,5,6,7,8,9,10> 
# >  paste("ind",1:10,sep="")
# [1] "ind1"  "ind2"  "ind3"  "ind4"  "ind5"  "ind6"  "ind7"  "ind8"  "ind9" 
# [10] "ind10"
# Copyright (c) 2010, Danny Arends, p256802
# last modified Jan, 2014
# first written May, 2013
#
# A copy of the GNU General Public License, version 3, is available
# at http://www.r-project.org/Licenses/GPL-3
#
# Contains R functions: myFunction
###
# Answer to 1a) 
1234 + 4567 
4596/2floor
# Answer to 1b)
 100456 - 3350 + 23
# Answer to 1c)
 natural logarithm of 15
# Answer to 1d) 
4596 / 12
#Answer to 1e)
 8998 * 76
# Answer to 1f)
Euclidean division remainder of 10 and 6
# Answer to 1g)
the square root of -8
"<your location>/Assignments1/"
2a) Use the c() function to create a vector from 1 to 10
2b) Use the : operator to create a vector from 11 to 20
2c) We can also use the seq() function to create more complex vectors, create a vector from 1 to 100
going in steps of 5. (so: 1, 6, 11, …)
2d) Use the LETTERS constant and the seq() function to create a vector that stores all the ‘even’
letters (gerade Buchstaben: B, D, F, etc)
2e) what is the type of vector2a, either use the class() function or ask explicitly using the is.numeric()
, is.character() or the is.logical() functions
2f) combine vector2a and vector2d, what is the type of the resulting vector ?
2g) Use the sqrt() function to compute the square root of vector2a
clas
x <- 2
 height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
 bmi <- weight/height^2
 bmi
 xbar <- sum(weight)/length(weight)
 weight - xbar
(weight - xbar)^2
hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
 lines(hh, 22.5 * hh^2)
args(plot.default)
function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
ann = par("ann"), axes = TRUE, frame.plot = axes,
panel.first = NULL, panel.last = NULL, asp = NA, ...)
c(42,57,12,39,1,3,4)
x <- c(1, 2, 3)
y <- c(10, 20)
c(x, y, 5)
100456 - 3350 + 456
[1] 97562
log(15)
[1] 2.70805
4596 / 12
[1] 383
8998 * 76
[1] 683848
floor (10 / 6)
[1] 1
sqrt (-8)
[1] NaN
c (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 
[1]  1  2  3  4  5  6  7  8  9 10
c ( 11:20)
[1] 11 12 13 14 15 16 17 18 19 20
> seq (1,20,5)
[1]  1  6 11 16
createEvenLettersVector <- function() {even_letters <- LETTERS[seq(from = 2, to = length(LETTERS), by = 2)]return(even_letters)}
# Call the function
even_letters_vector <- createEvenLettersVector()
createEvenLettersVector()
[1] "B" "D" "F" "H" "J" "L" "N" "P" "R" "T" "V" "X" "Z"
class (c)
[1] "function"
> is.character(c)
[1] FALSE
> is.logical (c)
[1] FALSE
