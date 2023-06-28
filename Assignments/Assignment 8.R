# Answers to the assignments lecture 8
##
### Danny arends
####
###

#1 
X3 <- seq(3, 999, 3)
X5 <- seq(5, 999, 5)

sum(c(X3[-which(X3 %in% X5)],X5))

or

sum(unique(c(X3,X5)))

#2 

fib <- function(x){
  if(x == 1) return(1)
  if(x == 2) return(2)
  return(fib(x-1) + fib(x-2))
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)

x <- 1
fibsum <- 0
fibnum <- fib(x)
while (fibnum < 1000000) {
  if ((fibnum %%2) == 0) {
    cat(x, " ", fibnum, "\n")
    fibsum <- fibsum + fibnum
  }
  x <- x + 1        # Increase x
  fibnum <- fib(x)  # Calculate the new fibonaci number
}
fibsum

#3

isPalindrome <- function(mystr) {
  rev_mystr <- paste0(rev(strsplit(mystr, "")[[1]]), collapse="")
  return(rev_mystr == mystr)
}

xI <- 0
yI <- 0
bigSoFar <- 0
for (x in 100:999) {
  for (y in x:999) {
    if (isPalindrome(as.character(x * y)) && x*y > bigSoFar) {
      bigSoFar <- x*y
      xI <- x
      yI <- y
    }
  }
}
bigSoFar
xI
yI

#4
countdown <- function(x) {
  if(x == 0){ cat("Countdown finished\n") }
  if(x > 0){ cat("Count:", x, "\n"); countdown(x - 1) }
  if(x < 0){ cat("Count:", x, "\n"); countdown(x + 1) }
}

countdown(100)
countdown(-100)

#5
myLapply <- function(x, FUN, ...) {
  if(!is.list(x)) stop("x must be a list")
  return(lapply(x, FUN, ...))
}

myLapply <- function(x, FUN, ...) {
  if(!is.list(x)) stop("x must be a list")
  res <- vector("list", length(x))
  for(i in 1:length(x)){
    res[[i]] <- FUN(x[[i]], ...)
  }
  return(res)
}

#6

mydots <- function(...){
  params <- list(...)
  for(i in 1:length(params)){
    cat(names(params)[i], "=", params[[i]], "\n")
  }
}

mydots(a = 10, xx = 14)


#7
GCD <- function(a, b){
  if(a == b){ return(a); }
  if(a < b){ return(GCD(a, b - a)); }
  if(a > b){ return(GCD(a - b, b)); }
}

GCD(100, 20)
GCD(8, 12)

#8
drawTree <- function(x1, y1, angle, depth, maxdepth = 8, nbranch = 3){
  if (depth != 0){
    x2 = x1 + (sin(angle * (pi / 180)));
    y2 = y1 + (cos(angle * (pi / 180)));
    lines(x=c(x1, x2), y=c(y1,y2))
    for(x in 1:nbranch){
      drawTree(x2, y2, 120 * (runif(1)-0.5), depth - 1, maxdepth, round((runif(1) * 5) + 1));
    }
  }
}
plot(c(-10,10),c(0,10), t = 'n')
drawTree(0, 0, 15, 2)

#8 with colors
drawTree <- function(x1, y1, angle, depth, maxdepth = 8, nbranch = 3){
  if (depth != 0){
    x2 = x1 + (sin(angle * (pi / 180)) * sqrt(depth) * depth * 4);
    y2 = y1 + (cos(angle * (pi / 180)) * sqrt(depth) * depth * 4);
    linecol <- rgb(0, runif(1,0.5,1), 0) ; lwd = 1;
    if(depth > 5 || runif(1) < 0.05){ linecol <- "brown" ; lwd = 2;}
    lines(x=c(x1, x2), y=c(y1,y2), col=linecol, lwd = lwd)
    for(x in 1:nbranch){
      drawTree(x2, y2, angle + (30 * ((maxdepth-1) - depth) * nbranch  * (runif(1)-0.5)), depth - 1, maxdepth, round((runif(1) * 5) + 1));
    }
  }
}
plot(c(0,600),c(0,800), t = 'n')
drawTree(300,0,10,8)