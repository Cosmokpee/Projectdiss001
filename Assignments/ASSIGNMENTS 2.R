#
##
###Answers to the assignments  2
#### 

#1a)
  secret <- runif(1)

#1b)
if(secret  < 0.5) {
  cat("Lower\n")
}else{
  cat("Higher\n")
}

#1c)
secret  <- runif(1, -10, 30)    

# (runif(1) * 40) - 10
if(secret  >= 0 && secret  <= 10) {
  cat("secret  value is", secret , "\n")
}else{
  stop(paste("Not in the range: [0, 10] value:", secret ))
}

#2a)
forsum <- 0
for(x in 1:1000) {
  forsum = forsum + x
}
forsum

#2b)
x <- 1
whilesum <- 0
while(x <= 1000) {
  whilesum = whilesum + x
  x = x + 1
}
whilesum

#3)
for(x in 1:100) {
   myvar <- round(runif(1, 0, 100)) # Also: myvar <- round(runif(1) * 100)
  if(myvar < 42) {
    cat(myvar, "is lower than 42\n")
  }else if(myvar == 42) {
    cat(myvar, "is the answer to life the universe and everything\n")
  }else{
    cat(myvar, "is higher than 42\n")
  }
}

#4)
for(x in 1:12) {
  for(y in 1:x) {
    cat("#")          # Print the hash tag x times
  }
  cat("\n")           # Done with the line
}

#7) Escaping
cat("I say: \"Escaping stuff is 'great', but \\ and / might be a nuisance.\n", file = "out.txt")
cat("You are correct, but I think the \\t and \\b create more problems then a basic “", file = "out.txt", append=TRUE)

#0a & 0b)
set.seed(1)
random1 <- round(runif(15 , 0, 10))
#0c)
set.seed(1)
rnorm(1)
#0d)
random2 <- round(runif(15 , 0, 10))
#0e) 
random1 
# Inspect what is in the variable
random2 
# Inspect what is in the variable
## The state was changed by drawing another random number
#### The state in my case, made the numbers shift loosing the first 3 numbers because of the rnorm call

#5)
flipCoin <- function() {
  rnd <- runif(1, 0, 1)
  if(rnd <= 0.5) return("Head")
  return("Tails")
}
# Flip the coin a couple of times
flipCoin()
flipCoin()
flipCoin()

#6)
triangle <- function(size){
  for(x in 1:size){
    cat(paste0(rep("#", x), collapse = ""), "\n")
  }
}
triangle(25)

#7a)
myfactorial <- function(x){
  f <- 1
  for(i in 1:x){ f <- i * f }
  return(f)
}


#Extra 1)
"!" <- function(x){ 
  myfactorial(x)
}

!5
!(5)

#Extra 2)
# This equation was known to the Pythagoreans as early as the sixth century BC
sumO <- function(x){
  return( (x * (x+1)) / 2)
}

#Extra 3)
isPrime <- function(N) {
  if(N < 0) stop("Function is only for positive integers")    
  
 # Make sure the function input is valid
  if(N == 0 || N == 1) return(FALSE)                         
  # By definition not prime: "A prime number is one with exactly two positive divisors"
  check <- 2
  while(check < (N-1)) {                                      
  # We don't have to check up to (N-1), we could stop at sqrt(N)
    if(N %% check == 0) return(FALSE)                         
	# Check divides the number N without any remainder
    check <- check + 1
  }
  return(TRUE)
}

for(x in 0:100){
  cat(x, "", isPrime(x), "\n")
}