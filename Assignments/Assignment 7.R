# Answers to the assignments 7
##
### Danny arends
####
####


#1

# Initialize the sum variable
sum_multiples <- 0

# Iterate through numbers from 1 to 999
for (num in 1:999) {
  # Check if the number is a multiple of 3 or 5
  if (num %% 3 == 0 || num %% 5 == 0) {
    # Add the number to the sum
    sum_multiples <- sum_multiples + num
  }
}

# Print the sum of the multiples
print(sum_multiples)

#2
					# Function to generate the Fibonacci sequence up to a given limit
					fibonacci <- function(limit) {
					  sequence <- c(1, 2)  
					  # Initialize the sequence with the first two terms
					  while (tail(sequence, 1) + tail(sequence, 2) <= limit) {
						next_term <- tail(sequence, 1) + tail(sequence, 2)
						sequence <- c(sequence, next_term)
					  }
					  return(sequence)
					}

					# Calculate the Fibonacci sequence
					fib_sequence <- fibonacci(1e6)

					# Initialize the sum variable
					sum_even <- 0

					# Iterate through the Fibonacci sequence and sum the even-valued terms
					for (num in fib_sequence) {
					  if (num %% 2 == 0) {
						sum_even <- sum_even + num
					  }
					}

					# Print the sum of the even-valued terms
					print(sum_even)
					#sum=0
					
#2 option2 
# Function to generate the Fibonacci sequence up to a given limit
fibonacci <- function(limit) {
  sequence <- c(1, 2)  # Initialize the sequence with the first two terms
  while (tail(sequence, 1) <= limit) {
    next_term <- tail(sequence, 1) + tail(sequence, 2)
    if (next_term <= limit) {
      sequence <- c(sequence, next_term)
    } else {
      break
    }
  }
  return(sequence)
}

# Calculate the Fibonacci sequence
fib_sequence <- fibonacci(1e6)

# Initialize the sum variable
sum_even <- 0

# Iterate through the Fibonacci sequence and sum the even-valued terms
for (num in fib_sequence) {
  if (num %% 2 == 0) {
    sum_even <- sum_even + num
  }
}

# Print the sum of the even-valued terms
print(sum_even)
sum = 0



#3
# Function to check if a number is a palindrome
is_palindrome <- function(num) {
  num_str <- as.character(num)
  reversed_str <- paste(rev(strsplit(num_str, "")[[1]]), collapse = "")
  return(num_str == reversed_str)
}

# Variables to store the largest palindrome and its factors
largest_palindrome <- 0
factor1 <- 0
factor2 <- 0

# Iterate through all possible combinations of 3-digit numbers
for (i in 100:999) {
  for (j in 100:999) {
    product <- i * j
    if (is_palindrome(product) && product > largest_palindrome) {
      largest_palindrome <- product
      factor1 <- i
      factor2 <- j
    }
  }
}

# Print the largest palindrome and its factors
print(largest_palindrome)
print(factor1)
print(factor2)


#4
# Recursive function to count towards 0
count_towards_zero <- function(num) {
  if (num == 0) {
    return(0)  # Base case: If num is already 0, return 0
  } else {
    print(num)
    count_towards_zero(num - 1)  # Recursive call with num decremented by 1
  }
}

# Call the recursive function with an initial number
count_towards_zero(10)

#5
mylapply <- function(x, FUN, ...) {
  if (!is.list(x)) {
    stop("Input 'x' must be a list")
  }
  
  lapply(x, FUN, ...)
}
# Example usage of mylapply
my_list <- list(a = 1:3, b = 4:6, c = 7:9)

# Apply the sum function to each element of the list using mylapply
result <- mylapply(my_list, sum)

print(result)


#6

gcd <- function(a, b) {
  if (b == 0) {
    return(a)  # Base case: if b is 0, GCD is a
  } else {
    return(gcd(b, a %% b))  # Recursive call with a and b modulo b
  }
}
# Example usage of gcd function
a <- 8
b <- 12
result <- gcd(a, b)

print(result)
