############################################################
## Problem 2. Sum of the even-valued terms in Fibonacci seq.
############################################################

library(gmp)
fibnumb <- function(n){ ## find nth term of sequence
phi <- (1+sqrt(5))/2
num <- (phi^n - (1-phi)^n)/sqrt(5)
return(as.bigz(num))
}

##. Fibonacci sequence for nth terms
fibonacci <- function(limit){
  vector <- c(0,1)
  for (i in 3:limit){
    next_elem <- vector[i-1]+vector[i-2]
    vector <- c(vector,next_elem)
  }
  return(vector)
}

##. Sum of all even numbers in Fibonacci up to 4e+6(n = 34)
evenfib_sum <- function(limit){ ## I came upon n =34 by chance.
                                ## I tested the Fibonacci func, and 
                                ## realized the 35th term was over 4e+6
  vector <- c(0,1)
  for (i in 3:limit){
    next_elem <- vector[i-1]+vector[i-2]
    vector <- c(vector,next_elem)
    this <- which(vector%%2 == 0)
    that <-vector[this]
  }
  return(sum(that)) 
}

############################################################
## Problem 3. largest prime factor of 600851475143
## (Use my Prime_Finder script).
############################################################

factorize <- function(limit){
  if(!length(limit)==1|| !limit == round(limit)||limit < 0){
    stop("Argument must be a nonnegative integer")
    if(limit < 4){
      return(limit)
    }
  }
  prime_factors <- (numeric())
  pnum <- sieve(sqrt(limit))
  fact <- which(limit%%pnum == 0)
  if(length(fact)== 0){
    return(limit)
  }
  for(i in pnum[fact]){
    while((limit)%%i == 0){
      prime_factors <- c(prime_factors,i)
      limit <- limit/i
    }
  } 
  if(limit >1)
    prime_factors <- c(prime_factors,limit)
  return(prime_factors)
}

############################################################
## Problem 4. Largest palindrome made from the product of two 
## 3-digit numbers.
############################################################
require(stringi)
palindrome <- numeric()
for (i in 100:999) {
  for(j in 101:999){
    result <- i*j
    if (as.character(result) == stri_reverse(result)) {
      palindrome <- c(palindrome, as.numeric(result))
    }
  }
}
max_pal <- max(palindrome)
print(max_pal)


#############################################################
## Problem 5. Smallest (+) integer divisible by 1:20
#############################################################

divisor <- 2520 ## 2520 is divisible by 1:10, we can start here.
while (sum(divisor %% (1:20)) != 0) {
  divisor <- divisor + 2520 ## and add 2520 each time until div
                            ## by 1:20
}
small_div <- divisor

#############################################################
## Problem 6. Sum Square Difference
#############################################################

ssqrdif <- function(limit) {
  ssquare <- sum((1:limit)^2)
  sum2 <- (sum(1:limit))^2
  return(sum2 - ssquare)
}


