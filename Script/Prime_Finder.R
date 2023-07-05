## The Sieve of Eratosthenes

## Steps
## 1. Create an array of size n+1
## 2. Starting with the number p = 2, iterate through the array from 'p' to 'n', with increments of 'p':
    ## a. For each multiple 'm' of 'p', mark the array value as false (false denotes a multiple)
## 3. Find the smallest number `p` in the array that is true and greater than `p` (the next prime)
    ## a. If no such number exists, terminate the algorithm
## 4. Set `p` to `p'` and repeat steps 3 -5

################################################################
## A. Creating function to find multiples in an array of values
################################################################
## loading multiple R libraries
using("gmp","rbenchmark","pracma")

find_multiples <- function(num,limit) {
  naturals <- 1:limit
  
   multiples <- sapply(naturals,function(i) if(i %% num == 0)i else NULL)
   multiples <- unlist(multiples[!is.null(multiples)])
   
  return(multiples)
}
 
################################################################
## B. Creating prime sieve
################################################################

sieve <- function(limit){
  naturals <- 2:limit
  numbers <- seq(2,floor(sqrt(limit)))
  
    multiples <- sapply(numbers,function(i) i * (2:floor(limit/i)))
    multiples <- unlist(multiples)
    naturals <- naturals[!(naturals %in% multiples)]

  return(naturals)
}


#################################################################
## C. Alternative Route to determine primality for any integer
#################################################################

prime <- function(a,p){
  a = as.bigz(a) ## a cannot be divisible by p
  p = as.bigz(p)
  if(a < 0 || p < 0){ ##a & p must be nonnegative
    stop("Argument must be a nonnegative integer")
  }
  
  Isprime <- (a^(p-1))%%p
  if(!Isprime %in% c(0,1)){
    print(sprintf("The integer is a composite number"))
  } else if(Isprime == 0){
    print(sprintf("Warning:`a` cannot be a factor of `p`"))
  } else {
    print(sprintf("The integer is a probable prime base"))
  }
    
  return(Isprime)
}

##################################################################
## D. Find all the prime factors of any given integer
##################################################################

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
##################################################################
## E. Count all factors of any given integer
##################################################################

factor_count <- function(num){
  if(num > 9e+15)
    stop("Argument must not be larger than 1e+15")
  if(num < 5 ||!num == round(num)|| num < 0||!length(num)==1)
    stop("Argument must be nonnegative integer greater than five")
  result <-factorize(num)
  encode <-rle(result)
  exponents <- ifelse(encode$lengths >=1,paste0(encode$values,"^",encode$lengths),result)
  exp_val <-sapply(strsplit(exponents,"\\^"), function(i) as.numeric(i[2]))
  addone_exp <- exp_val +1
  factor_num <- prod(addone_exp)
  return(factor_num)
}

#################################################################
## F. Find factors of any given integer
#################################################################

alfactor <- function(limit){
  if(limit >=9e+15)
    stop("Argument must not be greater than 9e+15")
  if(limit <= 5 ||!limit == round(limit)|| limit < 0||!length(limit)==1)
    stop("Argument must be nonnegative integer greater than five")
  
  vector <- 1:floor(sqrt(limit))
  prime_facts <- factorize(limit)
  products <- outer(prime_facts,vector,"*")
  factorz <- unique(c(products,prime_facts,vector))
  facts <- which(limit%%factorz==0)
  div <- limit/(factorz[facts])
  factorz <-unique(c(factorz[facts],div))
  return(sort(factorz))
}