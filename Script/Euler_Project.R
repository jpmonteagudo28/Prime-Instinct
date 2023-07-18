############################################################
## Problem 1. Multiples of 3 or 5
############################################################

mult3 <- find_multiples(3,1000) ## function in Prime_finder
mult5 <- find_multiples(5,1000)
mult15 <- find_multiples(15,1000)
(sum(mult3) +sum(mult5)) - sum(mult15)

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

#############################################################
## Problem 7. 10001st Prime
#############################################################

## We do not know what answer to expect so we can try to solve 
## this problem using trial division. However, if a good upper 
## bound for the target prime is known in advance, using a
## sieve of Eratosthenes is a much more efficient method.

primect <- function(num,term){
  count <- num/log(num)
  target <- as.numeric(term)
  if(count < 0|| !term == round(term)||!length(term) == 1
     || !length(num)==1|| !num == round(num))
    stop("Argument must be a nonnegative integer")
  if(count >= target){
    primes <- sieve(num) ## sieve function found in Prime_Finder script
    end <- primes[target]
    return(end)
  }
  while(count < target){
    num <- num + 1000
    count <- num/log(num)
  }
  primes <-sieve(num)
  goal <- primes[target]
  return(goal)
}

#############################################################
## Problem 8. Largest Product in a series
#############################################################

## Process of figuring out how to work out solution.

thnd_dgt <- "73766203193809456599982445949435627061939786100117250547173286503262376022458008465094333630120854338003194362163007597987225472483598640843335685441710193966274131338557192586399006789292714554767500194796127964596906605976605873665859580600161998556511368530960400907199253450604168622770350228527124626728538626805418833470107651091641919900725415994689920112219170907023561354484047025713734651608777544579846111001059482132180956689444108315785401642188044178788629853592228467331730519810763559577944882016286493908631503101121166109571682295769470379514531105239965209245314082665518579335511291525230373316486697786532335206274149240813489201828773854353041855598709390675430960381072270432383913542702130202430186637321862331068861776780211082856984506050024895394320139435868484643843368002496089956046419964019877586845530207748994394501505588146979082629871366088121763790555364513243984244004147636040219136443410377798011608722717131323621700159335786445601947601694025107888293017058178562647175461026384343438874861406516767158373279032321096262126551620255666605185789463207944391905756886829667520553014724372245300878786091700563444079107099009003380230356461989260377273986023281444076082783406824471703499844642915587790146384758051663547775336021829171033411043796977042190519657861762804226147480755555085278062866268677842432851421790544407006581148631979148571299417963950579210719961422405768071335213324842709316205032078384168750091017964584060285240107161561019930505687950233196051962261970932008838279760834318101044311710769457048672103958655016388894770892065267451228938951370237422841366052736174160431593023473217066764172949768821843606479073866252864377064398085101223216558344281956767163876579889759124956035672317578122141070933058555310274598884089982879647974020264495921703064439532898207943134374576254840272047075633856749514044298135927611328433323640657533550512376900773273703275329924651465759145114579174356770593439987135755889403613364529029604049868233807295134382284730745937309910703657676103447124097631074153287120040247837143656624045055614076111832245239612708339272798262887437416818440064925049838443370805645609424314780108030016683461562597569371539974003402697903023830108053034645133078208043917492087248958344081026378788915528519967248989338592027124423914083391771884524464968645052058218151010508471258285907685355807229880747677634789376"
pow <- as.bigz(substr(thnd_dgt,1,13))
split_pow <- strsplit(as.character(pow), split ="")[[1]]
split_pow <- paste(split_pow,collapse =",")
vec_pow <- strsplit(split_pow, split =",")[[1]]
vec_pow <- as.numeric(vec_pow)
prod(vec_pow)


## Largest Product in a Series function

maxprod_seq <- function(num){
  split_seq <- strsplit(as.character(num), split ="")[[1]]
  vec_seq <- as.numeric(split_seq)
  max_prod <- 0
  for(i in 1:length(vec_seq) - 12){
    prodct <- prod(vec_seq[i:(i+12)])
    if(prodct > max_prod){
      max_prod <- prodct
    }
  }
  return(max_prod)
}

## Alternative provided by AI
maxprod_seq <- function(vec_seq) {
  vec_seq <- as.numeric(vec_seq)
  if (length(vec_seq) < 13) {
    stop("Sequence must have at least 13 elements.")
  }
  products <- sapply(1:(length(vec_seq) - 12), function(i) prod(vec_seq[i:(i + 12)]))
  max_prod <- max(products)
  return(max_prod)
}

#############################################################
## Problem 9. Special Pythagorean Triplet
#############################################################

## a == m^2 - n^2, m & n are co-primes and one of them is odd
## b = 2m*n
## c == m^2 + n^2
## a+b+c == 1000
## ------- or---------- ##
## a = 3n, b = 4n, c = 5n
## We could iterate to find out which a+b+c == 1000
## By trial and error, we can figure out 80 < n < 85

triplets <- function(limit) {
  for (m in 2:limit) {
    for (n in 1:(m - 1)) {
      a <- m^2 - n^2
      b <- 2 * m * n
      c <- m^2 + n^2
      if (a + b + c == limit) {
        return(c(a, b, c))
      }
    }
  }
  return(NULL)
}

##. Product of a * b * c
tri_prod <- function(limit){
  prodct <- prod(triplets(limit))
  return(prodct)
}

##############################################################
## Problem 10. Summation of Primes
##############################################################

## Using Prime_Finder's sieve function to find primes < 2e+6
 primesum <- function(limit){
   primo <- sieve(limit)
   summ <- sum(primo)
   return(summ)
 }

############### END OF FIRST CODING DECATHLON ################



