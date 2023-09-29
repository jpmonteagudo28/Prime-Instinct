---
editor_options: 
  markdown: 
    wrap: 72
title: Prime Number Theory
---

------------------------------------------------------------------------



# The Sieve of Eratosthenes

![*AI generated rendering of Erastothenes'
sieve*](file:///C:/Users/jpmonteagudo/Downloads/Eratosthenes.png){width="45%"}

The Sieve of Eratosthenes is an ancient algorithm devised by the Greek
mathematician Eratosthenes of Cyrene around the 3rd century BCE.
Eratosthenes was the third librarian at Alexandria and a scholar who,
despite his contributions to various fields including mathematics,
geography, and astronomy, fell short of the highest rank:

```         
[Eratosthenes] was, indeed, recognised by his contemporaries as a man of great distinction in all branches of knowledge, though in each subject he just fell short of the highest place. On the latter ground he was called Beta, and another
nickname applied to him, Pentathlos, has the same implication, representing as it does an all-round athlete who was not the first runner or wrestler but took the second   prize in these contests as well as others.  
```

Among his contributions to the sciences, the motivation behind
Eratosthenes' development of the sieve algorithm was to efficiently find
prime numbers within a given range. Prime numbers have fascinated
mathematicians for centuries due to their unique properties and their
fundamental role in number theory.

According to historical accounts, Eratosthenes wanted to enumerate the
prime numbers up to a certain limit without having to manually test each
number for primality. Inspired by the concept of sieving used in
everyday life to separate grains from impurities, he devised a method
that involved systematically crossing out multiples of prime numbers to
identify the primes.

The Sieve of Eratosthenes algorithm provided an ingenious way to
generate prime numbers by iteratively sieving out composite numbers. By
starting with the first prime number and repeatedly marking its
multiples as composite, Eratosthenes was able to identify all the primes
up to a given limit effectively.

Eratosthenes' sieve was not only a remarkable mathematical
accomplishment but also a practical method that could be implemented
without complex calculations or advanced mathematical theories. Its
simplicity and efficiency made it a valuable tool in various
mathematical applications and laid the foundation for further
investigations into prime numbers and number theory.

To this day, the Sieve of Eratosthenes remains one of the most
well-known and widely used algorithms for finding prime numbers, even
after more than two thousand years since its inception.

![*Factoring the
Time*](https://imgs.xkcd.com/comics/factoring_the_time.png){width="53%"}

```{r echo=FALSE}
prp_num <- as.data.frame(read.delim("prp_number.txt",sep = "|", header = TRUE))
knitr::kable(prp_num, caption = "Table containing weak probable primes")
```
