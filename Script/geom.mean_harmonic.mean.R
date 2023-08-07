#---------------------------------------------------------------------#
# Geometric mean                                                      #
#---------------------------------------------------------------------#

geom.mean <- function(x, ..., na.rm = FALSE) {
  if (is.data.frame(x) || is.matrix(x)) {
    if (na.rm) {
      x <- apply(x, 2, function(column) column[!is.na(column)])
    }
    geom_mean <- apply(x, 2, function(column) {
      g.mean <- exp(mean(log(column)))
    })
    return(geom_mean)
  } else {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    g.mean <- exp(mean(log(x)))
    return(g.mean)
  }
}

#---------------------------------------------------------------------#
# Harmonic mean                                                       #
#---------------------------------------------------------------------#

harmonic.mean <- function(x, ..., na.rm = FALSE) {
  if (is.data.frame(x) || is.matrix(x)) {
    if (na.rm) {
      x <- apply(x, 2, function(column) column[!is.na(column)])
    }
    harm_mean <- apply(x, 2, function(column) {
      n <- length(column)
      inv.sum <- sum(column^-1)
      harm.mean <- n / inv.sum
      return(harm.mean)
    })
    return(harm_mean)
  } else {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    inv.sum <- sum(x^-1)
    harm.mean <- n / inv.sum
    return(harm.mean)
  }
}

#---------------------------------------------------------------------#
# Quadratic mean (Root Mean Square - RMS)                             #
#---------------------------------------------------------------------#

quad.mean <- function(x, ..., na.rm = FALSE) {
  if (is.data.frame(x) || is.matrix(x)) {
    if (na.rm) {
      x <- apply(x, 2, function(column) column[!is.na(column)])
    }

    quad_means <- apply(x, 2, function(column) {
      n <- length(column)
      square.sum <- sum(column^2)
      sqrt.mean <- sqrt(square.sum / n)
      return(sqrt.mean)
    })

    return(quad_means)
  } else {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    square.sum <- sum(x^2)
    sqrt.mean <- sqrt(square.sum / n)
    return(sqrt.mean)
  }
}

# If a non-empty vector contains equal values, the arithmetic, harmonic 
# and geometric mean will all yield the same result. However, when a 
# non-empty vector of unequal values is used the geometric mean and 
# harmonic mean will prove to be more robust in the presence of outliers 
# and in non-normal distributions.

# We know that min(x) <= harmonic.mean(x) <= geom.mean(x) <= mean(x) <= quad.mean <= sum(x^2)/sum(x) <= max(x)
# for a non-empty vector containing unequal values. 

