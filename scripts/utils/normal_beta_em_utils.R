#-------------------------------------------------------------------------------
# FUNCTIONS NEEDED TO IMPLEMENT EM ALGORITHM USED FOR THRESHOLDING IN THE
#   ANALYSIS OF THE T CELL POLYFUNCTIONALITY DATA 
#-------------------------------------------------------------------------------

library(magrittr)
library(purrr)
library(tidyr)
library(mclust)
library(dplyr)

# normalizes log probabilities on log scale for numerical stability
logsumexp <- function(lp) {
  y <- max(lp)
  return(log(sum(exp(lp - y))) + y)
}

# log likelihood of the mixture
# the weird transformations of the beta part are to deal with the -Inf values
#   for aggregate compositions <= 0, which will have non-infinite density in
#   the normal part
loglikelihood <- function(x, prop0, mu, sigma, shape1, shape2) {
  beta_part <- (1-prop0) * dbeta(x, shape1, shape2, log = FALSE)
  beta_part[beta_part == Inf] <- max(beta_part[beta_part != Inf])
  sum(log(prop0 * dnorm(x, mean = mu, sd = sigma, log = FALSE) +  beta_part))
}

# Same as above, but set up to work with optimization function over shape1 and shape2
optim_neg_loglikelihood <- function(param, x, prop0, mu, sigma) {
  shape1 <- exp(param[1])
  shape2 <- exp(param[2])
  beta_part <- (1-prop0) * dbeta(x, shape1, shape2, log = FALSE)
  beta_part[beta_part == Inf] <- max(beta_part[beta_part != Inf])
  -sum(log(prop0 * dnorm(x, mean = mu, sd = sigma, log = FALSE) +  beta_part))
}

# Fit a beta normal mixture using the expectation-maximization algorithm
# tol is to identify when algorithm has converged
# If mu_equal_zero = TRUE, we force the mean of the normal part to be 0, to be
#   in line with SPICE paper's suggestions. Otherwise, it is estimated.
fit_beta_normal_mixture <- function(x, itermax = 30, tol = 1E-10, mu_equal_zero = TRUE) {
  if(any(x > 1)) {
    stop("Background subtracted compositional data must all be less than 1. Check x.")
  }
  
  # sample size
  n <- length(x)
  
  # initialize starting values--------------------------------------------------
  
  # Normal part using Mclust
  init <- mclust::Mclust(x, G = 2)
  if(mu_equal_zero) {
    init_mu <- 0  
  } else {
    init_mu <- min(init$parameters$mean)
  }
  init_sigma <- sqrt(init$parameters$variance$sigmasq[which.min(init$parameters$mean)])
  
  # Mixing weights using Mclust
  prop0 <- init$parameters$pro[which.min(init$parameters$mean)]

  # Beta part using method of moments 
  betax <- x[(init$classification == which.max(init$parameters$mean))]
  shape1 <- max((mean(betax) * (1-mean(betax)) / var(betax) - 1) * mean(betax), .1)
  shape2 <- max((mean(betax) * (1-mean(betax)) / var(betax) - 1) * (1 - mean(betax)), .1)
  
  
  
  ll_old <- -Inf
  
  # E step ---------------------------------------------------------------------
  z <- matrix(NA, nrow = n, ncol = 2)
  colnames(z) <- c("zero", "non-zero")
  
  z[, "zero"] <-
    log(prop0) + dnorm(x, mean = init_mu, sd = init_sigma, log = TRUE)
  z[, "non-zero"] <-
    log(1 - prop0) + dbeta(
      x,
      shape1 = shape1,
      shape2 = shape2,
      log = TRUE
    )
  z[,"non-zero"][z[,"non-zero"] == Inf] <- max(z[,"non-zero"][z[,"non-zero"] != Inf])
  
  denom <- apply(z, 1, logsumexp)
  z <-  sweep(z, 1, denom, `-`)
  
  # Run EM---------
  for(step in 1:itermax) {
    # M step ---------------------------------------------------------------------
    expz <- exp(z)
    
    # Mixing weights---------
    prop0 <- mean(expz[,"zero"])
    
    # Normal component---------
    # mu
    if(mu_equal_zero) {
      mu <- 0
    } else {
      mu <- sum(expz[,"zero"] * x) / sum(expz[,"zero"])  
    }
    
    # sigma
    sigma <- sqrt(sum(expz[,"zero"] * (x - mu)^2) / sum(expz[,"zero"]))
    
    # Beta component---------
    # numerically optimize shape parameters
    opt <- optim(
      log(c(shape1, shape2)),
      fn = optim_neg_loglikelihood,
      x = x,
      prop0 = prop0,
      mu = mu,
      sigma = sigma,
      method = "Nelder-Mead"
    )
    shape1 <- exp(opt$par[1])
    shape2 <- exp(opt$par[2])
    
    # Check convergence --------------------------------------------------------
    ll_new <- loglikelihood(x, prop0, mu, sigma, shape1, shape2)
    if(abs(ll_new - ll_old) < tol) {
      break;
    } else {
      ll_old <- ll_new
    }
    
    # E step ---------------------------------------------------------------------
    z[, "zero"] <-
      log(prop0) + dnorm(x, mean = mu, sd = sigma, log = TRUE)
    z[, "non-zero"] <-
      log(1 - prop0) + dbeta(
        x,
        shape1 = shape1,
        shape2 = shape2,
        log = TRUE
      )
    z[,"non-zero"][z[,"non-zero"] == Inf] <- max(z[,"non-zero"][z[,"non-zero"] != Inf])
    denom <- apply(z, 1, logsumexp)
    z <-  sweep(z, 1, denom, `-`)
  }
  
  list(
    parameters = list("prop0" = prop0, "mu" = mu, "sigma" = sigma, "shape1" = unname(shape1), "shape2" = unname(shape2)),
    loglikelihood = ll_old,
    z = exp(z)
  )
}
