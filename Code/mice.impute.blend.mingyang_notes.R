rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)
library(purrr)


mice.impute.blended <- function(y, ry, x, p, wy = NULL, donors = 5L,
                                matchtype = 1L, ridge = 1e-05, ...) {
  
  if (is.null(wy)) {
    wy <- !ry
  }
  x <- cbind(1, as.matrix(x))
  ynum <- y
  if (is.factor(y)) {
    ynum <- as.integer(y)
  }
  parm <- .norm.draw(ynum, ry, x, ridge = ridge)
  
  if (matchtype == 0L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$coef
  }
  if (matchtype == 1L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$coef
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  if (matchtype == 2L) {
    yhatobs <- x[ry, , drop = FALSE] %*% parm$beta
    yhatmis <- x[wy, , drop = FALSE] %*% parm$beta
  }
  
  #calculate the mahalanobis distance
  Sx <- cov(x[ry, -1 , drop = FALSE])
  maha.dis.mis <- apply(x[wy, -1 , drop = FALSE], 1, 
                        function(z) mahalanobis(x[ry, -1 , drop = FALSE], z, Sx))
  
  maha.dis.mis.rank <- apply(maha.dis.mis, 2, 
                             function(x) rank(x, ties.method  = "random"))
  
  predict.match <- function(z){
    d <- abs(yhatobs - z)
    f <- d > 0
    a1 <- ifelse(any(f), min(d[f]), 1)
    d <- d + runif(length(d), 0, a1 / 10^10)
    return(d)
  }
  
  predict.dis <- map(yhatmis, predict.match) %>% unlist %>% 
    matrix(nrow = length(yhatobs), ncol = length(yhatmis))
  
  predict.dis.rank <- apply(predict.dis, 2, 
                            function(x) rank(x, ties.method  = "random"))
  
  blend.dis <- p * predict.dis.rank + (1 - p) * maha.dis.mis.rank
  
  
  donor.select <- function(z){
    if (donors == 1) {
      return(y[which.min(z)])
    }
    donors <- min(donors, length(z))
    donors <- max(donors, 1)
    y.obs<-y[ry]
    ds <- sort.int(z, partial = donors) 
    m <- sample(y.obs[z <= ds[donors]], 1)
  }
  
  y[wy] <- apply(blend.dis, 2, donor.select)
  x <- x[, -1]
  return(as.data.frame(cbind(y, x)))
  
}  

# set the characteristics for the data to be simulated
# set sample size 500?
# and then missingness should be 0.002 to simulate that we are predicting the height for just one 
# target child? 
# do we make sure that the missingness is in the specifically in the outcome variable, or does it not matter?
# correlation will be varied 0.1 vs 0.7
set.seed(123)
sample.size <- 200
missingness <- 0.3
simulations <- 1000
correlation.coef <- 0.8

#data generation function
# dimensionality will be varied 2 vs 5
# distribution will be varied normal vs skewed vs strongly skewed
create.data <- function(rho, n) {
  mu.x <- c(2, 2, 2)
  sigma.x <- matrix(data = c(12, 0, 0, 0, 12, 0, 0, 0, 12), nrow = 3, byrow = T)
  X <- mvrnorm(n, mu = mu.x, Sigma = sigma.x)
  return(X)
}


#output project
OUT <- list()

#simulation
pb <- txtProgressBar(min = 0, max = simulations, style = 3)
for (i in 1 : simulations) {
  #data generation
  complete.data <- create.data(correlation.coef, sample.size)
  #ampute
  incomplete.data <- ampute(complete.data, prop = missingness, mech = "MAR", 
                            type = 'RIGHT', patterns = c(0, 1, 1),
                            weights = c(0, 1, 0))$amp
  colnames(incomplete.data) <- c("X1", "X2", "X3")
  #impute
  multiple.impute <- list()
  for (j in 1 : 5) {
    # weight will be varied 1 vs 0.75 vs 0.5 vs 0.25 vs 0
    imp <- mice.impute.blended(incomplete.data$X1, !is.na(incomplete.data$X1), 
                        incomplete.data[, c("X2", "X3")], p = 1)
    colnames(imp) <- c("X1", "X2", "X3")
    multiple.impute[[j]] <- imp
  }
  
  OUT[[i]] <- multiple.impute
  setTxtProgressBar(pb, i)
}
close(pb)

# Evaluate E(X1)
# adjust this function to only calculate the performance measures of coverage, ciw, bias
# should a similarity measure between the trajectories of the matches and the target be calculated
# as well, e.g. the sum of squared differences?
evaluate.sims <- function(sim, truth = 2){
  POOL <- list()
  pb <- txtProgressBar(min = 0, max = simulations, style = 3)
  for (i in 1:length(sim)){
    #Extract means and variances
    Q            <- c(mean(sim[[i]][[1]]$X1), mean(sim[[i]][[2]]$X1), mean(sim[[i]][[3]]$X1),
                      mean(sim[[i]][[4]]$X1), mean(sim[[i]][[5]]$X1))
    U            <- c(var(sim[[i]][[1]]$X1), var(sim[[i]][[2]]$X1), var(sim[[i]][[3]]$X1),
                      var(sim[[i]][[4]]$X1), var(sim[[i]][[5]]$X1)) / sample.size
    #Pool the regular way
    pool         <- mice::pool.scalar(Q, U, n = 1000) # A really large number
    pool$lower   <- pool$qbar - qt(0.975, pool$df) * sqrt(pool$t)
    pool$upper   <- pool$qbar + qt(0.975, pool$df) * sqrt(pool$t)
    pool$coverage <- pool$lower < mean(truth) & mean(truth) < pool$upper
    POOL[[i]]     <- unlist(pool)
    setTxtProgressBar(pb, i)
  }
  return(POOL)
  close(pb)
}
EVAL <- evaluate.sims(OUT)

# Summarize
AVG.EVAL <- Reduce("+", EVAL) / length(EVAL)
round(AVG.EVAL, 3)

