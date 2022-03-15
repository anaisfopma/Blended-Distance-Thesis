rm(list=ls())
library(mice, warn.conflicts = FALSE)
library(miceadds)
library(MASS)
library(magrittr)
library(ggplot2)
library(dplyr)
library(purrr)
library(future)
library(furrr)

###########################
# DEFINE BLENDED FUNCTION #
###########################

mice.impute.blended <- function(y, ry, x, wy = NULL,
                                blend, 
                                donors = 5L,
                                matchtype = 1L, 
                                ridge = 1e-05, rank = TRUE,
                                ...) {

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
  
  Sx <- cov(x[ry, -1 , drop = FALSE])
  maha.dis <- apply(x[wy, -1 , drop = FALSE], 1, 
                        function(z) mahalanobis(x[ry, -1 , drop = FALSE], z, Sx))
  
  predict.match <- function(z){
    d <- abs(yhatobs - z)
    f <- d > 0
    a1 <- ifelse(any(f), min(d[f]), 1)
    d <- d + runif(length(d), 0, a1 / 10^10)
    return(d)
  }
  
  predict.dis <- map(yhatmis, predict.match) %>% 
    unlist %>% 
    matrix(nrow = length(yhatobs), 
           ncol = length(yhatmis))
  
  if (rank){ 
    pd.rank <- apply(predict.dis, 2, function(x) rank(x, ties.method  = "random"))
    mh.rank <- apply(maha.dis, 2, function(x) rank(x, ties.method  = "random"))
    blend.dis <- blend * pd.rank + (1 - blend) * mh.rank
  } else {
    blend.dis <- blend * scale(predict.dis) + (1 - blend) * scale(maha.dis)
    }

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

############################
# DATA GENERATING FUNCTION #
############################

# this should be adjusted to make the outcome have more variance, but I don't know how to do that
create.data <- function(rho, n, normal = TRUE) {
  mu.x <- c(2, 2, 2)
  sigma.x <- matrix(data = c(12, 0, 0, 0, 12, 0, 0, 0, 12), nrow = 3, byrow = T)
  X <- mvrnorm(n, mu = mu.x, Sigma = sigma.x)
  # should skewness be done like this?
  if (normal){X <- X} else {X <- X^12/max(X^11)}
  return(X)
}

##############
# EVALUATION #
##############

# Evaluate E(X1)
# should all performance measures calculated here be used? or just the ciw and cov for example? 
evaluate.sims <- function(sim, truth = 2){
  POOL <- list()
  pb <- txtProgressBar(min = 0, max = simulations, style = 3)
  for (i in 1:length(sim)){
    Q            <- c(mean(sim[[i]][[1]]$X1), mean(sim[[i]][[2]]$X1), mean(sim[[i]][[3]]$X1),
                      mean(sim[[i]][[4]]$X1), mean(sim[[i]][[5]]$X1))
    U            <- c(var(sim[[i]][[1]]$X1), var(sim[[i]][[2]]$X1), var(sim[[i]][[3]]$X1),
                      var(sim[[i]][[4]]$X1), var(sim[[i]][[5]]$X1)) / sample.size
    pool         <- mice::pool.scalar(Q, U, n = 1000)
    pool$lower   <- pool$qbar - qt(0.975, pool$df) * sqrt(pool$t)
    pool$upper   <- pool$qbar + qt(0.975, pool$df) * sqrt(pool$t)
    pool$coverage <- pool$lower < mean(truth) & mean(truth) < pool$upper
    POOL[[i]]     <- unlist(pool)
    setTxtProgressBar(pb, i)
  }
  return(POOL)
  close(pb)
}


##################
# FUTURE FOR PMM #
##################

# add the same simulation here but with mice.impute.pmm function instead of blended function



#############################
# FUTURE FOR BLENDED METRIC # 
#############################

plan(multisession)

# as you can see, I don't know how to use the future function
# in the example from synthemice, the future_map function is applied to a dataset, but how
# should it be done here, as the data is generated within the function?
simulation <- future(function(sample.size = 500, missingness, simulations = 1000, correlation.coef, 
                                  mechanism, mechanismtype, normal, rank, rankingfactor) {
  #output project
  OUT <- list()
  
  #simulation
  pb <- txtProgressBar(min = 0, max = simulations, style = 3)
  for (i in 1 : simulations) {
    #data generation
    complete.data <- create.data(correlation.coef, sample.size, normal = normal)
    #ampute
    incomplete.data <- ampute(complete.data, prop = missingness, mech = mechanism, 
                              type = mechanismtype, patterns = c(0, 1, 1),
                              weights = c(0, 1, 0))$amp
    colnames(incomplete.data) <- c("X1", "X2", "X3")
    #impute
    multiple.impute <- list()
    for (j in 1 : 5) {
      imp <- mice.impute.blended(incomplete.data$X1, !is.na(incomplete.data$X1), 
                                 incomplete.data[, c("X2", "X3")], rank=rank, blend = rankingfactor)
      colnames(imp) <- c("X1", "X2", "X3")
      multiple.impute[[j]] <- imp
    }
    
    OUT[[i]] <- multiple.impute
    setTxtProgressBar(pb, i)
  }
  close(pb)

}, .options = furrr_options(seed = as.integer(123)))


##################################################################################
# DGM 1: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0 #
##################################################################################

simulation <- future(missingness = 0.25, correlation.coef = 0, mechanism = "MCAR", 
                     normal = TRUE, rank = TRUE, rankingfactor = 1)
  
EVAL <- evaluate.sims(OUT)
AVG.EVAL <- Reduce("+", EVAL) / length(EVAL)
round(AVG.EVAL, 3)
# should this be included within the future?



# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



####################################################################################
# DGM 2: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0.1 #
####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



####################################################################################
# DGM 3: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0.7 #
####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



##################################################################################
# DGM 4: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0 #
##################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



####################################################################################
# DGM 5: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0.1 #
####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



####################################################################################
# DGM 6: MISSING = 25%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0.7 #
####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



######################################################################################
# DGM 7: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0 #
######################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



########################################################################################
# DGM 8: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0.1 #
########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



########################################################################################
# DGM 9: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0.7 #
########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#######################################################################################
# DGM 10: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0 #
#######################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 11: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0.1 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 12: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0.7 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



###################################################################################
# DGM 13: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0 #
###################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#####################################################################################
# DGM 14: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0.1 #
#####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#####################################################################################
# DGM 15: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = NORMAL, CORRELATION = 0.7 #
#####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



###################################################################################
# DGM 16: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0 #
###################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#####################################################################################
# DGM 17: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0.1 #
#####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#####################################################################################
# DGM 18: MISSING = 50%, MECHANISM = MCAR, DISTRIBUTION = SKEWED, CORRELATION = 0.7 #
#####################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#######################################################################################
# DGM 19: MISSING = 50%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0 #
#######################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 20: MISSING = 50%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0.1 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 21: MISSING = 50%, MECHANISM = MARRIGHT, DISTRIBUTION = NORMAL, CORRELATION = 0.7 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#######################################################################################
# DGM 22: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0 #
#######################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 23: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0.1 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0



#########################################################################################
# DGM 24: MISSING = 25%, MECHANISM = MARRIGHT, DISTRIBUTION = SKEWED, CORRELATION = 0.7 #
#########################################################################################
# PMM
# RANKED, BLENDING FACTOR = 1
# RANKED, BLENDING FACTOR = 0.5
# RANKED, BLENDING FACTOR = 0
# SCALED, BLENDING FACTOR = 1
# SCALED, BLENDING FACTOR = 0.5
# SCALED, BLENDING FACTOR = 0





