# load packages
library(faux)
library(tidyverse)
library(magrittr)


######################
# CODE FOR 1 DATASET #
######################

# simulate 1 dataset with 1000 observations of standardised height measurements at 
# birth, 1, 2, 3, 6, and 14 months, mean = 0, sd = 1, correlation = 0.7
dat <- rnorm_multi(1000, 6, 0, 1, 0.7, varnames = c("hgt_z_birth", "hgt_z_1mo", "hgt_z_2mo", "hgt_z_3mo", "hgt_z_6mo", "hgt_z_14mo"))

# add id's to this dataset
dat <- tibble::rowid_to_column(dat, "ID")

# select data of the donors
donor_data <- dat[2:1000, ]

# select data of the target
target_data <- dat[1, ]

# fit model on the donor data 
model <- lm(`hgt_z_14mo` ~ `hgt_z_birth` + `hgt_z_1mo` + `hgt_z_2mo` + `hgt_z_3mo` + `hgt_z_6mo`, data = donor_data)

# make predictions for the donors and the target 
donors_pred <- predict(model)
target_pred <- predict(model, newdata = target_data)

# calculate the predictive distance between the target and each of the donors
donor_data$PD <- abs(donors_pred - target_pred)

# assign a rank to each donor for the predictive distance
donor_data$PD_rank <- rank(donor_data$PD, ties.method = "random")

# select the values for the first 6 months for the donors and the target
donors_6m <- donor_data[ , 2:6]
target_6m <- target_data[ , 2:6]

# calculate the Mahalanobis distance for each of the donors 
donor_data$MD <- sqrt(mahalanobis(donors_6m, as.numeric(target_6m), cov(donors_6m)))
# Q: Is it correct to use target_6m as the center here?

# assign a rank to each donor for the Mahalanobis distance
donor_data$MD_rank <- rank(donor_data$MD, ties.method = "random")

# calculate blended distance with weight = 0.5
donor_data$BD <- 0.5 * donor_data$PD_rank + (1-0.5) * donor_data$MD_rank

# select k closest matches 
donors_ranked <- arrange(donor_data, BD)
matches <- donors_ranked[1:5, ]

# calculate SS
SS <- matches[, 2:6] %>% # select data for first 6 months of the matches
  sweep(2, as.numeric(target_6m)) %>% # subtract data for first 6 months of the target
  apply(2, function(x) x^2) %>% # square the differences
  apply(1, function(x) sum(x)) %>% # take the sum of squares for each match
  sum() # take the sum of the 5 matches
SS

# calculate squared error
matches_pred <- mean(matches[ , 7]) # height at 14 months predicted by the matches
target_true <- target_data[ , 7] # actual height at 14 months
SE <- (matches_pred - target_true)^2 # difference between them
SE
# Q: Should the height at 14 months predicted by the matches be the mean of the heights of the matches at 14 months?


######################
# CODE AS A FUNCTION #
######################

# r = the correlation for the simulated dataset, k = number of matches, p = weight assigned to predictive distance
BDfunction <- function(r, k, p){
  
  # simulate 1 dataset
  dat <- rnorm_multi(1000, 6, 0, 1, r, varnames = c("hgt_z_birth", "hgt_z_1mo", "hgt_z_2mo", "hgt_z_3mo", "hgt_z_6mo", "hgt_z_14mo"))
  
  # add id's to this dataset
  dat <- tibble::rowid_to_column(dat, "ID")
  
  # select data of the donors
  donor_data <- dat[2:1000, ]
  
  # select data of the target
  target_data <- dat[1, ]
  
  # fit model on the donor data 
  model <- lm(`hgt_z_14mo` ~ `hgt_z_birth` + `hgt_z_1mo` + `hgt_z_2mo` + `hgt_z_3mo` + `hgt_z_6mo`, data = donor_data)
  
  # make predictions for the donors and the target 
  donors_pred <- predict(model)
  target_pred <- predict(model, newdata = target_data)
  
  # calculate the predictive distance between the target and each of the donors
  donor_data$PD <- abs(donors_pred - target_pred)
  
  # assign a rank to each donor for the predictive distance
  donor_data$PD_rank <- rank(donor_data$PD, ties.method = "random")
  
  # select the values for the first 6 months for the donors and the target
  donors_6m <- donor_data[ , 2:6]
  target_6m <- target_data[ , 2:6]
  
  # calculate the Mahalanobis distance for each of the donors 
  donor_data$MD <- sqrt(mahalanobis(donors_6m, as.numeric(target_6m), cov(donors_6m)))
  
  # assign a rank to each donor for the Mahalanobis distance
  donor_data$MD_rank <- rank(donor_data$MD, ties.method = "random")
  
  # calculate blended distance with weighting
  donor_data$BD <- p * donor_data$PD_rank + (1-p) * donor_data$MD_rank
  
  # select k closest matches 
  donors_ranked <- arrange(donor_data, BD)
  matches <- donors_ranked[1:k, ]
  
  # calculate SS
  SS <- matches[, 2:6] %>%
    sweep(2, as.numeric(target_6m)) %>%
    apply(2, function(x) x^2) %>%
    apply(1, function(x) sum(x)) %>%
    sum()
  
  # calculate squared error for the RMSE
  matches_pred <- mean(matches[ , 7]) 
  target_true <- target_data[ , 7] 
  SE <- (matches_pred - target_true)^2 
  
  # define the output
  output <- list("SS" = SS, "SE" = SE, "matches_pred" = matches_pred, "target_true" = target_true)
  return(output)
  
}

###########################################
# EXAMPLE SIMULATION WITH 1000 ITERATIONS #
###########################################

# simulation with r = 0.7 and p = 0.5
set.seed(987)
res <- replicate(1000, BDfunction(0.7, 5, 0.5), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>% # select the SS of each iteration
  sum() # add them up
SS # = 2861.399
#Q: Here I take the sum of all the SS over the iterations, would it be nicer to take the mean instead?

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>% # select the squared error of each iteration
  sum() %>% # add them up
  `/`(length(res)) %>% # divide them by the number of iterations
  sqrt() # take the square root
RMSE # = 0.6694883

# R squared
predicted <- res %>%
  map_dbl(~(.x)$matches_pred) # select height at 14 months predicted by the 5 matches of each iteration 

true <- res %>%
  map_dbl(~(.x)$target_true) # select the actual height at 14 months of each iteration

Rsquared <- cor(predicted, true)^2 # calculate squared correlation between them
Rsquared # = 0.5472212
# Q: I have simulated data for donors and target at 14 months, 
# and I also predict the height at 14 months using the data of the first 6 months.
# I use the predicted values to calculate the predictive distance.
# But to calculate the RMSE, I use the mean of the simulated heights of the matches at 14 months 
# and the simulated (actual) height of the target at 14 months. Is this correct? 



# Q: Below are some results. I'm not sure if I calculate the performance measures correctly. 
# It seems that the more weight is given to the predictive distance, the higher the SS, as expected.
# But the RMSE and Rsquared don't seem to be influenced by the weighting that much, more so by
# the correlation in the data. Are the calculations correct, or is there a mistake somewhere else?

# Q: Does it make sense to use low correlations like 0.3 and 0.5 or should I only use correlations
# like 0.7, 0.8, 0.9, as real growth data is usually highly correlated?



##################################################
# SIMULATIONS + RESULTS WITH DIFFERENT WEIGHTING #
##################################################

# simulation with r = 0.7 and p = 0.1
set.seed(987)
res <- replicate(1000, BDfunction(0.7, 5, 0.1), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 2411.18

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.6686754

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.5443646



# simulation with r = 0.7 and p = 0.3
set.seed(987)
res <- replicate(1000, BDfunction(0.7, 5, 0.3), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 2534.555

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.6674264

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.548469



# simulation with r = 0.7 and p = 0.7
set.seed(987)
res <- replicate(1000, BDfunction(0.7, 5, 0.7), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 3360.995

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.6702328

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.548092



# simulation with r = 0.7 and p = 0.9
set.seed(987)
res <- replicate(1000, BDfunction(0.7, 5, 0.9), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 4483.628

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.6587468

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.5631043


#####################################################
# SIMULATIONS + RESULTS WITH DIFFERENT CORRELATIONS #
#####################################################

# simulation with r = 0.5 and p = 0.5
set.seed(987)
res <- replicate(1000, BDfunction(0.5, 5, 0.5), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 4510.485

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.8617325

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.3013509



# simulation with r = 0.6 and p = 0.5
set.seed(987)
res <- replicate(1000, BDfunction(0.6, 5, 0.5), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 3664.091

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.7656706

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.4276403



# simulation with r = 0.8 and p = 0.5
set.seed(987)
res <- replicate(1000, BDfunction(0.8, 5, 0.5), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 1976.697

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.5399845

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.7008675



# simulation with r = 0.9 and p = 0.5
set.seed(987)
res <- replicate(1000, BDfunction(0.9, 5, 0.5), simplify = FALSE)

# SS
SS <- res %>%
  map_dbl(~(.x)$SS) %>%
  sum() 
SS # = 1129.246

# RMSE
RMSE <- res %>%
  map_dbl(~(.x)$SE) %>%
  sum() %>% 
  `/`(length(res)) %>% 
  sqrt() 
RMSE # = 0.3878889

# R squared
Rsquared <- cor(map_dbl(res, ~(.x)$matches_pred), map_dbl(res, ~(.x)$target_true) )^2 
Rsquared # = 0.8392375
