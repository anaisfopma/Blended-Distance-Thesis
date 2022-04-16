#################
# SIMULATION II #
#################

# load packages
library(mvtnorm) # data generation
library(dplyr) # mutate
library(magrittr) # %>% pipe operator
library(purrr) # mapping
library(mice) # amputation of data and imputation with pmm
library(future) # parallel processing
library(furrr) # apply mapping functions in parallel using futures 
library(tibble) # column_to_rownames function in evaluate.function


# set simulation parameters
set.seed(123)
n = 500                   
nsim = 1000
n.imp = 50
rho = 0.7                     


############################
# DATA GENERATING FUNCTION #
############################

# skewed data generation
gen_data <- function(n) {
  out <- rmvnorm(n = n,
                 sigma = matrix(c(1, rho, rho, rho, 1, rho, rho, rho, 1), 
                                nrow = 3, 
                                ncol = 3), 
                 mean = c(10, 10, 10))
  colnames(out) <- c("x1", "x2", "x3")
  out %>% 
    apply(., MARGIN = 2, function(x) {x^12/max(x^11)}) %>%
    as_tibble %>% 
    mutate(y = x1 + x2 + x3 + rnorm(n, mean = 0, sd = 7))
}

# generate true reference data set
data <- gen_data(n)

# vector of rows to be made missing
mis.indic <- sample(1:nrow(data), size = nsim, replace = TRUE)

# create list of incomplete versions where only one value is made 
# missing based on mis.indic
gen_mis <- function(x) {
  dta <- data
  dta[x, "y"] <- NA
  return(dta)
}
mis_data <- lapply(X = mis.indic, 
                   FUN = gen_mis)

# comparitive truths (the y's that are made missing)
true.y <- lapply(mis.indic, function(x){data$y[x]})
  


# run imputation
plan(multisession) # increase speed through futures



###########
# METHODS #
###########

# PMM
imp.pmm <- mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "pmm", 
                         maxit = 1, 
                         print = FALSE, 
                         m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 1
imp.blend.pmm.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
                         blend = 1, 
                         maxit = 1, 
                         print = FALSE, 
                         m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.9
imp.blend0.9.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.9, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.8
imp.blend0.8.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.8, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.7
imp.blend0.7.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.7, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.6
imp.blend0.6.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.6, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.5
imp.blendhalf.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
                         blend = 0.5, 
                         maxit = 1, 
                         print = FALSE, 
                         m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.4
imp.blend0.4.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.4, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.3
imp.blend0.3.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.3, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.2
imp.blend0.2.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.2, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.1
imp.blend0.1.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
               blend = 0.1, 
               maxit = 1, 
               print = FALSE, 
               m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0
imp.mahalan.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% mice(meth = "blended", 
                         blend = 0, 
                         maxit = 1, 
                         print = FALSE, 
                         m = n.imp)
  }, .options = furrr_options(seed = as.integer(123)))

plan(sequential)



##########
# OUTPUT #
##########

# save the relevant output
out <- list(imp.pmm = imp.pmm,
            imp.blend.pmm.rank = imp.blend.pmm.rank,
            imp.blend0.9.rank = imp.blend0.9.rank,
            imp.blend0.8.rank = imp.blend0.8.rank,
            imp.blend0.7.rank = imp.blend0.7.rank,
            imp.blend0.6.rank = imp.blend0.6.rank,
            imp.blendhalf.rank = imp.blendhalf.rank,
            imp.blend0.4.rank = imp.blend0.4.rank, 
            imp.blend0.3.rank = imp.blend0.3.rank,
            imp.blend0.2.rank = imp.blend0.2.rank,
            imp.blend0.1.rank = imp.blend0.1.rank,
            imp.mahalan.rank = imp.mahalan.rank)

# load evaluation function
source("evaluate.function.R")

# evaluate the output
eval <- map(out, eval_sim2)
sapply(eval, colMeans) %>% t()
# plot eval mahalanobis estimate distribution vs pmm estimate distribution

# remove everything except output and evaluation
rm(list=setdiff(ls(), c("out", "eval", "eval_sims")))

# save workspace
save.image("Workspaces/Simulation_II.RData")
