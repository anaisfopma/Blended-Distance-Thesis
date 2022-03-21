##################################################################################
# DGM 4: MECHANISM = MCAR, MISSING = 25%, DISTRIBUTION = SKEWED, CORRELATION = 0 #
##################################################################################

# set simulation parameters
set.seed(123)
n = 500                     # fixed
nsim = 1000                 # fixed
mis = .25                   # variable (25% and 50%)
mech = "MCAR"               # variable (MCAR and MARright)
rho = 0                     # variable (rho = 0, rho = .1, rho = .7)



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

# generate list of data sets
data <- replicate(nsim, 
                  expr = gen_data(n), 
                  simplify = FALSE)

# make data missing
mis_data <- data %>% 
  map(~.x %>% ampute(prop = mis, 
                     mech = mech, 
                     pattern = c(1, 1, 1, 0))) 
# default type is set to right. so mech = "MAR" generates 
# right-tailed MAR missingness

# optional: remove data 
rm(data)

# run imputation
plan(multisession) # increase speed through futures



###########
# METHODS #
###########

# PMM
imp.pmm <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "pmm", 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 1
imp.blend.pmm.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 1, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0.5
imp.blendhalf.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = .5, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# RANKED, BLENDING FACTOR = 0
imp.mahalan.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 0, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# SCALED, BLENDING FACTOR = 1
imp.blend.pmm.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 1, 
                         maxit = 1,
                         rank = FALSE, # calculate as scale
                         print = FALSE) 
  }, .options = furrr_options(seed = as.integer(123)))

# SCALED, BLENDING FACTOR = 0.5
imp.blendhalf.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = .5, 
                         maxit = 1,
                         rank = FALSE, # calculate as scale
                         print = FALSE) 
  }, .options = furrr_options(seed = as.integer(123)))

# SCALED, BLENDING FACTOR = 0
imp.mahalan.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 0, 
                         maxit = 1,
                         rank = FALSE, # calculate as scale
                         print = FALSE) 
  }, .options = furrr_options(seed = as.integer(123)))

plan(sequential)



##########
# OUTPUT #
##########

# save the relevant output
out <- list(imp.pmm = imp.pmm,
            imp.blend.pmm.rank = imp.blend.pmm.rank,
            imp.blend.pmm.scale = imp.blend.pmm.scale,
            imp.blendhalf.rank = imp.blendhalf.rank,
            imp.blendhalf.scale = imp.blendhalf.scale,
            imp.mahalan.rank = imp.mahalan.rank,
            imp.mahalan.scale = imp.mahalan.scale)

# remove everything except output
rm(list=setdiff(ls(), "out"))

# evaluate the output
eval <- map(out, eval_sims)

# Save workspace
save.image("Workspaces/MCAR_25_skew_0.7.RData")
