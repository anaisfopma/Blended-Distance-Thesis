# What to compare on 500 cases with varying levels of missingness in your outcome (25%, 50%, perhaps if time permits 75%)  and according to MCAR and MAR mechanisms (MARleft, **MARright**, MARmid, MARtail) and correlation (0, .1, .7) and skewness (normal vs very skewed)
# 
# - pmm (reference because we like it)
# - blended pmm (blend factor as one --> this should be same as pmm)
# - one time rank
# - one time scale
# - blended pmm (blend as .5)
# - one time rank
# - one time scale
# - blended pmm (blend as zero)
# - one time rank
# - one time scale
# 
# On the above you can use the evaluation script from MY
# 
# --- 
#   
#   The above guides your practice to do the practical implementation.
# 
# 1. Now you remove a single value out of 500 and impute it >>m times (lets say 50). Evaluate it agains its original value. 
# 2. Make a subset in the 500 cases (let's say 25 cases) that together have similar predictive distances, but are dissimilar. and then you do (1), but you make sure that only on out of the 25 is removed.

# set up sim parameters
set.seed(123)
n = 500                     # fixed
nsim = 100                  # fixed
mis = .25 # 25% missingness # variable (25% and 50%)
mech = "MCAR"               # variable (MCAR and MARright)
rho = 0                     # variable (rho = 0, rho = .1, rho = .7 )
skewed = FALSE              # variable (skewed = FALSE (i.e. normal), skewed = TRUE)

# data generating function
gen_data <- function(n) {
  out <- rmvnorm(n = n,
                 sigma = matrix(c(1, rho, rho, rho, 1, rho, rho, rho, 1), 
                                nrow = 3, 
                                ncol = 3), 
                 mean = c(10, 10, 10))
  colnames(out) <- c("x1", "x2", "x3")
  out %>% 
    as_tibble %>% 
    mutate(y = x1 + x2 + x3 + rnorm(n, mean = 0, sd = 7))
  
  # ADJUSTMENT FOR WHEN SKEWED
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

# optional: remove data: 
# rm(data)

# run imputation
plan(multisession) # increase speed through futures

# - pmm (reference because we like it)
imp.pmm <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "pmm", 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# - blended pmm (blend factor as one --> this should be same as pmm)
# - one time rank
imp.blend.pmm.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 1, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# - one time scale
imp.blend.pmm.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 1, 
                         maxit = 1,
                         rank = FALSE, # calculate as scale
                         print = FALSE) 
  }, .options = furrr_options(seed = as.integer(123)))

# - blended pmm (blend as .5)
# - one time rank
imp.blendhalf.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = .5, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# - one time scale
imp.blendhalf.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = .5, 
                         maxit = 1,
                         rank = FALSE,
                         print = FALSE) # calculate as scale
  }, .options = furrr_options(seed = as.integer(123)))

# - blended pmm (blend as zero) --> equals mahalanobis
# - one time rank
imp.mahalan.rank <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 0, 
                         maxit = 1, 
                         print = FALSE)
  }, .options = furrr_options(seed = as.integer(123)))

# - one time scale
imp.mahalan.scale <- 
  mis_data %>%
  future_map(function(x){
    x %>% .$amp %>% mice(meth = "blended", 
                         blend = 0, 
                         maxit = 1,
                         rank = FALSE,
                         print = FALSE) # calculate as scale
  }, .options = furrr_options(seed = as.integer(123)))

plan(sequential)

# Save workspace
save.image("MCAR_25percent_rho0_normal.RData")


# # test eval function
# data %>% 
#   map(~.x %$% 
#         lm(y ~ x1 + x2 + x3) %>% 
#         coef()) %>% do.call("rbind", .) %>% colMeans()
