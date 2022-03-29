# load packages
library(mvtnorm) # data generation
library(dplyr) # mutate
library(magrittr) # %>% pipe operator
library(purrr) # mapping
library(mice) # amputation of data and imputation with pmm
library(future) # parallel processing
library(furrr) # apply mapping functions in parallel using futures 
library(tibble) # column_to_rownames function in evaluate.function

# load evaluation function
source("evaluate.function.R")

# execute scripts for condition MCAR with 25% missingness
source("Simulation conditions I/1.MCAR_25_normal_0.R")
source("Simulation conditions I/2.MCAR_25_normal_0.1.R")
source("Simulation conditions I/3.MCAR_25_normal_0.7.R")
source("Simulation conditions I/4.MCAR_25_skew_0.R")
source("Simulation conditions I/5.MCAR_25_skew_0.1.R")
source("Simulation conditions I/6.MCAR_25_skew_0.7.R")

# execute scripts for condition MCAR with 50% missingness
source("Simulation conditions I/7.MCAR_50_normal_0.R")
source("Simulation conditions I/8.MCAR_50_normal_0.1.R")
source("Simulation conditions I/9.MCAR_50_normal_0.7.R")
source("Simulation conditions I/10.MCAR_50_skew_0.R")
source("Simulation conditions I/11.MCAR_50_skew_0.1.R")
source("Simulation conditions I/12.MCAR_50_skew_0.7.R")

# execute scripts for condition MARright with 25% missingness
source("Simulation conditions I/13.MAR_25_normal_0.R")
source("Simulation conditions I/14.MAR_25_normal_0.1.R")
source("Simulation conditions I/15.MAR_25_normal_0.7.R")
source("Simulation conditions I/16.MAR_25_skew_0.R")
source("Simulation conditions I/17.MAR_25_skew_0.1.R")
source("Simulation conditions I/18.MAR_25_skew_0.7.R")

# execute scripts for condition MARright with 25% missingness
source("Simulation conditions I/19.MAR_50_normal_0.R")
source("Simulation conditions I/20.MAR_50_normal_0.1.R")
source("Simulation conditions I/21.MAR_50_normal_0.7.R")
source("Simulation conditions I/22.MAR_50_skew_0.R")
source("Simulation conditions I/23.MAR_50_skew_0.1.R")
source("Simulation conditions I/24.MAR_50_skew_0.7.R")
