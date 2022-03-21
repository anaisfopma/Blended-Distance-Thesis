# load required packages
require(mvtnorm) # data generation
require(dplyr) # mutate
require(magrittr) # %>% pipe operator
require(purrr) # mapping
require(mice) # amputation of data and imputation with pmm
require(future) # parallel processing
require(furrr) # apply mapping functions in parallel using futures 
require(textshape) # column_to_rownames function in evaluate.function

# load evaluation function
source("evaluate.function.R")

# execute scripts for condition MCAR with 25% missingness
source("Simulation study I/Simulation conditions I/1.MCAR_25_normal_0.R")
source("Simulation study I/Simulation conditions I/2.MCAR_25_normal_0.1.R")
source("Simulation study I/Simulation conditions I/3.MCAR_25_normal_0.7.R")
source("Simulation study I/Simulation conditions I/4.MCAR_25_skew_0.R")
source("Simulation study I/Simulation conditions I/5.MCAR_25_skew_0.1.R")
source("Simulation study I/Simulation conditions I/6.MCAR_25_skew_0.7.R")

# execute scripts for condition MCAR with 50% missingness
source("Simulation study I/Simulation conditions I/7.MCAR_50_normal_0.R")
source("Simulation study I/Simulation conditions I/8.MCAR_50_normal_0.1.R")
source("Simulation study I/Simulation conditions I/9.MCAR_50_normal_0.7.R")
source("Simulation study I/Simulation conditions I/10.MCAR_50_skew_0.R")
source("Simulation study I/Simulation conditions I/11.MCAR_50_skew_0.1.R")
source("Simulation study I/Simulation conditions I/12.MCAR_50_skew_0.7.R")

# execute scripts for condition MARright with 25% missingness
source("Simulation study I/Simulation conditions I/1.MAR_25_normal_0.R")
source("Simulation study I/Simulation conditions I/2.MAR_25_normal_0.1.R")
source("Simulation study I/Simulation conditions I/3.MAR_25_normal_0.7.R")
source("Simulation study I/Simulation conditions I/4.MAR_25_skew_0.R")
source("Simulation study I/Simulation conditions I/5.MAR_25_skew_0.1.R")
source("Simulation study I/Simulation conditions I/6.MAR_25_skew_0.7.R")

# execute scripts for condition MARright with 25% missingness
source("Simulation study I/Simulation conditions I/7.MAR_50_normal_0.R")
source("Simulation study I/Simulation conditions I/8.MAR_50_normal_0.1.R")
source("Simulation study I/Simulation conditions I/9.MAR_50_normal_0.7.R")
source("Simulation study I/Simulation conditions I/10.MAR_50_skew_0.R")
source("Simulation study I/Simulation conditions I/11.MAR_50_skew_0.1.R")
source("Simulation study I/Simulation conditions I/12.MAR_50_skew_0.7.R")
