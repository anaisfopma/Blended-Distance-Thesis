data <- replicate(10000, 
                  expr = gen_data(n), 
                  simplify = FALSE)
lapply(data, colMeans) %>% do.call("rbind", .) %>% colMeans






