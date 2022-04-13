eval_sim2 <- function(sims, truth = true.y){
  output <- list()
  for (i in 1:length(sims)){
    imps <- sims[[i]]$imp$y %>% unlist
    estimate <- imps %>% mean
    true <- truth[[i]]
    dev <-  (sims[[i]]$imp$y - true) # deviation from the truth
    ssd <- sum(dev^2)
    bias <- dev %>%  unlist %>% mean
    absbias <- abs(dev) %>%  unlist %>% mean
    se <- sd(imps) #empirical standard error
    se2 <- sqrt(ssd / length(imps)) #empirical standard error
    lwr <- estimate - qt(.975, 49) * se
    upr <- estimate + qt(.975, 49) * se
#    lwr2 <- quantile(imps, probs = c(.025))
#    upr2 <- quantile(imps, probs = c(.975))
    cov <- lwr < true & true < upr
#    cov2 <- lwr2 < true & true < upr2
    rmse <- sqrt(mean((true - imps)^2))
    output[[i]] <- c(estimate = estimate, true = true, bias = bias,
                     absbias = absbias,
                  ssd = ssd, se = se, lwr = lwr, upr = upr, cov = cov, rmse = rmse) 
                  #quant.lwr = lwr2, quant.upr = upr2, quant.cov = cov2)
  }
return(output %>% do.call("rbind", .) %>% as_tibble)
}
