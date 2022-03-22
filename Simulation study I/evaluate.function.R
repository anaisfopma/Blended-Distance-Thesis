eval_sims <- function(sims, truth = c(0, 1, 1, 1), true.y){
  params <- sims %>% 
    map(~.x %>% 
          with(lm(y ~ x1 + x2 + x3)) %>% 
          pool() %>% 
          summary(conf.int = TRUE) %>% 
          mutate(true = truth,
                 cov = `2.5 %` < true & true < `97.5 %`,
                 bias = estimate - true) %>% 
          column_to_rownames("term")) %>% 
    Reduce("+", .) / length(sims)
  y <- sims %>%
    map(~.x %>% # grab means and se's for each imputed set from each simulation
          complete("long") %>% 
          select(-x1, -x2, -x3, -.id) %>% 
          group_by(.imp) %>%
          summarise_all(list(n = length, mean = mean, sd = sd)) %>% 
          mutate(se = sd/sqrt(n)) %$%  
          pool.scalar(Q = .$mean,  # pooled means and variances for each sim
                      U = .$se^2, 
                      n = mean(.$n)) %>% 
          unlist()) %>% 
    do.call("rbind", .) %>% as_tibble() %>% # stack rows and make tibble
    mutate(`2.5 %` = qbar - qt(.975, df) * sqrt(t),
           `97.5 %` = qbar + qt(.975, df) * sqrt(t), 
           true = true.y, # mean of y in population
           cov = `2.5 %` < true & true < `97.5 %`, 
           se = sqrt(t),
           bias = qbar - true) %>% 
    select(qbar, se, t, df, b, `2.5 %`, `97.5 %`, true, cov, bias)
  list(params = params, outcome = colMeans(y))
}
