# load packages
library(dplyr)
library(xtable)
library(ggplot2)
library(tibble)
library(ggthemes)
library(mvtnorm)


# load the workspace with the evaluation results
load("Workspaces/Simulation_II.RData")

# create a table of the mean results
sapply(eval, colMeans) %>% 
  t() %>% 
  data.frame() %>%
  mutate(method = c("PMM", "Blending factor = 1", "Blending factor = 0.9", "Blending factor = 0.8",
                    "Blending factor = 0.7", "Blending factor = 0.6", "Blending factor = 0.5", "Blending factor = 0.4",
                    "Blending factor = 0.3", "Blending factor = 0.2", "Blending factor = 0.1", "Blending factor = 0"),
             .before = estimate) %>%
  xtable() %>%
  print(include.rownames=FALSE)

# plot coverage and SE results
res <- sapply(eval, colMeans) %>% 
  t() %>% 
  data.frame() %>%
  mutate(method = c("PMM", "1", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1", "0"))

secovplot <- ggplot(res, aes(x=method)) + 
  geom_point(aes(y = se), colour = "#69b3a2") +
  geom_point(aes(y = cov*10), colour = "#F8766D") +
  scale_y_continuous(
    name = "Standard Error",
    sec.axis = sec_axis(~./10, name="Coverage")) +
  theme_tufte() +
  theme(axis.line = element_line(size = 0.3, colour = "black"),
        axis.title.y = element_text(color = "#69b3a2"),
        axis.title.y.right = element_text(color = "#F8766D")) +
  labs(x = "Metric")

secovplot

# obtain true reference data set
set.seed(123)
n = 500                   
rho = 0.7                     

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

true_data <- gen_data(n)


# plot density of the estimates obtained with blend = 0, blend = 1, and the true data
data <- data.frame(values = c(eval$imp.blend.pmm.rank$estimate, eval$imp.mahalan.rank$estimate, true_data$y),
                   group = c(rep("Estimates blending factor = 1", 10000), rep("Estimates blending factor = 0", 10000), rep("True data", 500)))

densplot <- ggplot(data, aes(x = values, fill = group)) +                       
  geom_density(position = "identity", alpha = 0.2, aes(y = ..density..)) +
  geom_vline(aes(xintercept=4.287998),
             color="black", linetype="dashed", size=0.5) +
  labs(y= "Density", x = " ") +
  theme_tufte() +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(size = 0.3, colour = "black"))

densplot


# plot absolute bias results for blend = 0 and blend = 1
data_absbias <- data.frame(values = c(eval$imp.blend.pmm.rank$absbias, eval$imp.mahalan.rank$absbias),
                   group = c(rep("Absolute bias blending factor = 1", 10000), rep("Absolute bias blending factor = 0", 10000)))

densplot_absbias <- ggplot(data_absbias, aes(x = values, fill = group)) +                       
  geom_density(position = "identity", alpha = 0.2) +
  geom_vline(aes(xintercept=0),
             color="black", linetype="dashed", size=0.5) +
  labs(y= "Density", x = " ") +
  theme_tufte() +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(size = 0.3, colour = "black"))

densplot_absbias
