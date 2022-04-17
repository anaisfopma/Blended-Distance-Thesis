# load packages
library(dplyr)
library(xtable)
library(ggplot2)
library(tibble)
library(ggthemes)


# load the workspace with the evaluation results
load("Workspaces/Simulation_II.RData")

# create a table of the mean results
sapply(eval, colMeans) %>% 
  t() %>% 
  data.frame() %>%
  xtable() %>%
  print(include.rownames=FALSE)
  
# plot the distributions of the estimates obtained with blend = 0 and blend = 1
data <- data.frame(values = c(eval$imp.blend.pmm.rank$estimate, eval$imp.mahalan.rank$estimate),
                   group = c(rep("Blending factor = 1", 10000), rep("Blending factor = 0", 10000)))
ggplot(data, aes(x = values, fill = group)) +                       
  geom_histogram(position = "identity", alpha = 0.2, bins = 100) +
  geom_vline(aes(xintercept=4.287998),
             color="black", linetype="dashed", size=0.5) +
  labs(y= "Frequency", x = "Estimate") +
  theme_tufte() +
  theme(legend.title = element_blank())

