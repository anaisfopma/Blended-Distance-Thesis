####################
# DATA PREPARATION #
####################

# load packages
library(dplyr)
library(xtable)
library(tidyverse)
library(tibble)
library(gridExtra)
library(ggplot2)
library(ggthemes)

# create separate environment for all tables
env <- new.env()

# table 1
load("Workspaces/1.MCAR_25_normal_0.RData")
env$tab1 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 2
load("Workspaces/2.MCAR_25_normal_0.1.RData")
env$tab2 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 3
load("Workspaces/3.MCAR_25_normal_0.7.RData")
env$tab3 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 4
load("Workspaces/4.MCAR_25_skew_0.RData")
env$tab4 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 5
load("Workspaces/5.MCAR_25_skew_0.1.RData")
env$tab5 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 6
load("Workspaces/6.MCAR_25_skew_0.7.RData")
env$tab6 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 7
load("Workspaces/7.MCAR_50_normal_0.RData")
env$tab7 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 8
load("Workspaces/8.MCAR_50_normal_0.1.RData")
env$tab8 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 9
load("Workspaces/9.MCAR_50_normal_0.7.RData")
env$tab9 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 10
load("Workspaces/10.MCAR_50_skew_0.RData")
env$tab10 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 11
load("Workspaces/11.MCAR_50_skew_0.1.RData")
env$tab11 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 12
load("Workspaces/12.MCAR_50_skew_0.7.RData")
env$tab12 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 13
load("Workspaces/13.MAR_25_normal_0.RData")
env$tab13 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 14
load("Workspaces/14.MAR_25_normal_0.1.RData")
env$tab14 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 15
load("Workspaces/15.MAR_25_normal_0.7.RData")
env$tab15 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 16
load("Workspaces/16.MAR_25_skew_0.RData")
env$tab16 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 17
load("Workspaces/17.MAR_25_skew_0.1.RData")
env$tab17 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 18
load("Workspaces/18.MAR_25_skew_0.7.RData")
env$tab18 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 19
load("Workspaces/19.MAR_50_normal_0.RData")
env$tab19 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 20
load("Workspaces/20.MAR_50_normal_0.1.RData")
env$tab20 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 21
load("Workspaces/21.MAR_50_normal_0.7.RData")
env$tab21 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 22
load("Workspaces/22.MAR_50_skew_0.RData")
env$tab22 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 23
load("Workspaces/23.MAR_50_skew_0.1.RData")
env$tab23 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()

# table 24
load("Workspaces/24.MAR_50_skew_0.7.RData")
env$tab24 <- sapply(eval, function(x){mean(x$R2$est)}) %>% data.frame()



######################
# CREATE DATA FRAMES #
######################

# data frame 1
dat1 <- env$tab1 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "normal", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat1 <- rename(dat1, R2 = .)

# data frame 2
dat2 <- env$tab2 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "normal", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat2 <- rename(dat2, R2 = .)

# data frame 3
dat3 <- env$tab3 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "normal", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat3 <- rename(dat3, R2 = .)

# data frame 4
dat4 <- env$tab4 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "skewed", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat4 <- rename(dat4, R2 = .)

# data frame 5
dat5 <- env$tab5 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "skewed", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat5 <- rename(dat5, R2 = .)

# data frame 6
dat6 <- env$tab6 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "25%", dist = "skewed", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat6 <- rename(dat6, R2 = .)

# data frame 7
dat7 <- env$tab7 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "normal", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat7 <- rename(dat7, R2 = .)

# data frame 8
dat8 <- env$tab8 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "normal", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat8 <- rename(dat8, R2 = .)

# data frame 9
dat9 <- env$tab9 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "normal", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat9 <- rename(dat9, R2 = .)

# data frame 10
dat10 <- env$tab10 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "skewed", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat10 <- rename(dat10, R2 = .)

# data frame 11
dat11 <- env$tab11 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "skewed", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat11 <- rename(dat11, R2 = .)

# data frame 12
dat12 <- env$tab12 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MCAR", mis = "50%", dist = "skewed", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat12 <- rename(dat12, R2 = .)

# data frame 13
dat13 <- env$tab13 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "normal", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat13 <- rename(dat13, R2 = .)

# data frame 14
dat14 <- env$tab14 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "normal", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat14 <- rename(dat14, R2 = .)

# data frame 15
dat15 <- env$tab15 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "normal", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat15 <- rename(dat15, R2 = .)

# data frame 16
dat16 <- env$tab16 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "skewed", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat16 <- rename(dat16, R2 = .)

# data frame 17
dat17 <- env$tab17 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "skewed", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat17 <- rename(dat17, R2 = .)

# data frame 18
dat18 <- env$tab18 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "25%", dist = "skewed", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat18 <- rename(dat18, R2 = .)

# data frame 19
dat19 <- env$tab19 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "normal", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat19 <- rename(dat19, R2 = .)

# data frame 20
dat20 <- env$tab20 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "normal", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat20 <- rename(dat20, R2 = .)

# data frame 21
dat21 <- env$tab21 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "normal", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat21 <- rename(dat21, R2 = .)

# data frame 22
dat22 <- env$tab22 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "skewed", cor = 0) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat22 <- rename(dat22, R2 = .)

# data frame 23
dat23 <- env$tab23 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "skewed", cor = 0.1) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat23 <- rename(dat23, R2 = .)

# data frame 24
dat24 <- env$tab24 %>% 
  tibble::rownames_to_column("method") %>% 
  add_column(mech = "MAR", mis = "50%", dist = "skewed", cor = 0.7) %>%
  mutate(method =  factor(method, levels = c("imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                             "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"))) %>%
  arrange(method)
dat24 <- rename(dat24, R2 = .)

dat_R2 <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11, dat12, 
                 dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20, dat21, dat22, dat23, dat24)

dat_R2$mech <- factor(dat_R2$mech, levels = c("MCAR", "MAR"))
dat_R2$R2 <- round(dat_R2$R2, 3)
dat_R2 <- dat_R2 %>% round(3)

#########################
# CREATE PLOTS FOR BIAS #
#########################

plots_R2 <- ggplot(dat_R2, aes(x=method, y=R2)) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.1) +
  geom_point(aes(color = method)) +
  facet_grid(dist + cor ~ mech + mis, scales = "free") +
  theme_tufte() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  scale_colour_manual(name="Metric",
                      breaks=c( "imp.pmm", "imp.blend.pmm.rank", "imp.blendhalf.rank","imp.mahalan.rank", 
                                "imp.blend.pmm.scale", "imp.blendhalf.scale", "imp.mahalan.scale"),
                      labels=c("Predictive", "Ranked, blend = 1", "Ranked, blend = 0.5", "Ranked, blend = 0", 
                               "Scaled, blend = 1", "Scaled, blend = 0.5", "Scaled, blend = 0"),
                      values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#666666")) +
  theme(panel.spacing = unit(1, "lines"))

plots_R2
