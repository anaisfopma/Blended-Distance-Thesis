####################
# DATA PREPARATION #
####################

# load packages
library(dplyr)
library(xtable)

# create separate environment for all tables
env <- new.env()

# table 1
load("Workspaces/1.MCAR_25_normal_0.RData")
env$tab1 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab1 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab1

# table 2
load("Workspaces/2.MCAR_25_normal_0.1.RData")
env$tab2 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab2 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab2

# table 3
load("Workspaces/3.MCAR_25_normal_0.7.RData")
env$tab3 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab3 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab3

# table 4
load("Workspaces/4.MCAR_25_skew_0.RData")
env$tab4 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab4 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab4

# table 5
load("Workspaces/5.MCAR_25_skew_0.1.RData")
env$tab5 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab5 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab5

# table 6
load("Workspaces/6.MCAR_25_skew_0.7.RData")
env$tab6 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab6 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab6

# table 7
load("Workspaces/7.MCAR_50_normal_0.RData")
env$tab7 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab7 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab7

# table 8
load("Workspaces/8.MCAR_50_normal_0.1.RData")
env$tab8 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab8 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab8

# table 9
load("Workspaces/9.MCAR_50_normal_0.7.RData")
env$tab9 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab9 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab9

# table 10
load("Workspaces/10.MCAR_50_skew_0.RData")
env$tab10 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab10 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab10

# table 11
load("Workspaces/11.MCAR_50_skew_0.1.RData")
env$tab11 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab11 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab11

# table 12
load("Workspaces/12.MCAR_50_skew_0.7.RData")
env$tab12 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab12 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab12

# table 13
load("Workspaces/13.MAR_25_normal_0.RData")
env$tab13 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab13 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab13

# table 14
load("Workspaces/14.MAR_25_normal_0.1.RData")
env$tab14 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab14 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab14

# table 15
load("Workspaces/15.MAR_25_normal_0.7.RData")
env$tab15 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab15 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab15

# table 16
load("Workspaces/16.MAR_25_skew_0.RData")
env$tab16 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab16 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab16

# table 17
load("Workspaces/17.MAR_25_skew_0.1.RData")
env$tab17 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab17 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab17

# table 18
load("Workspaces/18.MAR_25_skew_0.7.RData")
env$tab18 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab18 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab18

# table 19
load("Workspaces/19.MAR_50_normal_0.RData")
env$tab19 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab19 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab19

# table 20
load("Workspaces/20.MAR_50_normal_0.1.RData")
env$tab20 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab20 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab20

# table 21
load("Workspaces/21.MAR_50_normal_0.7.RData")
env$tab21 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab21 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab21

# table 22
load("Workspaces/22.MAR_50_skew_0.RData")
env$tab22 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab22 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab22

# table 23
load("Workspaces/23.MAR_50_skew_0.1.RData")
env$tab23 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab23 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab23

# table 24
load("Workspaces/24.MAR_50_skew_0.7.RData")
env$tab24 <- sapply(eval, function(x){x$outcome}) %>% data.frame() %>%
  add_row(sapply(eval, function(x){mean(x$R2$est)}) %>% t() %>% data.frame())
# optional: table with R2 results
env$R2tab24 <- sapply(eval, function(x){colMeans(x$R2)}) %>% t() %>% data.frame()
env$R2tab24

# make columns with conditions
MECH <- c("MCAR", "", "", "", "", "", "", "", "", "", "", "", "MAR", "","", "", "", "", "", "", "", "", "", "")
MIS <- c("25%", "", "", "", "", "", "50%", "", "", "", "", "", "25%", "", "", "", "", "", "50%", "", "", "", "", "")
DIST <- c("normal", "", "", "skewed", "", "", "normal", "", "", "skewed", "", "", "normal", "", "", "skewed", "", "",
          "normal", "", "", "skewed", "", "")
COR <- rep(c(0, 0.1, 0.7), 8)

conditions <- cbind(MECH, MIS, DIST, COR)



##############################
# CREATE TABLES WITH RESULTS #
##############################

# make table 1 for pmm
tab_pmm <- rbind(env$tab1$imp.pmm, env$tab2$imp.pmm, env$tab3$imp.pmm, env$tab4$imp.pmm, env$tab5$imp.pmm,
                 env$tab6$imp.pmm, env$tab7$imp.pmm, env$tab8$imp.pmm, env$tab9$imp.pmm, env$tab10$imp.pmm,
                 env$tab11$imp.pmm, env$tab12$imp.pmm, env$tab13$imp.pmm, env$tab14$imp.pmm, env$tab15$imp.pmm,
                 env$tab16$imp.pmm, env$tab17$imp.pmm, env$tab18$imp.pmm, env$tab19$imp.pmm, env$tab20$imp.pmm,
                 env$tab21$imp.pmm, env$tab22$imp.pmm, env$tab23$imp.pmm, env$tab24$imp.pmm) %>%
  round(3)
tab_pmm <- cbind(conditions, tab_pmm) # add conditions to the table
colnames(tab_pmm) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_pmm_latex <- tab_pmm %>% # create table in latex format
  xtable() %>%
  print(include.rownames=FALSE)

# make table 2 for ranked blend = 1
tab_blend.pmm.rank <- rbind(env$tab1$imp.blend.pmm.rank, env$tab2$imp.blend.pmm.rank, env$tab3$imp.blend.pmm.rank, env$tab4$imp.blend.pmm.rank, env$tab5$imp.blend.pmm.rank,
                            env$tab6$imp.blend.pmm.rank, env$tab7$imp.blend.pmm.rank, env$tab8$imp.blend.pmm.rank, env$tab9$imp.blend.pmm.rank, env$tab10$imp.blend.pmm.rank,
                            env$tab11$imp.blend.pmm.rank, env$tab12$imp.blend.pmm.rank, env$tab13$imp.blend.pmm.rank, env$tab14$imp.blend.pmm.rank, env$tab15$imp.blend.pmm.rank,
                            env$tab16$imp.blend.pmm.rank, env$tab17$imp.blend.pmm.rank, env$tab18$imp.blend.pmm.rank, env$tab19$imp.blend.pmm.rank, env$tab20$imp.blend.pmm.rank,
                            env$tab21$imp.blend.pmm.rank, env$tab22$imp.blend.pmm.rank, env$tab23$imp.blend.pmm.rank, env$tab24$imp.blend.pmm.rank) %>%
  round(3)
tab_blend.pmm.rank <- cbind(conditions, tab_blend.pmm.rank)
colnames(tab_blend.pmm.rank) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_blend.pmm.rank_latex <- tab_blend.pmm.rank %>% 
  xtable() %>%
  print(include.rownames=FALSE)

# make table 3 for ranked blend = 0.5
tab_blendhalf.rank <- rbind(env$tab1$imp.blendhalf.rank, env$tab2$imp.blendhalf.rank, env$tab3$imp.blendhalf.rank, env$tab4$imp.blendhalf.rank, env$tab5$imp.blendhalf.rank,
                            env$tab6$imp.blendhalf.rank, env$tab7$imp.blendhalf.rank, env$tab8$imp.blendhalf.rank, env$tab9$imp.blendhalf.rank, env$tab10$imp.blendhalf.rank,
                            env$tab11$imp.blendhalf.rank, env$tab12$imp.blendhalf.rank, env$tab13$imp.blendhalf.rank, env$tab14$imp.blendhalf.rank, env$tab15$imp.blendhalf.rank,
                            env$tab16$imp.blendhalf.rank, env$tab17$imp.blendhalf.rank, env$tab18$imp.blendhalf.rank, env$tab19$imp.blendhalf.rank, env$tab20$imp.blendhalf.rank,
                            env$tab21$imp.blendhalf.rank, env$tab22$imp.blendhalf.rank, env$tab23$imp.blendhalf.rank, env$tab24$imp.blendhalf.rank) %>%
  round(3)
tab_blendhalf.rank <- cbind(conditions, tab_blendhalf.rank)
colnames(tab_blendhalf.rank) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", 'R2')
tab_blendhalf.rank_latex <- tab_blendhalf.rank %>% 
  xtable() %>%
  print(include.rownames=FALSE)

# make table 4 for ranked blend = 0
tab_mahalan.rank <- rbind(env$tab1$imp.mahalan.rank, env$tab2$imp.mahalan.rank, env$tab3$imp.mahalan.rank, env$tab4$imp.mahalan.rank, env$tab5$imp.mahalan.rank,
                          env$tab6$imp.mahalan.rank, env$tab7$imp.mahalan.rank, env$tab8$imp.mahalan.rank, env$tab9$imp.mahalan.rank, env$tab10$imp.mahalan.rank,
                          env$tab11$imp.mahalan.rank, env$tab12$imp.mahalan.rank, env$tab13$imp.mahalan.rank, env$tab14$imp.mahalan.rank, env$tab15$imp.mahalan.rank,
                          env$tab16$imp.mahalan.rank, env$tab17$imp.mahalan.rank, env$tab18$imp.mahalan.rank, env$tab19$imp.mahalan.rank, env$tab20$imp.mahalan.rank,
                          env$tab21$imp.mahalan.rank, env$tab22$imp.mahalan.rank, env$tab23$imp.mahalan.rank, env$tab24$imp.mahalan.rank) %>%
  round(3)
tab_mahalan.rank <- cbind(conditions, tab_mahalan.rank)
colnames(tab_mahalan.rank) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_mahalan.rank_latex <- tab_mahalan.rank %>% 
  xtable() %>%
  print(include.rownames=FALSE)

# make table 5 for scaled blend = 1
tab_blend.pmm.scale <- rbind(env$tab1$imp.blend.pmm.scale, env$tab2$imp.blend.pmm.scale, env$tab3$imp.blend.pmm.scale, env$tab4$imp.blend.pmm.scale, env$tab5$imp.blend.pmm.scale,
                             env$tab6$imp.blend.pmm.scale, env$tab7$imp.blend.pmm.scale, env$tab8$imp.blend.pmm.scale, env$tab9$imp.blend.pmm.scale, env$tab10$imp.blend.pmm.scale,
                             env$tab11$imp.blend.pmm.scale, env$tab12$imp.blend.pmm.scale, env$tab13$imp.blend.pmm.scale, env$tab14$imp.blend.pmm.scale, env$tab15$imp.blend.pmm.scale,
                             env$tab16$imp.blend.pmm.scale, env$tab17$imp.blend.pmm.scale, env$tab18$imp.blend.pmm.scale, env$tab19$imp.blend.pmm.scale, env$tab20$imp.blend.pmm.scale,
                             env$tab21$imp.blend.pmm.scale, env$tab22$imp.blend.pmm.scale, env$tab23$imp.blend.pmm.scale, env$tab24$imp.blend.pmm.scale) %>%
  round(3)
tab_blend.pmm.scale <- cbind(conditions, tab_blend.pmm.scale)
colnames(tab_blend.pmm.scale) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_blend.pmm.scale_latex <- tab_blend.pmm.scale %>% 
  xtable() %>%
  print(include.rownames=FALSE)

# make table 6 for scaled blend = 0.5
tab_blendhalf.scale <- rbind(env$tab1$imp.blendhalf.scale, env$tab2$imp.blendhalf.scale, env$tab3$imp.blendhalf.scale, env$tab4$imp.blendhalf.scale, env$tab5$imp.blendhalf.scale,
                             env$tab6$imp.blendhalf.scale, env$tab7$imp.blendhalf.scale, env$tab8$imp.blendhalf.scale, env$tab9$imp.blendhalf.scale, env$tab10$imp.blendhalf.scale,
                             env$tab11$imp.blendhalf.scale, env$tab12$imp.blendhalf.scale, env$tab13$imp.blendhalf.scale, env$tab14$imp.blendhalf.scale, env$tab15$imp.blendhalf.scale,
                             env$tab16$imp.blendhalf.scale, env$tab17$imp.blendhalf.scale, env$tab18$imp.blendhalf.scale, env$tab19$imp.blendhalf.scale, env$tab20$imp.blendhalf.scale,
                             env$tab21$imp.blendhalf.scale, env$tab22$imp.blendhalf.scale, env$tab23$imp.blendhalf.scale, env$tab24$imp.blendhalf.scale) %>%
  round(3)
tab_blendhalf.scale <- cbind(conditions, tab_blendhalf.scale)
colnames(tab_blendhalf.scale) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_blendhalf.scale_latex <- tab_blendhalf.scale %>% 
  xtable() %>%
  print(include.rownames=FALSE)

# make table 7 for scaled blend = 0
tab_mahalan.scale <- rbind(env$tab1$imp.mahalan.scale, env$tab2$imp.mahalan.scale, env$tab3$imp.mahalan.scale, env$tab4$imp.mahalan.scale, env$tab5$imp.mahalan.scale,
                           env$tab6$imp.mahalan.scale, env$tab7$imp.mahalan.scale, env$tab8$imp.mahalan.scale, env$tab9$imp.mahalan.scale, env$tab10$imp.mahalan.scale,
                           env$tab11$imp.mahalan.scale, env$tab12$imp.mahalan.scale, env$tab13$imp.mahalan.scale, env$tab14$imp.mahalan.scale, env$tab15$imp.mahalan.scale,
                           env$tab16$imp.mahalan.scale, env$tab17$imp.mahalan.scale, env$tab18$imp.mahalan.scale, env$tab19$imp.mahalan.scale, env$tab20$imp.mahalan.scale,
                           env$tab21$imp.mahalan.scale, env$tab22$imp.mahalan.scale, env$tab23$imp.mahalan.scale, env$tab24$imp.mahalan.scale) %>%
  round(3)
tab_mahalan.scale <- cbind(conditions, tab_mahalan.scale)
colnames(tab_mahalan.scale) <- c("mech","mis", "dist", "cor", "qbar", "se", "t", "df", "b", "2.5%", "97.5%", "true", "cov", "bias", "R2")
tab_mahalan.scale_latex <- tab_mahalan.scale %>% 
  xtable() %>%
  print(include.rownames=FALSE)
