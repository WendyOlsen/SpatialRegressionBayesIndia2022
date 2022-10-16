rm(list=ls())

library("rstan")
library("ggplot2")
library("bayesplot")
library("dplyr")


setwd("~/Dropbox/ArkadDiegoWOMadhuonSOSS/Data")

India_Employment <- read_csv("sampledPLFS201718.csv")

head(India_Employment)

theme_set(bayesplot::theme_default())

India_Employment %>%
  group_by(age) %>%
  summarise(
    logit_prop_medwork = qlogis(sum(medwork) / sum(32023))
  ) %>%
  ungroup() %>%
  mutate(age = as.integer(as.factor(age))) -> cps_plot

###
# By Gender 

p <- ggplot(cps_plot, aes(x = age, y = logit_prop_medwork))

p1 <- p + geom_point(size = 2.5, color = "#DCBCBC") +
  geom_smooth(method = "gam", se = FALSE, color = "#7C0000")

p1


#https://rawgit.com/rtrangucci/class_20170809/master/logistic-regression/logistic-regression.html
#https://github.com/rtrangucci/class_20170809/tree/master/logistic-regression


