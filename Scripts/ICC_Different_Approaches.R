
#https://www.barelysignificant.com/post/icc/


rm(list=ls())

library(tidyverse)
library(arm)
library(MASS)
library(lme4)
library(performance)
library(ggeffects)
library(sjPlot)


setwd("~/Dropbox/ArkadDiegoWOMadhuonSOSS/Data")

India_Employment <- read_csv("sampledPLFS201718.csv")

India_Employment <- India_Employment %>% filter( sex != "3")

head(India_Employment)

### - Do for States and District 
#Representation at District Level 

table( India_Employment$district )

#Representation at State level 

table( India_Employment$state )

# sex + age + I(age^2) + I(age^3) + rural + hhweight,
# my.lmer <- lmer(medwork ~ 1 + sex + age + (1 + sex + age | state), 
#                       data = India_Employment)

# Hierarchical Logistic Model - Add covariates - Sex + Age - Nonlinear effects?

my.lmer <-  glmer(medwork ~ 1 + (1 | state), 
                  family =  binomial(logit), data = India_Employment)

summary(my.lmer)

#ICC - Calculation by "hand"
tot <- 0.06329 + (pi^2/3) 
state.glmer <- 0.06329/tot
state.glmer

#TURE : 0.01887474

#tab_model(my.lmer,  p.style = "numeric", show.aic = T)
#icc.lmer <- icc(my.lmer)

#library(jtools)
#summ(my.lmer)
#plot_model(my.lmer , type = "int") +  theme_ggeffects()


# BRMS 

library(brms)

m0.brms <- brm(data = India_Employment,
               family = bernoulli(link = logit),
               medwork ~ 1 + (1 | state),
               iter = 1500, warmup = 500, thin = 10, chains = 1, cores = 4)

summary(m0.brms)

total <- 0.27^2 +  (pi^2/3)
state <- (0.27^2)/total
state

#pairs(m0.brms)
#icc(m0.brms)

#hyp <- "sd_state__Intercept^2 / (sd_state__Intercept^2 + 3.289868) = 0"
#hypothesis(m0.brms, hyp, class = NULL)
#icc <- variance_decomposition(m0.brms)

#Approach 1 

tau2 <- brms::VarCorr(m0.brms)[[1]]$sd[1]^2

# computing the ICC for the intercept
ICC1 <- tau2 / (tau2 + (pi^2 / 3) )
ICC1

#Approach 2 - Simulation

# extracting the model estimates
est <- brms::fixef(m0.brms)[,1]


# number of simulations
N <- 1e5

# drawing varying effects from the estimated distribution of varying effects
a_dpt <- rnorm(N, mean = est[1], sd = sqrt(tau2) )

# computing the ICC for females
# probability of the outcome
pA <- exp(a_dpt + est[1] * -0.5) / (1 + exp(a_dpt + est[1] * -0.5) )
# compute the Bernoulli level-1 residual variance
vA <- pA * (1 - pA)
# mean of Bernoulli variances
sA <- mean(vA)
# compute the ICC
ICC2.f <- var(pA) / (var(pA) + sA)

ICC2.f

#library("ICCbin") 
#ICCbin::iccbin(my.lmer)
#iccbin(n, y=medwork, data = India_Employment, method = "A")

#Third Approach 

# evaluating pi at the mean of the distribution of the level 2 varying effect
p <- exp(est[1]  ) / (1 + exp(est[1]  ) )
# computing var(p)
sig1 <- p * (1 - p)
# computing var(yij)
sig2 <- tau2 * p^2 * (1 + exp(est[1] ) )^(-2)
# computing the ICC
ICC3.f <- sig2 / (sig1 + sig2)

ICC3.f
