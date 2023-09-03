#Authors 2023
#Diego Perez Ruiz has written this code in 2021/2022.    A few minor amendments by Wendy Olsen help us access data for different 
#age groups, years, or sex.

#Furthermore, the advisor on all the materials is Arkadiusz Wisniowski, and the research assistant in 2021/22 was Madhu Chauhan.

#We thank the funder, University of Manchester - School of Social Sciences. 

#https://www.barelysignificant.com/post/icc/

#The command below is  used for 2018/2019 Periodic Labour Force Survey data (India). 

rm(list=ls())

library(tidyverse)
library(arm)
library(MASS)
library(lme4)
library(performance)
library(ggeffects)
library(sjPlot)

setwd("C:/data/SpatialBayesian2023newFiles/data")
Indiaraw<- readRDS("IndiaPLFS201718.rds")
# previously had used  India_Employment <- read_csv("data/sampledPLFS201718.csv")  in 2022
set.seed(123456)
Indiaraw <- Indiaraw[sample(nrow(Indiaraw), 20000), ]
#   Archive: in previous 2022 round we used this file instead. Indiaraw <- read_csv("data/sampledPLFS201718.rds")
Indiaraw %>% count(female)
# filter out very few cases of other or missing sex
Indiaraw <- Indiaraw %>% filter( sex != "3")
India_Employment<-Indiaraw

#Now proceed with the ICC routines.
setwd("c:/data/SpatialBayesian2023newFiles")


#* * *****************Here, you can choose to use only certain age groups, or just one sex.* * * * 
#* 
summary(India_Employment$rural)
summary(India_Employment$female)
summary(India_Employment$age)
India_Employmentbak<-India_Employment
India_Employment <- India_Employment[India_Employment$age<31&India_Employment$age>15,]
summary(India_Employment$age)

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

