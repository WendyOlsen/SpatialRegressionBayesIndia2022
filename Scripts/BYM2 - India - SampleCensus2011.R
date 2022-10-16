
rm(list=ls())

pkgs <- c("sf", "spdep", "INLA", "rstan", "tidyverse")
lapply(pkgs, require, character.only = TRUE)
rstan_options(javascript=FALSE)

setwd("~/Dropbox/SOST-Recovery Grant/Scripts")

source("icar-functions.R")

## the commented-out code will set you up to use the same data as in the README document
## get a shapefile
## url <- "https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_state_20m.zip"
## get_shp(url, "states")
## states <- st_read("states")

## prep data for ICAR function in Stan
##C <- spdep::nb2mat(spdep::poly2nb(states, queen = TRUE), style = "B", zero.policy = TRUE)
##icar.data <- prep_icar_data(C)

# load data: from Spatial Epi package, a data.frame and a shapefile
library(SpatialEpi)

setwd("~/Dropbox/ArkadDiegoWOMadhuonSOSS/Data")

India_Employment <- load(file = "India_Employment_withCensus2011.rda")

dim(India_Employment)

rstan_options(auto_write = TRUE)

setwd("~/Dropbox/SOST-Recovery Grant/maps-master/Districts/Census_2011")

India_Districs <- st_read("2011_Dist.shp")

sf::sf_use_s2(FALSE)

sp <- India_Districs$geometry

India_Data_Emplotment_Census2011 <- left_join(India_Districs,India_Sample_Employment, by = "censuscode" )


## prep data for ICAR function in Stan
C <- spdep::nb2mat(spdep::poly2nb(sp, queen = TRUE), style = "B", zero.policy = TRUE)
icar.data <- prep_icar_data(C)

## notice that the scale_factor is just ones. 
icar.data$inv_sqrt_scale_factor

## calculate the scale factor for each of k connected group of nodes, using scale_c function
k <- icar.data$k
scale_factor <- vector(mode = "numeric", length = k)
for (j in 1:k) {
  g.idx <- which(icar.data$comp_id == j) 
  if (length(g.idx) == 1) {
    scale_factor[j] <- 1
    next
  }    
  Cg <- C[g.idx, g.idx] 
  scale_factor[j] <- scale_c(Cg) 
}

## update the data list for Stan
icar.data$inv_sqrt_scale_factor <- 1 / sqrt( scale_factor )

## see the new values
print(icar.data$inv_sqrt_scale_factor)


#India_Employment_District <- India_Employment %>%
#  group_by(distcode) %>%
#  summarise(Freq_District = sum(medwork)) 


n <- nrow(India_Data_Emplotment_Census2011)

India_Data_Emplotment_Census2011$Freq_State[is.na(India_Data_Emplotment_Census2011$Freq_State)] <- 0
  
dl <- list(
  n = n,
  prior_only = 0, # ignore the data, sample from the joint prior probability of parameters
  y = India_Data_Emplotment_Census2011$Freq_State , # just a placeholder
  offset = rep(1, n) # placeholder  
)


dl <- c(dl, icar.data)

setwd("~/Dropbox/SOST-Recovery Grant/Scripts")

source("icar-functions.R")

## compile the model
BYM2 <- stan_model("BYM2.stan")

## sample
fit <- sampling(BYM2, data = dl, chains = 4, cores = 4)

summary(fit)

## view some results from the joint prior probability
plot(fit, pars = "phi_tilde")
plot(fit, pars = "convolution")
plot(fit, pars = "spatial_scale", plotfun = "hist")
plot(fit, pars = "rho", plotfun = "hist")

fit@model_pars

#
rho <- as.matrix(fit, pars = "rho")
mean(exp(rho))
hist(exp(rho))

spatial_scale <- as.matrix(fit, pars = "spatial_scale")
mean(spatial_scale)
hist(spatial_scale)
exp(mean(spatial_scale))

## degree of spatial autocorrelation (SA) in the convolution term
convolution <- as.matrix(fit, pars = "convolution")
sa <- apply(convolution, 1, mc, w=C)
hist(sa)

## simple map of the posterior mean of the convolution term 
spx <- st_as_sf(sp)
spx$convolution <- apply(convolution, 2, mean)
plot(spx[,"convolution"])



# joint probability of phi, as specified here,
#is driven by phi'*(D-C)*phi with D a diagonal matrix 
#containing number of neighbors of each observation

D_diag <- rowSums(C)
phi.samples <- as.matrix(fit, pars = "phi_tilde")
phi.var <- apply(phi.samples, 2, var)
plot(D_diag, phi.var)

##
library("ggcorrplot")

 
corr <- round(cor(phi.samples), 1)

colnames(corr) <- India_Districs$DISTRICT #NULL
rownames(corr) <- India_Districs$DISTRICT #NULL


#ggcorrplot(corr, 
#           hc.order = TRUE, 
#           outline.color = "white")


# Get the upper triangle
ggcorrplot(corr,
           ##hc.order = TRUE,
           type = "upper",
           outline.color = "white")

highlyCorrelated <- findCorrelation(corr, cutoff=(0.1),verbose = FALSE)

# Get the upper triangle
ggcorrplot(corr[highlyCorrelated ,highlyCorrelated ],
           hc.order = TRUE,
           tl.cex = 4,
           type = "upper",
           ggtheme = ggplot2::theme_bw, 
           outline.color = "white")  

##
corrplot(corr[highlyCorrelated ,highlyCorrelated ], method = 'square', diag = FALSE, order = 'hclust',
         addrect = 5, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')

## drop states with no neighbors
#drop.idx <- which( India_Districs$ST_NM %in% c("Alaska", "Hawaii", "Puerto Rico"))
#cont <- states[-drop.idx, ]
#phi_variance <- phi.var[-drop.idx]

ggplot(India_Districs) +
  geom_sf(aes(fill=log(phi.var))) +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  )


