## ----setup, include=FALSE, cache=FALSE----------------------------------------
# set global chunk options
library('knitr')
opts_chunk$set(fig.path="caperpy-",
               fig.align="center",
               fig.show="hold",
               echo=TRUE,
               results="markup",
               fig.width=10,
               fig.height=10, out.width='\\linewidth',
               out.height='\\linewidth',
               cache=FALSE,
               dev='png',
               concordance=TRUE,
               error=FALSE)
opts_knit$set(aliases = c(h = 'fig.height',
              w = 'fig.width',
              wo='out.width',
              ho='out.height'))
options(replace.assign=TRUE,width=60)
set.seed(9567)


## ----eval=FALSE---------------------------------------------------------------
## ## If devtools is not yet installed, type
## install.packages("devtools")
## 
## ## Install the package caperpyogm
## devtools::install_github("ClementCalenge/pochardTrend", ref="main")


## -----------------------------------------------------------------------------
library(pochardTrend)


## -----------------------------------------------------------------------------
head(pochard)


## -----------------------------------------------------------------------------
plot(euromap, col="grey")
plot(quadratMap, col="yellow", add=TRUE)


## ----eval=FALSE---------------------------------------------------------------
## fittedModel <- fitModel(pochard$count, pochard$site, pochard$year,
##                         pochard$idg, pochard$latq)


## ----fig.width=12, fig.height=6, out.width='\\linewidth', out.height='0.5\\linewidth'----
par(mfrow = c(1,2))
qqnorm(fittedModel$random$bqi[,2], main="Random effect grid cell b_q(i)")
qqnorm(fittedModel$random$di[,2], main="Random effect site d_i")


## -----------------------------------------------------------------------------
qqnorm(fittedModel$random$eit)


## -----------------------------------------------------------------------------
library(ggplot2)
dae <- data.frame(year=pochard$year, eit=fittedModel$random$eit)
ggplot(dae,aes(x=factor(year), y=eit))+geom_violin()+
    geom_boxplot(width=0.2, fill="grey")


## -----------------------------------------------------------------------------
## Coordinates of the centroids of the grid cells 
xy <- st_coordinates(st_centroid(quadratMap))

## calculate the neighbourhood graph from the coordinates
## of the centroids
library(spdep)
gnb <- graph2nb(relativeneigh(xy), sym=TRUE)
## We remove a weird neighbouring relationship
gnb[[185]] <- 184L
gnb[[184]] <- 185L


## -----------------------------------------------------------------------------
## Moran test
moran.test(fittedModel$random$bqi[,2], nb2listw(gnb))


## -----------------------------------------------------------------------------
fittedModel


## -----------------------------------------------------------------------------
beta0 <- fittedModel$fixed$beta0
latitude_effect <- fittedModel$fixed$latitude_effect

beta0 + latitude_effect*c(46,60)


## -----------------------------------------------------------------------------
## We load MASS to simulate a multivariate
library(MASS)

## Covariance matrix for the two coefficients
co <- cf <- fittedModel$rawTMB$cov.fixed[c("beta0","latitude_effect"),
                                         c("beta0","latitude_effect")]

## Simulate 100 000 values from the bivariate Gaussian distribution
simv <- mvrnorm(100000, c(beta0, latitude_effect), co)

## Decrease per year
## at 60 degrees latitude
dimpa60 <- (1-exp(apply(simv,1,function(x) x[2]*60+x[1])))

## at 46 degrees latitude
dimpa46 <- (1-exp(apply(simv,1,function(x) x[2]*46+x[1])))


## point estimate and SE at 60 degrees
1-exp(beta0+latitude_effect*60)
## SE
sd(dimpa60)

## point estimate and SE at 46 degrees
1-exp(beta0+latitude_effect*46)
sd(dimpa46)


## -----------------------------------------------------------------------------
dimpe60 <- (1-exp(apply(simv,1,function(x) 11*(x[2]*60+x[1]))))
dimpe46 <- (1-exp(apply(simv,1,function(x) 11*(x[2]*46+x[1]))))

## 60 degrees
1-exp(11*(beta0+latitude_effect*60))
sd(dimpe60)

## 46 degrees
1-exp(11*(beta0+latitude_effect*46))
sd(dimpe46)


## -----------------------------------------------------------------------------
## Point estimate
1-exp(beta0+latitude_effect*mean(pochard$latq))

## SE
dimens <- (1-exp(apply(simv,1,function(x) x[2]*mean(pochard$latq)+x[1])))
sd(dimens)


## -----------------------------------------------------------------------------
## Point estimate
1-exp(11*(beta0+latitude_effect*mean(pochard$latq)))

## SE
dimenso <- (1-exp(apply(simv,1,function(x) 11*(x[2]*mean(pochard$latq)+x[1]))))
sd(dimenso)


## -----------------------------------------------------------------------------
lat <- seq(45, 60, by=5)
lat[1] <- 46

showTrends(fittedModel, 2002:2012,lat)+theme(legend.position = "none")


## -----------------------------------------------------------------------------
fittedModel


## -----------------------------------------------------------------------------
## sigma_bqi
## point estimate
mean(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmabqi"],
               sqrt(fittedModel$rawTMB$cov.fixed["log_sigmabqi","log_sigmabqi"]))))
## SE
sd(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmabqi"],
             sqrt(fittedModel$rawTMB$cov.fixed["log_sigmabqi","log_sigmabqi"]))))

## sigma_di
## point estimate
mean(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmadi"],
               sqrt(fittedModel$rawTMB$cov.fixed["log_sigmadi","log_sigmadi"]))))
## SE
sd(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmadi"],
             sqrt(fittedModel$rawTMB$cov.fixed["log_sigmadi","log_sigmadi"]))))

## sigma_e
## point estimate
mean(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmae"],
               sqrt(fittedModel$rawTMB$cov.fixed["log_sigmae","log_sigmae"]))))
## SE
sd(exp(rnorm(100000, fittedModel$rawTMB$par.fixed["log_sigmae"],
             sqrt(fittedModel$rawTMB$cov.fixed["log_sigmae","log_sigmae"]))))

