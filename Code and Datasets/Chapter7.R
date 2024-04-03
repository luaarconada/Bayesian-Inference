rm(list=ls())
#
library(mixAK)
# Pg. 9
data(Galaxy)
help(Galaxy)
summary(Galaxy)
hist(Galaxy, prob=TRUE, breaks=seq(5, 40, by=0.5))
fit.freq <- NMixEM(Galaxy, K = 6)
x <- seq(5, 40, length=300)
fx <- dMVNmixture(x, weight=fit.freq$weight, mean=fit.freq$mean,
                  Sigma=rep(fit.freq$Sigma, fit.freq$K))
lines(x, fx, col="black", lwd=2)
#
# Pg. 10
Prior <- list(priorK = "uniform", Kmax = 6)
Prior <- list(priorK = "tpoisson", lambda= 2,Kmax = 6)
Prior <- list(priorK = "fixed", Kmax = 6)
#
# Pg. 14
#
# Known K
kprior.fixed <- list(priorK = "fixed", Kmax = 6)
nMCMC <- c(burn=5000, keep=10000, thin=30, info=1000)
fit.bayes <- NMixMCMC(y0 = Galaxy, prior = kprior.fixed,
                      scale=list(shift=0, scale=1), nMCMC = nMCMC, PED = F)
pdens <-NMixPredDensMarg(fit.bayes, lgrid=300)
lines(pdens$x$x1,pdens$dens$"1", lwd=2, col="magenta")
plot.ts(fit.bayes$w)
plot.ts(fit.bayes$mu)
plot.ts(fit.bayes$Sigma)
#
# Unknown K
kprior.unif <- list(priorK="uniform", Kmax=30)
nMCMC <- c(burn=5000, keep=10000, thin=5, info=1000)
RJModel <- NMixMCMC(y0=Galaxy, prior=kprior.unif, nMCMC=nMCMC,
                    scale=list(shift=0, scale=1),PED=F)
pdensRJ <- NMixPredDensMarg(RJModel, lgrid=300)
lines(pdensRJ$x$x1,pdensRJ$dens$"1", lwd=2, col="blue")
ts.plot(RJModel$K)
barplot(prop.table(table(RJModel$K)))
fit.bayes$DIC
RJModel$DIC
#
# Pg. 15
prob <- fit.bayes$poster.comp.prob_u
prob
#
# Pg. 16
data(iris)
summary(iris)
head(iris)
VARS <- names(iris)[1:4]
VARS
plot(iris)
# Pg. 18
Prior <- list(priorK = "fixed", Kmax = 3)
nMCMC <- c(burn=5000, keep=10000, thin=5, info=1000)
fit <- NMixMCMC(y0 = iris[, VARS], prior = Prior, nMCMC = nMCMC,
                scale = list(shift=0, scale=1), PED = F)
#
pdens1 <- NMixPredDensMarg(fit, lgrid=150)
plot(pdens1, main=VARS, xlab=VARS)
#
pdens2 <- NMixPredDensJoint2(fit)
plot(pdens2, xylab=VARS)
#
prob <- fit$poster.comp.prob_u
prob
prob[1:50,]
prob[51:100,]
prob[101:150,]
