rm(list=ls())
#
# 1
x = read.csv("UCLA.csv", sep=",", dec=".")
names(x)
summary(x)
#a
boxplot(gre~admit,data=x)
#b
boxplot(gpa~admit,data=x)
#c
boxplot(rank~admit,data=x)
#d
fit <- glm(admit~gre+gpa+rank,data=x,family=binomial)
summary(fit)
#e
predict(fit, data.frame(gre=790,gpa=3.8,rank=1),type="response")
#f
library(MCMCpack)
fit.bayes <- MCMClogit(admit~gre+gpa+rank,data=x)
summary(fit.bayes)
#g
plot(fit.bayes)
acf(fit.bayes[,1]);acf(fit.bayes[,2]);acf(fit.bayes[,3]);acf(fit.bayes[,4])
fit.bayes <- MCMClogit(admit~gre+gpa+rank,data=x,thin=20)
acf(fit.bayes[,1]);acf(fit.bayes[,2]);acf(fit.bayes[,3]);acf(fit.bayes[,4])
#h
linear = fit.bayes[,1] +fit.bayes[,2]* 790 + fit.bayes[,3]*3.8 +  fit.bayes[,4]*1
pred.prob = exp(linear)/(1+exp(linear))
hist(pred.prob)
mean(pred.prob)
median(pred.prob)
quantile(pred.prob,c(0.025,0.975))
#
# 2
#
x = read.csv("Heart.csv", sep=",", dec=".")
x = na.omit(x)
names(x)
summary(x)
#a
plot(table(x$AHD,x$Thal))
#b
plot(table(x$AHD,x$ChestPain))
#c
boxplot(Ca~AHD,data=x)
#d
AHD.bin=ifelse(x$AHD=="Yes",1,0)
fit <- glm(AHD.bin~Ca+Thal+ChestPain,data=x,family=binomial)
summary(fit)
#e
predict(fit, data.frame(Ca=2,Thal="fixed",ChestPain="typical"),type="response")
#f
fit.bayes <- MCMClogit(AHD.bin ~ Ca + Thal + ChestPain,data=x)
summary(fit.bayes)
#g
plot(fit.bayes)
acf(fit.bayes[,1]);acf(fit.bayes[,2]);acf(fit.bayes[,3]);acf(fit.bayes[,4])
fit.bayes <- MCMClogit(AHD.bin ~ Ca + Thal + ChestPain,data=x,thin=20)
acf(fit.bayes[,1]);acf(fit.bayes[,2]);acf(fit.bayes[,3]);acf(fit.bayes[,4])
#h
linear = fit.bayes[,1] +fit.bayes[,2]* 2 + fit.bayes[,7]
pred.prob = exp(linear)/(1+exp(linear))
hist(pred.prob)
mean(pred.prob)
median(pred.prob)
quantile(pred.prob,c(0.025,0.975))
# 
# 3
#
data("warpbreaks")
boxplot(warpbreaks$breaks~warpbreaks$wool)
boxplot(warpbreaks$breaks~warpbreaks$tension)
fit=glm(breaks~wool+tension,data=warpbreaks,family="poisson")
summary(fit)

predict(fit,data.frame(wool="A",tension="H"),type="response")

bayes.fit=MCMCpoisson(breaks~wool+tension,data=warpbreaks)
summary(bayes.fit)

plot(bayes.fit)
acf(bayes.fit[,1]);acf(bayes.fit[,2]);acf(bayes.fit[,3]);acf(bayes.fit[,4])
bayes.fit=MCMCpoisson(breaks~wool+tension,data=warpbreaks,thin=20)
acf(bayes.fit[,1]);acf(bayes.fit[,2]);acf(bayes.fit[,3]);acf(bayes.fit[,4])

linear=bayes.fit[,1]+bayes.fit[,4]
pred.prob = exp(linear)/(1+exp(linear))

pred.lambda = exp(linear)
hist(pred.lambda)
mean(pred.lambda)
quantile(pred.lambda,c(0.025,0.975))

pred.breaks =rpois(length(pred.lambda),pred.lambda)
plot(table(pred.breaks))
#
