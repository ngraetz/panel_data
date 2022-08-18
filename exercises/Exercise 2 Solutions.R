library(nlme)
library(data.table)
library(ggplot2)
setwd('C:/Users/ncgra/Downloads/')

# Multilevel models in Table 5.7
unemployment <- read.table("unemployment_pp.txt", header=T, sep=",")

modelA <- lme(cesd ~ months, unemployment, random= ~months|id, method="ML")

modelB <- update(modelA, cesd~months+unemp)

modelC <- update(modelB, cesd~months*unemp)

modelD <- lme(cesd~unemp+unemp:months, 
              random=~unemp+unemp:months|id, 
              data=unemployment, control = list(opt = "optim", tolerance=1e-6))

# Visualize global patterns
unemployment <- as.data.table(unemployment)
range(unemployment[, months])
preds <- as.data.table(expand.grid('(Intercept)'=1,unemp=c(0,1),months=0:16))
preds[, `unemp:months` := unemp*months]
preds[, `months:unemp` := months*unemp]
fixed <- fixed.effects(modelD)
preds[, fit := as.matrix(preds[,names(fixed),with=F]) %*% fixed] ## Why do we not predict with random effects?
ggplot() + 
  geom_line(data=preds,
            aes(x=months,
                y=fit,
                color=as.factor(unemp)),
            size=2) + 
  theme_bw()

# Add data
ggplot() + 
  geom_line(data=preds,
            aes(x=months,
                y=fit,
                color=as.factor(unemp)),
            size=2) + 
  geom_point(data=unemployment,
             aes(x=months,
                 y=cesd,
                 color=as.factor(unemp))) + 
  theme_bw()

## Get confidence intervals - why do we simulate? 
draws <- 1000
fixed_draws <- mvrnorm(draws,fixed.effects(modelD),vcov(modelD))
fits <- fixed_draws %*% t(as.matrix(preds[,colnames(fixed_draws),with=F]))
fits <- as.data.table(fits)
means <- as.numeric(fits[, lapply(.SD,mean)])
lowers <- as.numeric(fits[, lapply(.SD,quantile,0.025)])
uppers <- as.numeric(fits[, lapply(.SD,quantile,0.975)])
preds[, mean := means]
preds[, lower := lowers]
preds[, upper := uppers]
ggplot() + 
  geom_line(data=preds,
            aes(x=months,
                y=fit,
                color=as.factor(unemp)),
            size=2) + 
  geom_ribbon(data=preds,
              aes(x=months,
                  ymin=lower,
                  ymax=upper,
                  fill=as.factor(unemp)),
              alpha=0.5) + 
  geom_point(data=unemployment,
             aes(x=months,
                 y=cesd,
                 color=as.factor(unemp))) + 
  theme_bw()
## Would the above confidence interval calculation be correct if including random effects?




library(dplyr)
setwd("C:/Users/ncgra/Downloads/")
dataset <- read.delim("RIESBYT4.dat", header=FALSE, sep="")
dataset <- dataset %>%
  rename(id = V1, hdrs = V2, week = V4, sex = V5, imi = V7, dmi = V8)

dataset <- as.data.table(dataset)
dataset[, ldim := dmi-imi]
dataset[, mean_ldim := mean(ldim), by='id']
dataset[, within_ldim := ldim - mean_ldim]

library(nlme)
modela <- lme(hdrs~week, 
              random=~week|id, 
              data=dataset, 
              control = list(opt = "optim", tolerance=1e-6, method="ML"))
modelb <- lme(hdrs~week+ldim, 
              random=~week+ldim|id, 
              data=dataset, 
              control = list(opt = "optim", tolerance=1e-6, method="ML"))
modelc <- lme(hdrs~week+within_ldim+mean_ldim, 
              random=~week+within_ldim|id, 
              data=dataset, 
              control = list(opt = "optim", tolerance=1e-6, method="ML"))
anova(modela,modelb,modelc)
library(multcomp)
cont=matrix(c(0,0,1,-1),ncol=4)
rownames(cont)="mean_ldim - within_ldim"
summary(glht(modelD,linfct=cont))


library(gtools)
library(data.table)
library(ggplot2)
library(lme4)
d <- fread('C:/Users/ncgra/Downloads/foxngeese_pp.csv')
d[, read_bar := read-mean(read)]
m1 <- glmer(cbind(nmoves, 20-nmoves) ~ game + (game|id), data=d, family=binomial(link='logit'))
d[, pred := predict(m1, type='response') * 20]
ggplot(data=d, aes(x=game)) + 
  geom_point(aes(y=nmoves)) +
  geom_line(aes(y=pred)) +
  facet_wrap(~id) + 
  theme_bw()


glmer(nmoves~ 1 + 19/ (1 + xmid*exp( -scal*game + u)),
      fixed=scal+xmid~1, random= scal+u~1 |id, 
      start=c(scal=.2, xmid=12))




setwd("C:/Users/ncgra/Downloads/")
dataset <- read.delim("RIESBYT4.dat", header=FALSE, sep="")
dataset <- dataset %>%
  rename(id = V1, hdrs = V2, week = V4, sex = V5,
         diagnose = V6, imi = V7, dmi = V8)
dataset <- as.data.table(dataset)
dataset[, ldim := dmi-imi]
dataset[, mean_ldim := mean(ldim), by='id']
dataset[, within_ldim := ldim - mean_ldim]
dataset[, endog := diagnose]
modelA <- lme(hdrs~week + week^2 + ldim + endog + ldim*endog, data=dataset, random = ~week + week^2 | id)
summary(modelA)
#specifying the covariance and correlation matrix
corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[1]]
  print(corm)
  varstruct <- print(glsob$modelStruct$varStruct)  
  varests <- coef(varstruct, uncons=F, allCoef=T)
  covm <- corm*glsob$sigma^2*t(t(varests))%*%t(varests)
  return(covm)
}

# unstructured 
unstruct <- gls(hdrs~week + week^2 + ldim + endog + ldim*endog, data=dataset, correlation=corSymm(form = ~ 1 |id),  weights=varIdent(form = ~ 1|week), method="REML")
corandcov(unstruct)

unstruct <- gls(opp~time*ccog, data=opposites, correlation=corSymm(form = ~ 1 |id),  weights=varIdent(form = ~ 1|wave), method="REML")
corandcov(unstruct)
