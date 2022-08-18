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


