library(ggplot2)
library(data.table)
library(survminer)
rearrest <- fread('C:/Users/ncgra/Downloads/rearrest.csv')
rearrest[, event := abs(censor - 1)]
m1 <- coxph(Surv(months, event) ~ personal + property + cage, data=rearrest) 
fit <- survfit(m1)

## KM plot
km_fit <- survfit(Surv(months, event) ~ personal + property, data=rearrest)
ggsurvplot(km_fit)

## Plotting predictions by covariate averages 
ggsurvplot(fit, data=rearrest)
ggsurvplot(fit, data=rearrest, fun='event')
ggsurvplot(fit, data=rearrest, fun='cumhaz')
## Plotting predictions by specific covariates
fit1 <- survfit(m1, newdata=data.table(personal=0,
                                       property=0,
                                       cage=0))
fit2 <- survfit(m1, newdata=data.table(personal=1,
                                       property=1,
                                       cage=0))
fit3 <- survfit(m1)
fit_list <- list('Personal=0, Property=0'=fit1,
                 'Personal=1, Property=1'=fit2,
                 'Average'=fit3)
## Plot cumulative functions
ggsurvplot_combine(fit_list, data=rearrest, conf.int=T) ## Survival 
ggsurvplot_combine(fit_list, data=rearrest, conf.int=T, fun='event') ## Failure
ggsurvplot_combine(fit_list, data=rearrest, conf.int=T, fun='cumhaz') ## Cumulative hazard
## Should you use time-constant, non-continuous variables as a Cox PH covariate or just use Kaplan Meier?
## Plot instantaneous hazard (have to smooth somehow).
cumhaz1 <- -log(summary(fit1)$surv)
cumhaz2 <- -log(summary(fit2)$surv)
cumhaz3 <- -log(summary(fit3)$surv)
haz1 <- loess(diff(c(0,cumhaz1))~summary(fit1)$time, degree=1, span=.25)
haz2 <- loess(diff(c(0,cumhaz2))~summary(fit2)$time, degree=1, span=.25)
haz3 <- loess(diff(c(0,cumhaz3))~summary(fit3)$time, degree=1, span=.25)
preds <- data.table(time=summary(fit1)$time,
                    hazard1=predict(haz1),
                    hazard2=predict(haz2),
                    hazard3=predict(haz3))
preds <- melt(preds,id.vars='time')
ggplot(data=preds,aes(x=time,y=value,color=variable)) +
  geom_line(size=2) +
  theme_bw()
## Plot log(hazard). This is proportional.
ggplot(data=preds,aes(x=time,y=log(value),color=variable)) +
  geom_line(size=2) +
  theme_bw()


## Homework 5
library(haven)
library(ggplot2)
library(survminer)
library(survival)
library(data.table)
recid <- read_dta("C:/Users/ncgra/Downloads/recid_stata7.dta")
recid <- as.data.table(recid)
## KM curve
km_fit <- survfit(Surv(week, arrest) ~ 1, data=recid)
ggsurvplot(km_fit)
## Plot smooth hazard 
cumhaz <- -log(summary(km_fit)$surv)
haz1 <- loess(diff(c(0,cumhaz))~summary(km_fit)$time, degree=1, span=.25)
preds <- data.table(time=summary(km_fit)$time,
                    hazard=predict(haz1))
preds <- melt(preds,id.vars='time')
ggplot(data=preds,aes(x=time,y=value,color=variable)) +
  geom_line(size=2) +
  theme_bw()
## Reshape
recid[, id := 1:.N]
setnames(recid, 'week', 'week_event')
recid_long <- melt(recid, id.vars = c('id'), measure.vars = c(names(recid)[grepl('emp',names(recid))]), value.name = 'emp', variable.name = 'week')
recid_long[, week := as.numeric(gsub('emp','',week))]
recid[, grep("emp",names(recid)):=NULL]
recid_long <- merge(recid_long, recid, by='id')
recid_long[arrest==1, event := ifelse(week==week_event,1,0)]
recid_long[arrest==0, event := 0]
recid_long[is.na(emp), event := NA]
## Cox PH
m1 <- coxph(Surv(week, event) ~ 1, data=recid_long) 
fit <- survfit(m1)
ggsurvplot(fit, data=recid_long)
## Log-log
m2 <- glm(event ~ factor(week), family="binomial", data=recid_long)
template <- data.table(week=1:52) ## Add all covariates you use as equal to 0/reference/mean for binary/categorical/continuous.
template[, discrete_hazard := predict(m2,type='response',newdata=template)]
ggplot() + 
  geom_line(data=template,
            aes(x=week,y=discrete_hazard),size=1) +
  theme_bw()
## Try lots of splines for continuous week, predicting new hazards on your template.
library(lspline)
library(lmtest)
## Compare linear splines with 2,3,4 evenly spaced knots.
m_spline2 <- glm(event~1 + qlspline(week, 2), family="binomial", data=recid_long)
m_spline3 <- glm(event~1 + qlspline(week, 3), family="binomial", data=recid_long)
m_spline4 <- glm(event~1 + qlspline(week, 4), family="binomial", data=recid_long)
lrtest(m_spline2,m_spline3,m_spline4)
## Plot hazard based on linear splines with 2-10 evenly spaced knots. 
template <- data.table(week=unique(recid_long[,week])) ## Add all covariates you use as equal to 0/reference/mean for binary/categorical/continuous.
template[, discrete_hazard := predict(m2,type='response',newdata=template)]
for(s in 2:10) {
  m <- glm(event~1 + qlspline(week, s), family="binomial", data=recid_long)
  template[, (paste0('haz_spline_',s)) := predict(m,type='response',newdata=template)]
}
## Plot all on one graph
template_plot <- melt(template, id.vars='week', measure.vars=c('discrete_hazard',paste0('haz_spline_',2:10)), value.name = 'Hazard', variable.name = 'Hazard type')
ggplot() + 
  geom_line(data=template_plot,
            aes(x=week,y=Hazard,color=`Hazard type`),
            size=2) +
  scale_color_viridis_d() + 
  theme_bw()
## Add lagged unemployment
recid_long <- recid_long[order(id,week)]
recid_long[, lag_unemp := shift(emp), by='id']
m3 <- glm(event ~ factor(week) + fin + age + race + wexp + mar + paro + prio + lag_unemp, family="binomial", data=recid_long)
summary(m3)
