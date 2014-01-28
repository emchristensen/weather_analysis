Time = time(precip.w)
tmodel = lm(precip.w~Time)
summary(tmodel)
confint(tmodel)
acf(tmodel$resid)
pacf(tmodel$resid) #resids are autocorelated - should use glm

library(nlme)
library(zoo)
precip.approx = na.approx(precip.w)
precip.gls = gls(precip.approx~Time,cor=corAR1(.28))
coef(precip.gls)
confint(precip.gls)

#need to account for seasonal component
Seas = cycle(precip.w)
precip.lm = lm(precip.w~Time+factor(Seas)) #0 in model forces intercept to 0
coef(precip.lm)
precip.gls = gls(precip.approx~Time+factor(Seas),cor=corAR1(.28))
coef(precip.gls)