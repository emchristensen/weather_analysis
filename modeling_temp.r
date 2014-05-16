#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

library(TTR)
library(zoo)

weathfile = "data/Monthly_ppt_1980_present_patched.csv"
weathframe = csv_to_dataframe(weathfile)

ensofile = "data/enso.csv"
ensoframe = csv_to_dataframe(ensofile)

enso.vec = as.vector(t(ensoframe[,c(2,3,4,5,6,7,8,9,10,11,12,13)]))
enso.ts = ts(enso.vec,start=c(1950,1),end=c(2013,6),freq=12)
enso.w = window(enso.ts,start=c(1980,1),end=c(2013,6))

# ============================================================================
# fitting sine model
Time = seq(length(weathframe$year))
maxtemp = weathframe$maxtemp
mintemp = weathframe$mintemp

xc<-cos(2*pi*Time/12)
xs<-sin(2*pi*Time/12)
maxt.lm <- lm(maxtemp~xc+xs)

fitmax = fitted(maxt.lm)
residsmax = resid(maxt.lm)
plot(maxtemp)
lines(fitmax,col='red')

mint.lm = lm(mintemp~xc+xs)
fitmin = fitted(mint.lm)
residsmin = resid(mint.lm)
plot(mintemp)
lines(fitmin,col='red')

# ===============================================================================
# do stuff with the residuals
residmax = resid(maxt.lm)
plot(residmax)
lines(residmax)

residmax.lm = lm(residmax[1:402]~as.vector(enso.w))
summary(residmax.lm)
acf(maxt.lm$resid)
pacf(maxt.lm$resid)
