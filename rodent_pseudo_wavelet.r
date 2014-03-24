#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

library(TTR)

rodentfile = 'data/Monthly_rodents.csv'
rodentframe = csv_to_dataframe(rodentfile)

# creat time series ----------------------------------------------------------------
rodents.ts = ts(rodentframe$x,start=c(1977,7),end=c(2014,2),freq=12)

# fill in NAs
rodents.ap = na.approx(rodents.ts)

# smoothing ----------------------------------------------------------------------
rodents.smooth = SMA(rodents.ap,3*12)
plot(rodents.smooth,xlab='',ylab='rodent abundance',main='n=3*12')


# correlating to weather -------------------------------------------------------
weathfile = "data/Monthly_ppt_1980_present_patched.csv"
weathframe = csv_to_dataframe(weathfile)

sumprecip.ts = ts(weathframe$sumprecip,start=c(1980,1),end=c(2013,10),freq=12)
maxtemp.ts = ts(weathframe$maxtemp,start=c(1980,1),end=c(2013,10),freq=12)
mintemp.ts = ts(weathframe$mintemp,start=c(1980,1),end=c(2013,10),freq=12)

sumprecip.ap = na.approx(sumprecip.ts)
maxtemp.ap = na.approx(maxtemp.ts)
mintemp.ap = na.approx(mintemp.ts)

rodents.w = window(rodents.ap,start=c(1980,1),end=c(2013,10))


# models ------------------------------------------------------------------
model1 = lm(rodents.w~sumprecip.ap)
summary(model1)
plot(sumprecip.ap,rodents.w)

rodents.s1 = SMA(rodents.w,3)
sumprecip.s1 = SMA(sumprecip.ap,3)
model2 = lm(rodents.s1~sumprecip.s1)
summary(model2)

plot(sumprecip.s1,rodents.s1)

rodents.s2 = SMA(rodents.w,12)
sumprecip.s2 = SMA(sumprecip.ap,12)
plot(sumprecip.s2,rodents.s2)

rodents.s3 = SMA(rodents.w,3*12)
sumprecip.s3 = SMA(sumprecip.ap,3*12)
plot(sumprecip.s3,rodents.s3)

rodents.s4 = SMA(rodents.w,5*12)

# correlate to temp -------------------------------------------------------------------
maxtemp.s1 = SMA(maxtemp.ap,3)
maxtemp.s2 = SMA(maxtemp.ap,12)
maxtemp.s3 = SMA(maxtemp.ap,3*12)
maxtemp.s4 = SMA(maxtemp.ap,5*12)

plot(maxtemp.s1,rodents.s1)
plot(maxtemp.s2,rodents.s2)
plot(maxtemp.s3,rodents.s3)
plot(maxtemp.s4,rodents.s4)
