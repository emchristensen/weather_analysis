#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

library(TTR)

weathfile = "data/Monthly_ppt_1980_present_patched.csv"
weathframe = csv_to_dataframe(weathfile)

# creat time series ----------------------------------------------------------------
sumprecip.ts = ts(weathframe$sumprecip,start=c(1980,1),end=c(2013,10),freq=12)
maxtemp.ts = ts(weathframe$maxtemp,start=c(1980,1),end=c(2013,10),freq=12)
mintemp.ts = ts(weathframe$mintemp,start=c(1980,1),end=c(2013,10),freq=12)

# remove NAs ------------------------------------------------------------------------
sumprecip.ap = na.approx(sumprecip.ts)
maxtemp.ap = na.approx(maxtemp.ts)
mintemp.ap = na.approx(mintemp.ts)

# smoothing ----------------------------------------------------------------------
sumprecip.smooth = SMA(sumprecip.ap,12)
plot(sumprecip.smooth,xlab='',ylab='total precip',main='n=10*12')

maxtemp.smooth = SMA(maxtemp.ap,4*12)
plot(maxtemp.smooth,xlab='',ylab='max monthly temp',main='n=10*12')

mintemp.smooth = SMA(mintemp.ap,10*12)
plot(mintemp.smooth,xlab='',ylab='min monthly temp',main='n=10*12')
