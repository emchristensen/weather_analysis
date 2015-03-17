#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

library(TTR)
library(zoo)

weathfile = "data/Monthly_ppt_1980_present_patched.csv"
weathframe = csv_to_dataframe(weathfile)

# ===================================================================================
# create time series 
end = tail(weathframe,1)
sumprecip.ts = ts(weathframe$sumprecip,start=c(1980,1),end=c(end$year,end$month),freq=12)
maxtemp.ts = ts(weathframe$maxtemp,start=c(1980,1),end=c(end$year,end$month),freq=12)
mintemp.ts = ts(weathframe$mintemp,start=c(1980,1),end=c(end$year,end$month),freq=12)

# ==================================================================================
# remove any remaining NAs using a linear approximation from the zoo package
sumprecip.ap = na.approx(sumprecip.ts)
maxtemp.ap = na.approx(maxtemp.ts)
mintemp.ap = na.approx(mintemp.ts)

# =================================================================================
# smoothing using SMA frmo TTR library (moving average)
smoothingfactor = 12

sumprecip.smooth = SMA(sumprecip.ap,smoothingfactor)
plot(sumprecip.smooth,xlab='',ylab='total precip',main=paste('n =',smoothingfactor))

maxtemp.smooth = SMA(maxtemp.ap,smoothingfactor)
plot(maxtemp.smooth,xlab='',ylab='max monthly temp',main=paste('n =',smoothingfactor))

mintemp.smooth = SMA(mintemp.ap,smoothingfactor)
plot(mintemp.smooth,xlab='',ylab='min monthly temp',main=paste('n =',smoothingfactor))


# =====================================================================================
# decompose using Holt-Winters method
sumprecip.hw = HoltWinters(sumprecip.ap)
plot(sumprecip.hw$fitted)
sumprecip.decom = decompose(sumprecip.ap)
plot(sumprecip.decom)

maxtemp.hw = HoltWinters(maxtemp.ap)
plot(maxtemp.hw$fitted)
