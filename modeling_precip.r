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
enso.w = window(enso.ts,start=c(1979,11),end=c(2013,6))

# ==================================================================================
# sine model
Time = seq(length(weathframe$year))
precip = weathframe$sumprecip

xc<-cos(2*pi*Time/12)
xs<-sin(2*pi*Time/12)
ppt.lm <- lm(precip~xc+xs)

fitppt = fitted(ppt.lm)
residsppt = resid(ppt.lm)
plot(precip)
lines(fitppt,col='red')
summary(ppt.lm)

# =====================================================================================
# resids
plot(residsppt)
ppt.trend = lm(precip[1:404]~xc[1:404]+xs[1:404]+Time[1:404]+as.vector(enso.w))
plot(precip)
lines(fitted(ppt.trend),col='red')
summary(ppt.trend)

# ======================================================================================
# model seasonal using just mean of the 12 months
jan = weathframe[weathframe$month=='1',3]
feb = weathframe[weathframe$month=='2',3]
mar = weathframe[weathframe$month=='3',3]
apr = weathframe[weathframe$month=='4',3]
may = weathframe[weathframe$month=='5',3]
jun = weathframe[weathframe$month=='6',3]
jul = weathframe[weathframe$month=='7',3]
aug = weathframe[weathframe$month=='8',3]
sep = weathframe[weathframe$month=='9',3]
oct = weathframe[weathframe$month=='10',3]
nov = weathframe[weathframe$month=='11',3]
dec = weathframe[weathframe$month=='12',3]

mmean = c(mean(jan,na.rm=T),
          mean(feb,na.rm=T), 
          mean(mar,na.rm=T),
          mean(apr,na.rm=T),
          mean(may,na.rm=T),
          mean(jun,na.rm=T),
          mean(jul,na.rm=T),
          mean(aug,na.rm=T),
          mean(sep,na.rm=T),
          mean(oct,na.rm=T),
          mean(nov,na.rm=T),
          mean(dec,na.rm=T))

seasonal = rep(mmean,34)

seasonal.lm = lm(precip[1:408]~Time[1:408]+seasonal)
plot(precip)
lines(fitted(seasonal.lm),col='red')
summary(seasonal.lm)
