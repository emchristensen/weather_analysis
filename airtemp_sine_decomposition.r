# This script explores modeling air temp as a sine


library(TTR)


# ===================================================================================
# monthly analysis
weathframe = read.csv("data/Monthly_ppt_1980_present_patched.csv")
maxtemp.ts = ts(weathframe$maxtemp,start=c(1980,1),end=c(2013,10),freq=12)
maxtemp.ap = na.approx(maxtemp.ts)

mintemp.ts = ts(weathframe$mintemp,start=c(1980,1),end=c(2013,10),freq=12)
mintemp.ap = na.approx(mintemp.ts)

sumprecip.ts = ts(weathframe$sumprecip,start=c(1980,1),end=c(2013,10),freq=12)
sumprecip.ap = na.approx(sumprecip.ts)

# fit sine to temperature -------------------------------------------------------------
Time = seq(length(maxtemp.ap))

xc<-cos(2*pi*Time/12)
xs<-sin(2*pi*Time/12)
fit.lm <- lm(maxtemp.ts~xs+xc)

fit <- fitted(fit.lm)  

pred <- predict(fit.lm, newdata=data.frame(Time=Time))  

plot(Time,maxtemp.ap,xlim=c(1,100))
lines(Time,pred,col='blue')

fit.min = lm(mintemp.ap~xs+xc)
pred.min = predict(fit.min,newdata=data.frame(Time=Time))
plot(Time,mintemp.ap)
lines(Time,pred.min,col='blue')

# find residuals -------------------------------------------------------
resids = maxtemp.ap-pred
plot(resids)
abline(h=0,col='red')

resids.smooth = SMA(resids,12)
plot(resids.smooth)

# fit sine to precip ----------------------------------------------------------

fit.ppt = lm(sumprecip.ap~xs+xc)
predppt = predict(fit.ppt,newdata=data.frame(Time=Time))

plot(Time,sumprecip.ap,xlim=c(1,100))
lines(Time,predppt,col='blue')

# ==================================================================================
# Daily weather
daily = read.csv('data/Daily_weather_1980_present_fixed_withgaps.csv')
# remove leap days so the model can handle it
noleap = daily[!(daily$Month==2 & daily$Day==29),]
noleap = noleap[!noleap$Year == 2015,]
maxt.ts= ts(noleap$TempAirMax,start=c(1980,1,1),end=c(tail(daily$Year,1),tail(daily$Month,1),tail(daily$Day,1)),freq=365)
maxt.ap = na.approx(maxt.ts)

mint.ts = ts(noleap$TempAirMin,start=c(1980,1,1),end=c(tail(daily$Year,1),tail(daily$Month,1),tail(daily$Day,1)),freq=365)
mint.ap = na.approx(mint.ts)

# fit sine to temperature ---------------------------------------------------------
Time = seq(length(maxt.ap))

xc<-cos(2*pi*Time/365)
xs<-sin(2*pi*Time/365)

fit.lm <- lm(maxt.ap~xs+xc)
fit <- fitted(fit.lm)  
pred <- predict(fit.lm, newdata=data.frame(Time=Time))  

fit.min = lm(mint.ap~xs+xc)
pred.min = predict(fit.min,newdata=data.frame(Time=Time))
max_sin = data.frame(Year = noleap$Year,Month = noleap$Month,Day = noleap$Day,
                     SinMaxT = pred[1:length(noleap$Day)],
                     SinMinT = pred.min[1:length(noleap$Day)])


plot(Time,maxt.ap,xlim=c(1,500))
lines(Time,pred,col='blue')

fit.min = lm(mint.ap~xs+xc)
pred.min = predict(fit.min,newdata=data.frame(Time=Time))
plot(Time,mint.ap)
lines(Time,pred.min,col='blue')

# find residuals -------------------------------------------------------
resids = maxt.ap-pred
plot(resids,xlim=c(1980,1982))
abline(h=0,col='red')

resids.smooth = SMA(resids,12)
plot(resids.smooth)
