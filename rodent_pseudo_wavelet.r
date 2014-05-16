#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')
source('portal_weather/period_to_monthly_ts.r')

library(TTR)

# =================================================================================
# import raw data
# ---------------------------------------------------------------------------------
dat = read.csv("data/Rodents.csv", as.is = TRUE,  colClasses = c(note1='character'))
# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"
# just control plots
dat = dat[dat$plot %in% c(1,2,4,8,9,11,12,14,17,22),]

# ==================================================================================
# convert raw database rodent file to regularly-spaced timeseries
# ---------------------------------------------------------------------------------

raw_to_ts = function(dat) {
  # aggregate data by month.  there will be some NAs
  dat_monthly = period_to_ts(dat)
  
  start = head(dat_monthly$date,1)
  end = tail(dat_monthly$date,1)
  rodents.ts = ts(dat_monthly$x,
                  start=c(as.integer(format(start,'%Y')),as.integer(format(start,'%m'))),
                  end=c(as.integer(format(end,'%Y')),as.integer(format(end,'%m'))),
                  freq=12)
  
  # fill in NAs
  rodents.ap = na.approx(rodents.ts)
  return(rodents.ap)
}

# ================================================================================
# create timeseries for subsets of rodent data
rodents.ap = raw_to_ts(dat)
pp.ap = raw_to_ts(dat[dat$species == 'PP',])
dm.ap = raw_to_ts(dat[dat$species == 'DM',])

# ================================================================================
# smoothing plots
smoothfactor = 12

rodents.smooth = SMA(rodents.ap,smoothfactor)
plot(rodents.smooth,xlab='',ylab='rodent abundance',main=paste('n =',smoothfactor))

pp.smooth = SMA(pp.ap,smoothfactor)
plot(pp.smooth,xlab='',ylab='PP abundance',main=paste('n =',smoothfactor))

dm.smooth = SMA(dm.ap,smoothfactor)
plot(dm.smooth,xlab='',ylab='DM abundance',main=paste('n =',smoothfactor))

# decompose ---------------------------------------------------------------------
rodents.decom = decompose(rodents.ap)
plot(rodents.decom)
rodents.hw = HoltWinters(rodents.ap)
plot(rodents.hw$fitted)

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
