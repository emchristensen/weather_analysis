# This script takes the old daily weather data (Daily_weather_1980_89.csv) and combines it with the
# newer weather station data (Hourly_PPT_mm_1989_present_fixed.csv) and creates a suite of regressor
# variables
# Written by EMC 3/2/15
#
# Input file:
#     Daily_weather_1980_present_fixed_patched.csv
#         Year, Month, Day, Precipitation, TempAirMax, TempAirMin, patch
#         this data is created (updated) by the script weather_daily_summary.r
#         fixed means I adjusted for the 1997 weather station hiccup
#         patched means I used Portal4sw and SanSimon data to estimate the holes in the Portal weather station data
#     
# Output file:
#     all_abiotic_vars.csv
#         year, month, yrmo, sumprecip, maxtemp, mintemp, wet_frac, wet, dry, freeze, sumprecip3, wetfrac3, freeze3,
#             sumprecip6, wetfrac6, freeze6, sumprecip12, wetfrac12, freeze12
#         wet_frac = fraction of days in month with rain
#         freeze = number of days in month where temp got below 0
#         sumprecip3 = sum of precip in past 3 months (6 months, 12 months)

# ===============================================================================================
# functions

monthly_summary_from_daily = function(dataframe) {
  #takes hourly daily from portal and produces monthly total/avg
  sumprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=sum)
  maxtemp = aggregate(dataframe$TempAirMax,by=list(dataframe$Month,dataframe$Year),FUN=max)
  mintemp = aggregate(dataframe$TempAirMin,by=list(dataframe$Month,dataframe$Year),FUN=min)
  
  month = sumprecip$Group.1
  year = sumprecip$Group.2
  sumprecip = sumprecip$x
  maxtemp = maxtemp$x
  mintemp = mintemp$x
  
  return(data.frame(year,month,sumprecip,maxtemp,mintemp))
}

wet_days = function(daily) {
  # returns fraction of days with rain in a month
  wet = daily[daily$Precipitation!=0,]
  wet = wet[!is.na(wet$Month),]
  nwet = aggregate(wet$Month,by=list(wet$Month,wet$Year),FUN=length)
  dry = daily[daily$Precipitation==0,]
  dry = dry[!is.na(dry$Month),]
  ndry = aggregate(dry$Month,by=list(dry$Month,dry$Year),FUN=length)
  
  wetdry = merge(nwet,ndry,by=c('Group.1','Group.2'),all=T)
  wetdry$x.x[is.na(wetdry$x.x)] = 0
  wetdry$x.y[is.na(wetdry$x.y)] = 0
  month = wetdry$Group.1
  year = wetdry$Group.2
  wet_frac = wetdry$x.x/(wetdry$x.x+wetdry$x.y)
  
  return(data.frame(yrmo = paste(year,month),wet_frac,wet=wetdry$x.x,dry=wetdry$x.y))
}

freeze_days = function(daily) {
  # returns number of days below freezing
  freeze = daily[daily$TempAirMin<0,]
  freeze = freeze[!is.na(freeze$Month),]
  nfreeze = aggregate(freeze$Month,by=list(freeze$Month,freeze$Year),FUN=length)

  names(nfreeze) = c('month','year','freeze')
  return(nfreeze)
}

# ================================================================================================
# read in data

daily = read.csv('data/Daily_weather_1980_present_fixed_patched.csv')

# =================================================================================================
# aggregate into monthly summary

monthly = monthly_summary_from_daily(daily)
monthly$yrmo = paste(monthly$year,monthly$month)

wet_frac = wet_days(daily)

monthly = merge(monthly,wet_frac,by=c('yrmo'),all=T)
monthly = monthly[order(monthly$year,monthly$month),]
monthly$wet_frac[is.na(monthly$sumprecip)] = NA         # make sure where precip is NA, wet/dry fraction is also NA
monthly$wet[is.na(monthly$sumprecip)] = NA
monthly$dry[is.na(monthly$sumprecip)] = NA

freeze = freeze_days(daily)
monthly = merge(monthly,freeze,by=c('year','month'),all=T)
monthly$freeze[is.na(monthly$freeze)] = 0
monthly$freeze[is.na(monthly$maxtemp)] = NA

# ================================================================================================
# aggregate into 3-month chunks

monthly$sumprecip3 = rep(NA)
monthly$wetfrac3 = rep(NA)
monthly$freeze3 = rep(NA)

for (n in 3:length(monthly$yrmo)) {
  monthly$sumprecip3[n] = sum(monthly$sumprecip[(n-2):n])
  monthly$wetfrac3[n] = sum(monthly$wet[(n-2):n])/(sum(monthly$wet[(n-2):n])+sum(monthly$dry[(n-2):n]))
  monthly$freeze3[n] = sum(monthly$freeze[(n-2):n])
}

# ==================================================================================================
# aggregate into 6-month chunks

monthly$sumprecip6 = rep(NA)
monthly$wetfrac6 = rep(NA)
monthly$freeze6 = rep(NA)

for (n in 6:length(monthly$yrmo)) {
  monthly$sumprecip6[n] = sum(monthly$sumprecip[(n-5):n])
  monthly$wetfrac6[n] = sum(monthly$wet[(n-5):n])/(sum(monthly$wet[(n-5):n])+sum(monthly$dry[(n-5):n]))
  monthly$freeze6[n] = sum(monthly$freeze[(n-5):n])
}

# ==================================================================================================
# 1 year

monthly$sumprecip12 = rep(NA)
monthly$wetfrac12 = rep(NA)
monthly$freeze12 = rep(NA)

for (n in 12:length(monthly$yrmo)) {
  monthly$sumprecip12[n] = sum(monthly$sumprecip[(n-11):n])
  monthly$wetfrac12[n] = sum(monthly$wet[(n-11):n])/(sum(monthly$wet[(n-11):n])+sum(monthly$dry[(n-11):n]))
  monthly$freeze12[n] = sum(monthly$freeze[(n-11):n])
}

# =================================================================================================
# write to csv
write.csv(monthly,file='data/all_abiotic_vars.csv',row.names=F)
