# This script takes daily summary data from the on-site weather station and patches it from
# data from two nearby stations, Portal4sw and SanSimon
# Written by EMC 3/5/15
#
# Input:
#   Daily_weather_1980_present_fixed_withgaps.csv
#       Year, Month, Day, Precipitation, TempAirMax,TempAirMin
#
# Output:
#   Daily_weather_1980_present_fixed_patched.csv
#       Year, Month, Day, Precipitation, TempAirMax.x, TempAirMin.x, patch
#
# Scripts called:
#    clean_NOAA_data.r          cleans up data downloaded from NOAA stations for use
# 


source('portal_weather/clean_NOAA_data.r')

# ==========================================================================================
# read in data

daily = read.csv('data/Daily_weather_1980_present_fixed_withgaps.csv')
p4sw = clean_NOAA_data('original_data/Portal4sw_ppt.csv')
sans = clean_NOAA_data('original_data/SanSimon_ppt.csv')

# ==========================================================================================
# models

# approximation of min and max temperature at the site, derived from linear models contained in compare_stations_daily.r

p4sw_mod = data.frame(Year = p4sw$Year, Month = p4sw$Month, Day = p4sw$Day)
p4sw_mod$TempAirMax = 1.861750 + 1.054005 * p4sw$TempAirMax
p4sw_mod$TempAirMin = 6.714013 + 0.886423 * p4sw$TempAirMin

sans_mod = data.frame(Year = sans$Year, Month = sans$Month, Day = sans$Day)
sans_mod$TempAirMax = 0.642443 + 0.917132 * sans$TempAirMax
sans_mod$TempAirMin = 3.990803 + 0.839019 * sans$TempAirMin


models = merge(daily,sans_mod,by=c('Year','Month','Day'),all.x=T)
models = merge(models,p4sw_mod,by=c('Year','Month','Day'),all.x=T)

# =====================================================================================
# plots to check out models
#plot(models$TempAirMax.x,xlim = c(3000,3100))
#points(models$TempAirMax.y,col='blue')
#points(models$TempAirMax,col='red')

# patch NAs -- san simon gets precedence because the r^2 was slightly better
models$patch = rep(0)
for (n in 1:length(models$Year)) {
  if (is.na(models$TempAirMax.x[n])) {
    if (is.na(models$TempAirMax.y[n])) {
      models$TempAirMax.x[n] = models$TempAirMax[n]
      models$patch[n] = 1
    }
    else {
      models$TempAirMax.x[n] = models$TempAirMax.y[n]
      models$patch[n] = 1
    }
  }
  if (is.na(models$TempAirMin.x[n])) {
    if (is.na(models$TempAirMin.y[n])) {
      models$TempAirMin.x[n] = models$TempAirMin[n]
      models$patch[n] = 1
    }
    else {
      models$TempAirMin.x[n] = models$TempAirMin.y[n]
      models$patch[n] = 1
    }
  }
}

# write to csv
write.csv(models[,c(1:6,11)],file="data/Daily_weather_1980_present_fixed_patched.csv",row.names=F)

# Clear workspace
rm(list=ls())
