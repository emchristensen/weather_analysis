# This script takes the monthly summary data (output of weather_monthly_summary.R) and uses the
# Portal4sw weather station data to patch the holes.
# Written by EMC 8/7/14
#
# Input: 
#     Monthly_ppt_1980_present.csv
#         year, month, sumprecip, maxtemp, mintemp
#     Monthly_ppt_Portal4sw.csv                                   output of portal_4sw_weather.r
#         month, year, sumprecip, meanprecip, maxtemp, mintemp
#     Monthly_ppt_SanSimon.csv                                    output of sansimon_weather.r
#         month, year, sumprecip, meanprecip, maxtemp, mintemp
#           
# Output:
#     Monthly_ppt_1980_present_patched.csv
#         year, month, sumprecip, maxtemp, mintemp, patch, pptpatch
#
# Scripts called:  
#     weather_monthly_summary.R     creates input file Monthly_ppt_1980_present.csv
#
# Secondary scripts required:
#     find_gaps_weather_data.r
#     clean_NOAA_data.r
#     

#####################################################################################################
# call scripts for creating necessary input files
#####################################################################################################
source('portal_weather/weather_monthly_summary.R')

portal = read.csv("data/Monthly_ppt_1980_present.csv")
p4sw   = read.csv("data/Monthly_ppt_Portal4sw.csv")
sans   = read.csv("data/Monthly_ppt_SanSimon.csv")

#####################################################################################################
# use linear model to estimate portal temp/ppt based on portal 4sw station (compare_stations_monthly.r)
#####################################################################################################
p4sw_mod = data.frame(year = p4sw$year, month = p4sw$month)
p4sw_mod$maxtemp = 1.35085 + 1.08319 * p4sw$maxtemp
p4sw_mod$mintemp = 6.44628 + 1.02776 * p4sw$mintemp
p4sw_mod$sumprecip = 3.36144 + 0.45307 * p4sw$sumprecip

sans_mod = data.frame(year = sans$year, month = sans$month)
sans_mod$maxtemp = 1.05168 + 0.92275 * sans$maxtemp
sans_mod$mintemp = 3.15418 + 0.95564 * sans$mintemp
sans_mod$sumprecip = 5.92341 + 0.70195 * sans$sumprecip

models = merge(portal,sans_mod,by=c('year','month'),all.x=T)
models = merge(models,p4sw_mod,by=c('year','month'),all.x=T)

#####################################################################################################
# replace NAs with model-derived estimates
#####################################################################################################
# patch NAs -- san simon gets precedence for temp because the r^2 was slightly better
models$patch = rep(0)
for (n in 1:length(models$year)) {
  if (is.na(models$maxtemp.x[n])) {
    if (is.na(models$maxtemp.y[n])) {
      models$maxtemp.x[n] = models$maxtemp[n]
      models$patch[n] = 1
    }
    else {
      models$maxtemp.x[n] = models$maxtemp.y[n]
      models$patch[n] = 1
    }
  }
  if (is.na(models$mintemp.x[n])) {
    if (is.na(models$mintemp.y[n])) {
      models$mintemp.x[n] = models$mintemp[n]
      models$patch[n] = 1
    }
    else {
      models$mintemp.x[n] = models$mintemp.y[n]
      models$patch[n] = 1
    }
  }
}

# patch precip -- portal4sw has higher r^2
models$pptpatch = rep(0)
for (n in 1:length(models$year)) {
  if (is.na(models$sumprecip.x[n])) {
    if (is.na(models$sumprecip[n])) {
      models$sumprecip.x[n] = models$sumprecip.y[n]
      models$pptpatch[n] = 1
    }
    else {
      models$sumprecip.x[n] = models$sumprecip[n]
      models$pptpatch[n] = 1
    }
  }
}

#####################################################################################################
# save patched data as new csv file
#####################################################################################################
portalpatched = models[,c(1:5,12,13)]
names(portalpatched) = c('year','month','sumprecip','maxtemp','mintemp','patch','pptpatch')
write.csv(portalpatched,file="data/Monthly_ppt_1980_present_patched.csv",row.names=F)

# Clear workspace
rm(list=ls())
