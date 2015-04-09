# This script takes the old daily weather data (Monthly_ppt_1980_present.csv) and combines it with the
# newer weather station data (Hourly_PPT_mm_1989_present_fixed.csv) and writes a new file of yearly 
# total precipitation.
# Written by EMC 12/6/14
#
# Input:
#    Monthly_ppt_1980_present_patched.csv
#       year, month, sumprecip, maxtemp, mintemp
#
# Output:
#    Yearly_ppt_1980_present.csv
#        year, totalppt
#
# Scripts called:
#     patch_portal_weather.r        creates input file Monthly_ppt_1980_present_patched.csv
#
# Secondary scripts required:
#     find_gaps_weather_data.r
#     weather_monthly_summary.R
#     patch_portal_weather.r
#     portal_4sw_weather.r
#     clean_NOAA_data.r

################################################################################################
# call scripts for input data
################################################################################################
source('portal_weather/patch_portal_weather.r')

################################################################################################
# Load data and process
################################################################################################
dataframe = read.csv("data/Monthly_ppt_1980_present_patched.csv")

# Yearly summary
sumprecip = aggregate(dataframe$sumprecip,
                      by=list(dataframe$year),
                      FUN=sum,
                      na.rm=T)
yearly = data.frame(year = sumprecip$Group.1, totalppt = sumprecip$x)

write.csv(yearly,file="data/Yearly_ppt_1980_present.csv",row.names=F)

# Clear workspace
rm(list=ls())