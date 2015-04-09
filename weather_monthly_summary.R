# This script takes daily weather data summarized from original files and writes a new file of monthly 
# total and mean precip, and max and min temp.
# Written by EMC 8/7/14
#
# Input:
#   Daily_weather_1980_present_fixed_withgaps.csv
#       Year, Month, Day, Precipitation, TempAirMax,TempAirMin
#
# Output:
#     Monthly_ppt_1980_present.csv
#         year, month, sumprecip, maxtemp, mintemp
#
# Scripts called:
#     find_gaps_weather_data.r    creates input file Hourly_PPT_mm_1989_present_fixed_withgaps.csv
#     weather_daily_summary.r     combines hourly dataset (1989-present) and daily dataset (1980-89)
  

# set working directory and run prelim scripts
setwd('C:/Users/EC/git_dir/')
source('portal_weather/weather_daily_summary.r')

#####################################################################################################
# Define functions
#####################################################################################################

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

###############################################################################################
# Read in files and run functions
###############################################################################################

daily = read.csv('data/Daily_weather_1980_present_fixed_withgaps.csv')

# monthly summary
monthly = monthly_summary_from_daily(daily)

# write to csv file
write.csv(monthly,file="data/Monthly_ppt_1980_present.csv",row.names=F)

# Clear workspace
rm(list=ls())
