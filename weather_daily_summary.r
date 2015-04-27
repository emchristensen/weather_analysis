# This script takes the old weather data (daily 1980 - 1989) and the new weather data (hourly 1989 - present)
# and combines into one complete set of daily data.  
# Written by EMC 3/5/15
#
# Input:
#   Hourly_PPT_mm_1989_present_fixed_withgaps.csv
#       Year, Month, Day, Hour, TempAir, RelHumid, TempSoil, Precipitation, Uncert_level
#   Daily_weather_1980_90.csv
#       Year, Month, Day, TempAirAvg, TempAirMax, TempAirMin, RH Avg, RH Max, RH Min, TempSoilAvg, TempSoilMax, TempSoilMin, Precipitation
#
# Output:
#   Daily_weather_1980_present_fixed_withgaps.csv
#       Year, Month, Day, Precipitation, TempAirMax,TempAirMin
#
# Scripts called:
#    find_gaps_weather_data.r       takes original hourly data file and puts NAs as placeholders for missing data
# 

source('weather_analysis/find_gaps_weather_data.r')

oldwfile = 'C:/Users/EC/git_dir/original_data/Daily_weather_1980_89.csv'
newwfile = 'C:/Users/EC/git_dir/data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv'

# ===========================================================================================
# daily summary function

daily_summary_from_hourly = function(dataframe) {
  #takes hourly data from portal (1989-present) and produces daily total and daily avg
  
  sumprecip = aggregate(dataframe$Precipitation,
                        by=list(dataframe$Day,dataframe$Month,dataframe$Year),
                        FUN=sum)
  maxtemp = aggregate(dataframe$TempAir,
                      by=list(dataframe$Day,dataframe$Month,dataframe$Year),
                      FUN=max)
  mintemp = aggregate(dataframe$TempAir,
                      by=list(dataframe$Day,dataframe$Month,dataframe$Year),
                      FUN=min)
  Day = sumprecip$Group.1
  Month = sumprecip$Group.2
  Year = sumprecip$Group.3
  Precipitation = sumprecip$x
  TempAirMax = maxtemp$x
  TempAirMin = mintemp$x
  
  return(data.frame(Year,Month,Day,Precipitation,TempAirMax,TempAirMin))
}

# ===========================================================================================
# create daily weather dataset

oldwframe = read.csv(oldwfile)
hourly_withgaps = read.csv(newwfile)

# re-arrange old data to match new
daily1 = data.frame(Year = oldwframe$Year,
                    Month = oldwframe$Month,
                    Day = oldwframe$Day,
                    Precipitation = oldwframe$Precipitation*10,
                    TempAirMax = oldwframe$TempAirMax,
                    TempAirMin = oldwframe$TempAirMin)
# daily summary of new hourly data
daily2 = daily_summary_from_hourly(hourly_withgaps)
  
daily1 = daily1[1:3442,]    #remove days where 2 stations overlap - keeping data from newer station
daily = rbind(daily1,daily2)

# write to csv
write.csv(daily,file="data/Daily_weather_1980_present_fixed_withgaps.csv",row.names=F)

# Clear workspace
rm(list=ls())
