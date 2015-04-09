# This script takes  the newer weather station data (Hourly_PPT_mm_1989_present_fixed.csv) 
# and writes a new file with NAs added where the weather station is missing data points.
# Written by EMC 8/13/14
#
# Input:
#   Hourly_PPT_mm_1989_present_fixed.csv
#       Year, Month, Day, Hour, TempAir, RelHumid, TempSoil, Precipitation, Uncert_level
#       This data has been altered from the original to correct the timestamp error that occurred in 1997: see the document 1997 temp problem for more info
#       Make sure you are using the most updated version of this file!
#
# Output:
#   Hourly_PPT_mm_1989_present_fixed_withgaps.csv
#       Year, Month, Day, Hour, TempAir, RelHumid, TempSoil, Precipitation, Uncert_level
#
# Scripts called:
#    
# 


########################################################################################
# main function
########################################################################################
find_gaps_weather_data = function(weathfile) {
  # function to find gaps in latest version of hourly data and fill gaps with NAs
  weathframe = read.csv(weathfile)
  
  # finds gaps in hourly ppt data
  date_info = with(weathframe,paste(Year, Month, Day, Hour/100))
  dates = strptime(date_info, '%Y %m %d %H')
  d1 = dates[1]
  d2 = dates[length(dates)]
  datesthereshouldbe = as.character(seq(d1,d2,by='hour'))
  missingdates = setdiff(datesthereshouldbe,as.character(dates))
  missingdates = strptime(missingdates,'%Y-%m-%d %H:%M:%S')
  
  # create a dataframe of the hours for which weather data is missing, with NA for weather data
  TempAir = rep(NA,length(missingdates))
  RelHumid = rep(NA,length(missingdates))
  TempSoil = rep(NA,length(missingdates))
  Precipitation = rep(NA,length(missingdates))
  Uncert_level = rep(1,length(missingdates))
  
  mdates = as.Date(missingdates)
  hr = as.integer(format(missingdates,'%H'))
  Hour = hr*100
  midnight = Hour==0
  Hour[midnight] = 2400
  mdates[midnight] = mdates[midnight]-1
  
  Year = as.integer(format(mdates, '%Y'))
  Month = as.integer(format(mdates, '%m'))
  Day = as.integer(format(mdates, '%d'))
  
  missingframe = data.frame(Year,Month,Day,Hour,TempAir,RelHumid,TempSoil,Precipitation,Uncert_level)
  
  #combine the dataframe of weather data with the dataframe of missing weather data
  withgaps = rbind(weathframe,missingframe)
  
  return(withgaps[order(withgaps[,1],withgaps[,2],withgaps[,3],withgaps[,4]),])
}

hourly_withgaps = find_gaps_weather_data('data/Hourly_PPT_mm_1989_present_fixed.csv')

write.csv(hourly_withgaps,file="data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv",row.names=F)

# Clear workspace
rm(list=ls())

