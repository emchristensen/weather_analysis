#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

create_missingframe = function(missingdates) {
  # this function creates a dataframe of the dates for which weather data is missing, with NA for weather data
  
  TempAir = rep(NA,length(missingdates))
  RelHumid = rep(NA,length(missingdates))
  TempSoil = rep(NA,length(missingdates))
  Precipitation = rep(NA,length(missingdates))
  Uncert_level = rep(1,length(missingdates))
  
  dates = as.Date(missingdates)
  hr = as.integer(format(missingdates,'%H'))
  Hour = hr*100
  midnight = Hour==0
  Hour[midnight] = 2400
  dates[midnight] = dates[midnight]-1
  
  Year = as.integer(format(dates, '%Y'))
  Month = as.integer(format(dates, '%m'))
  Day = as.integer(format(dates, '%d'))
  
  missingframe = data.frame(Year,Month,Day,Hour,TempAir,RelHumid,TempSoil,Precipitation,Uncert_level)
  return(missingframe)
}

find_missing_dates = function(weathframe) {
  # this function finds gaps in hourly ppt data
  date_info = with(weathframe,paste(Year, Month, Day, Hour/100))
  dates = strptime(date_info, '%Y %m %d %H')
  d1 = dates[1]
  d2 = dates[length(dates)]
  datesthereshouldbe = as.character(seq(d1,d2,by='hour'))
  missingdates = setdiff(datesthereshouldbe,as.character(dates))
  return(missingdates)
}

combine_two_frames = function(weathframe,missingframe) {
  #combines the dataframe of weather data with the dataframe of missing weather data
  Year = c(weathframe$Year,missingframe$Year)
  Month = c(weathframe$Month,missingframe$Month)
  Day = c(weathframe$Day,missingframe$Day)
  Hour = c(weathframe$Hour,missingframe$Hour)
  TempAir = c(weathframe$TempAir,missingframe$TempAir)
  RelHumid = c(weathframe$RelHumid,missingframe$RelHumid)
  TempSoil = c(weathframe$TempSoil,missingframe$TempSoil)
  Precipitation = c(weathframe$Precipitation,missingframe$Precipitation)
  
  withgaps = data.frame(Year,Month,Day,Hour,TempAir,RelHumid,TempSoil,Precipitation)
  return(withgaps[order(withgaps[,1],withgaps[,2],withgaps[,3],withgaps[,4]),])
}

weathfile = "data/Hourly_PPT_mm_1989_present_fixed.csv"
weathframe = csv_to_dataframe(weathfile)

missingdates = find_missing_dates(weathframe)

missingframe = create_missingframe(strptime(missingdates,'%Y-%m-%d %H:%M:%S'))

withgaps = combine_two_frames(weathframe,missingframe)

outfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
write.csv(withgaps,file=outfile,row.names=F)