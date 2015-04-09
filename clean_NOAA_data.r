# This script contains two functions for cleaning up downloaded NOAA weather station 
# data (Portal4sw and SanSimon)


find_missing_dates = function(weathframe) {
  # This function finds gaps in daily ppt/temp data
  date_info = with(weathframe,paste(Year, Month, Day,sep='-'))
  dates = as.Date(date_info)
  d1 = head(dates,1)
  d2 = tail(dates,1)
  datesthereshouldbe = as.character(seq(d1,d2,by='day'))
  missingdates = setdiff(datesthereshouldbe,as.character(dates))
  missingdates = as.Date(missingdates)
  
  # Create frame of NAs for missing dates
  TempAirMax = rep(NA,length(missingdates))
  TempAirMin = rep(NA,length(missingdates))
  Precipitation = rep(NA,length(missingdates))
  Year = as.integer(format(missingdates, '%Y'))
  Month = as.integer(format(missingdates, '%m'))
  Day = as.integer(format(missingdates, '%d'))
  missingframe = data.frame(Year,Month,Day,Precipitation,TempAirMax,TempAirMin)
  
  return(missingframe)
}


clean_NOAA_data = function(filename) {
  # This function takes the raw data downloaded from http://www.ncdc.noaa.gov/cdo-web/
  # and puts it into a usable data frame
  dataframe = read.csv(filename)
  
  dates = strptime(dataframe$DATE,'%Y%m%d')
  Year = as.integer(format(dates,'%Y'))
  Month = as.integer(format(dates,'%m'))
  Day = as.integer(format(dates,'%d'))
  Precipitation = dataframe$PRCP/10       # convert from tenths of mm to mm
  Precipitation[Precipitation<0] = NA     # NAs originally recorded as -9999
  TempAirMax = dataframe$TMAX/10          # convert from tenths of degrees C to degrees C
  TempAirMax[TempAirMax< -900] = NA
  TempAirMin = dataframe$TMIN/10          # convert from tenths of degrees C to degrees C
  TempAirMin[TempAirMin< -900] = NA
  
  rawframe = data.frame(Year,Month,Day,Precipitation,TempAirMax,TempAirMin)
  
  # find missing dates (should be daily data) and combine with present data
  missing = find_missing_dates(rawframe)
  weathframe = rbind(rawframe,missing)
  weathframe = weathframe[order(weathframe[,1],weathframe[,2],weathframe[,3]),]
  
  return(weathframe)
}
