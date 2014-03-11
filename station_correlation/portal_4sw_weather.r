#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

find_missing_dates_4sw = function(weathframe) {
  # this function finds gaps in daily ppt data
  date_info = with(weathframe,paste(Year, Month, Day,sep='-'))
  dates = as.Date(date_info)
  d1 = dates[1]
  d2 = dates[length(dates)]
  datesthereshouldbe = as.character(seq(d1,d2,by='day'))
  missingdates = setdiff(datesthereshouldbe,as.character(dates))
  return(missingdates)
}

create_missingframe_4sw = function(missingdates) {
  # this function creates a dataframe of the dates for which weather data is missing, with NA for weather data
  
  TempAirMax = rep(NA,length(missingdates))
  TempAirMin = rep(NA,length(missingdates))
  Precipitation = rep(NA,length(missingdates))
  
  Year = as.integer(format(missingdates, '%Y'))
  Month = as.integer(format(missingdates, '%m'))
  Day = as.integer(format(missingdates, '%d'))
  
  missingframe = data.frame(Year,Month,Day,Precipitation,TempAirMax,TempAirMin)
  return(missingframe)
}

combine_two_frames_4sw = function(weathframe,missingframe) {
  #combines the dataframe of weather data with the dataframe of missing weather data
  Year = c(weathframe$Year,missingframe$Year)
  Month = c(weathframe$Month,missingframe$Month)
  Day = c(weathframe$Day,missingframe$Day)
  TempAirMax = c(weathframe$TempAirMax,missingframe$TempAirMax)
  TempAirMin = c(weathframe$TempAirMin,missingframe$TempAirMin)
  Precipitation = c(weathframe$Precipitation,missingframe$Precipitation)
  
  withgaps = data.frame(Year,Month,Day,TempAirMax,TempAirMin,Precipitation)
  return(withgaps[order(withgaps[,1],withgaps[,2],withgaps[,3]),])
}

monthly_summary_from_daily_4sw = function(dataframe) {
  #takes daily weather data and produces monthly total and monthly avg
  sumprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=sum)
  meanprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  maxtemp = aggregate(dataframe$TempAirMax,by=list(dataframe$Month,dataframe$Year),FUN=max)
  mintemp = aggregate(dataframe$TempAirMin,by=list(dataframe$Month,dataframe$Year),FUN=min)
  
  month = sumprecip$Group.1
  year = sumprecip$Group.2
  sumprecip = sumprecip$x
  meanprecip = meanprecip$x*10/24 #convert from mm/day to mm/hr
  maxtemp = maxtemp$x
  mintemp = mintemp$x
  
  return(data.frame(month,year,sumprecip,meanprecip,maxtemp,mintemp))
}

monthly_portal4sw = function(dataframe) {
  # this function takes the raw data from the Portal 4sw station and reduces it to monthly data
  dates = strptime(dataframe$DATE,'%Y%m%d')
  Year = as.integer(format(dates,'%Y'))
  Month = as.integer(format(dates,'%m'))
  Day = as.integer(format(dates,'%d'))
  Precipitation = dataframe$PRCP/10 #tenths of mm
  Precipitation[Precipitation<0] = NA
  TempAirMax = dataframe$TMAX/10 #tenths of degrees C
  TempAirMax[TempAirMax< -900] = NA
  TempAirMin = dataframe$TMIN/10
  TempAirMin[TempAirMin< -900] = NA
  weathframe = data.frame(Year,Month,Day,Precipitation,TempAirMax,TempAirMin)
  missingdates = find_missing_dates_4sw(weathframe)
  missingframe = create_missingframe_4sw(as.Date(missingdates))
  withgaps = combine_two_frames_4sw(weathframe,missingframe)
  monthly_4sw = monthly_summary_from_daily_4sw(withgaps)
  
  outfile = "data/Monthly_ppt_Portal4sw.csv"
  write.csv(monthly_4sw,file=outfile,row.names=F)
}

portal4swfile = "data/Portal4sw_ppt.csv"
portal4swframe = csv_to_dataframe(portal4swfile)
monthly_portal4sw(portal4swframe)
