create_dataframe = function(datafile) {
  #converts data in csv file to data frame form
  rawdata = read.csv(paste(datafile,sep=''),head=T,sep=',')
  dataframe = data.frame(rawdata)
  return(dataframe)
}

monthly_summary_from_hourly = function(dataframe) {
  #takes hourly data from portal (1989-present) and produces monthly total and monthly avg
  sumprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=sum)
  meanprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  #meantemp = aggregate(dataframe$TempAir,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  maxtemp = aggregate(dataframe$TempAir,by=list(dataframe$Month,dataframe$Year),FUN=max)
  mintemp = aggregate(dataframe$TempAir,by=list(dataframe$Month,dataframe$Year),FUN=min)
  #meanhumid = aggregate(dataframe$RelHumid,by=list(dataframe$Monh,dataframe$Year),FUN=mean)
  
  month = sumprecip$Group.1
  year = sumprecip$Group.2
  sumprecip = sumprecip$x
  meanprecip = meanprecip$x
  #meantemp = meantemp$x
  maxtemp = maxtemp$x
  mintemp = mintemp$x
  #meanhumid = meanhumid$x
  
  return(data.frame(month,year,sumprecip,meanprecip,maxtemp,mintemp))
}

monthly_summary_from_daily = function(dataframe) {
  #takes hourly daily from portal (1980-1989) and produces monthly total and monthly avg
  sumprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=sum)
  meanprecip = aggregate(dataframe$Precipitation,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  #meantemp = aggregate(dataframe$TempAir,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  maxtemp = aggregate(dataframe$TempAirMax,by=list(dataframe$Month,dataframe$Year),FUN=max)
  mintemp = aggregate(dataframe$TempAirMin,by=list(dataframe$Month,dataframe$Year),FUN=min)
  #meanhumid = aggregate(dataframe$RelHumid,by=list(dataframe$Monh,dataframe$Year),FUN=mean)
  
  month = sumprecip$Group.1
  year = sumprecip$Group.2
  sumprecip = sumprecip$x*10    #1980-1989 data taken in cm
  meanprecip = meanprecip$x*10
  #meantemp = meantemp$x
  maxtemp = maxtemp$x
  mintemp = mintemp$x
  #meanhumid = meanhumid$x
  
  return(data.frame(month,year,sumprecip,meanprecip,maxtemp,mintemp))
}


weathfile = "C://Users//EC//Dropbox//Portal_EC//Hourly_PPT_mm_1989_present_fixed_date.csv"
weathframe = create_dataframe(weathfile)

oldweathfile = "C://Users//EC//Dropbox//Portal_EC//Daily_weather_1980_89.csv"
oldwframe = create_dataframe(oldweathfile)

monthly1 = monthly_summary_from_hourly(weathframe)
monthly2 = monthly_summary_from_daily(oldwframe)

monthly2 = monthly2[1:113,] #remove last two entries so two halves of series match up
year = c(monthly2$year,monthly1$year)
month = c(monthly2$month,monthly1$month)
sumprecip = c(monthly2$sumprecip,monthly1$sumprecip)
monthly = data.frame(year,month,sumprecip)