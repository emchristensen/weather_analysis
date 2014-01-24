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
  #meanhumid = aggregate(dataframe$RelHumid,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  
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
  #meanhumid = aggregate(dataframe$RelHumid,by=list(dataframe$Month,dataframe$Year),FUN=mean)
  
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

combine_two_datasets = function(dataframe1,dataframe2) {
  #combines the two monthly-summary data sets into one
  year = c(dataframe1$year,dataframe2$year)
  month = c(dataframe1$month,dataframe2$month)
  sumprecip = c(dataframe1$sumprecip,dataframe2$sumprecip)
  meanprecip = c(dataframe1$meanprecip,dataframe2$meanprecip)
  maxtemp = c(dataframe1$maxtemp,dataframe2$maxtemp)
  mintemp = c(dataframe1$mintemp,dataframe2$mintemp)
  
  return(data.frame(year,month,sumprecip,meanprecip,maxtemp,mintemp))
}

weathfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
weathframe = create_dataframe(weathfile)

oldweathfile = "data/Daily_weather_1980_89.csv"
oldwframe = create_dataframe(oldweathfile)

monthly1 = monthly_summary_from_daily(oldwframe)
monthly2 = monthly_summary_from_hourly(weathframe)

monthly1 = monthly1[1:113,] #remove last two entries so two halves of series match up

monthly = combine_two_datasets(monthly1,monthly2)

outfile = "data/Monthly_ppt_1980_2013.csv"
write.csv(monthly,file=outfile,row.names=F)