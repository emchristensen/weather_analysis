#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

weathfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
weathframe = csv_to_dataframe(weathfile)
weathframe$date = as.Date(paste(weathframe$Year,weathframe$Month,weathframe$Day,sep='-'))

yearly = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=mean)
plot(yearly[2:25,],xlab='',ylab='mean annual air temp')
lines(yearly[2:25,])

monthly = aggregate(weathframe$TempAir,by=list(weathframe$Month,weathframe$Year),FUN=mean)
plot(monthly$x[monthly$Group.1==1],xlab='',ylab='avg Jan temp')

yearmin = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=min)
yearmax = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=max)
plot(yearmin$Group.1[2:25],yearmax$x[2:25]-yearmin$x[2:25],xlab='',ylab='temp range')
