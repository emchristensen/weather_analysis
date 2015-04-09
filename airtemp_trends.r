# This script analyzes
#
#
# Input file:
#     Hourly_PPT_mm_1989_present_fixed_withgaps.csv
#         Year, Month, Day, Hour, TempAir, RelHumid, TempSoil, Precipitation, Uncert_level
#       This data set has been "fixed" a la the 1997 weather station problem.  Entries that
#       were changed are tagged with a 1 in Uncert_level

# ===================================================================
# read in data

weathframe = read.csv("data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv")
weathframe$date = as.Date(paste(weathframe$Year,weathframe$Month,weathframe$Day,sep='-'))

# plot yearly average temp
yearly = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=mean)
plot(yearly[2:25,],xlab='',ylab='mean annual air temp')

# plot monthly average temp
monthly = aggregate(weathframe$TempAir,by=list(weathframe$Month,weathframe$Year),FUN=mean)
monthly$date = as.Date(paste(monthly$Group.2,monthly$Group.1,1,sep='-'))
plot(monthly$date,monthly$x,xlab='',ylab='mean monthly air temp')
lines(monthly$date,monthly$x)

# plot yearly termperature range: yearly max-min
yearmin = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=min)
yearmax = aggregate(weathframe$TempAir,by=list(weathframe$Year),FUN=max)
plot(yearmin$Group.1[2:25],yearmax$x[2:25]-yearmin$x[2:25],xlab='',ylab='temp range')
