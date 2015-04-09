# This script takes monthly weather summary and writes a new file of 
# total winter precip.
# Written by EMC 3/11/15
#
# Input:
#     Monthly_ppt_1980_present.csv
#         year, month, sumprecip, maxtemp, mintemp
#
# Output:
#     
#
# Scripts called:
#     weather_daily_summary.r     combines hourly dataset (1989-present) and daily dataset (1980-89)


# set working directory and run prelim scripts
setwd('C:/Users/EC/git_dir/')
source('portal_weather/weather_monthly_summary.r')

# =====================================================================================================
# functions

yearly_winter_precip = function(dataframe) {
  #sums precipitation for winter months (Dec-March), returns yearly total
  year= vector()
  ppt = vector()
  for (yr in 1980:2014) {
    yr_mo = list(c(yr-1,10),c(yr-1,11),c(yr-1,12),c(yr,1),c(yr,2),c(yr,3),c(yr,4))
    win_ppt = vector()
    for (i in 1:length(dataframe$year)) {
      if (is.element(list(c(dataframe$year[i],dataframe$month[i])),yr_mo)) {
        win_ppt = append(win_ppt,dataframe$sumprecip[i])
      }
    }
    year = append(year,yr)
    ppt = append(ppt,sum(win_ppt))
  }
  return(data.frame(year,ppt))
}

# read in daily data, aggregate  by month excluding NAs, only take data from months with <10 days NA
weathframe = read.csv("data/Daily_weather_1980_present_fixed_withgaps.csv")
monthly = aggregate(weathframe$Precipitation,by=list(weathframe$Year,weathframe$Month),FUN=sum,na.rm=T)
names(monthly) = c('year','month','sumprecip')

nas = weathframe[is.na(weathframe$Precipitation),]
nacount = aggregate(nas$Precipitation,by=list(nas$Year,nas$Month),FUN=length)
names(nacount) = c('year','month','na')

monthly = merge(monthly,nacount,by=c('year','month'),all=T)
monthly[is.na(monthly$na),4] = 0
# make precip NA if >10 days missing
monthly[monthly$na>10,3] = NA

winter_ppt = yearly_winter_precip(monthly)
