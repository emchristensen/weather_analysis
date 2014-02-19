#load function for reading csv data
source('csv_to_dataframe.r')

yearly_summer_precip = function(dataframe) {
  #sums precipitation for summer months (June-Sept), returns yearly total
  year= vector()
  ppt = vector()
  for (yr in 1990:2012) {
    yr_mo = list(c(yr,6),c(yr,7),c(yr,8),c(yr,9))
    summ_ppt = vector()
    for (i in 1:length(dataframe$Year)) {
      if (is.element(list(c(dataframe$Year[i],dataframe$Month[i])),yr_mo)) {
        summ_ppt = append(summ_ppt,dataframe$Precipitation[i])
      }
    }
    year = append(year,yr)
    ppt = append(ppt,sum(summ_ppt))
  }
  return(data.frame(year,ppt))
}

yearly_winter_precip = function(dataframe) {
  #sums precipitation for winter months (Dec-March), returns yearly total
  year= vector()
  ppt = vector()
  for (yr in 1990:2012) {
    yr_mo = list(c(yr,12),c(yr+1,1),c(yr+1,2),c(yr+1,3))
    win_ppt = vector()
    for (i in 1:length(dataframe$Year)) {
      if (is.element(list(c(dataframe$Year[i],dataframe$Month[i])),yr_mo)) {
        win_ppt = append(win_ppt,dataframe$Precipitation[i])
      }
    }
    year = append(year,yr)
    ppt = append(ppt,sum(win_ppt))
  }
  return(data.frame(year,ppt))
}

weathfile = "data/Hourly_PPT_mm_1989_present_fixed.csv"
weathframe = csv_to_dataframe(weathfile)

weathframe$intensity[weathframe$Precipitation>7.6] = 3
weathframe$intensity[weathframe$Precipitation<3.8] = 1

#highint_s = yearly_summer_precip(weathframe[weathframe$intensity==3,])
#lowint_s = yearly_summer_precip(weathframe[weathframe$intensity==1,])
highint_w = yearly_winter_precip(weathframe[weathframe$intensity==3,])
lowint_w = yearly_winter_precip(weathframe[weathframe$intensity==1,])
