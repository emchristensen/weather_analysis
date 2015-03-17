#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

yearly_summer_precip = function(dataframe) {
  #sums precipitation for summer months (June-Sept), returns yearly total
  year= vector()
  ppt = vector()
  for (yr in 1980:2012) {
    yr_mo = list(c(yr,6),c(yr,7),c(yr,8),c(yr,9))
    summ_ppt = vector()
    for (i in 1:length(dataframe$year)) {
      if (is.element(list(c(dataframe$year[i],dataframe$month[i])),yr_mo)) {
        summ_ppt = append(summ_ppt,dataframe$sumprecip[i])
      }
    }
    year = append(year,yr)
    ppt = append(ppt,sum(summ_ppt))
  }
  return(data.frame(year,ppt))
}

weathfile = "data/Monthly_ppt_1980_2013_nona.csv"
weathframe = csv_to_dataframe(weathfile)

summer_ppt = yearly_summer_precip(weathframe)
