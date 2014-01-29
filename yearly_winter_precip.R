create_dataframe = function(datafile) {
  #converts data in csv file to data frame form
  rawdata = read.csv(paste(datafile,sep=''),head=T,sep=',')
  dataframe = data.frame(rawdata)
  return(dataframe)
}

winter_total = function(dataframe) {
  #sums precipitation for winter months (Dec-March), returns yearly total
  year= vector()
  ppt = vector()
  for (yr in 1980:2012) {
    yr_mo = list(c(yr,12),c(yr+1,1),c(yr+1,2),c(yr+1,3))
    win_ppt = vector()
    for (i in 1:length(dataframe$year)) {
      if (is.element(list(c(dataframe$year[i],dataframe$month[i])),yr_mo)) {
        win_ppt = append(win_ppt,dataframe$sumprecip[i])
      }
    }
    year = append(year,yr)
    ppt = append(ppt,sum(win_ppt))
  }
}

weathfile = "data/Monthly_ppt_1980_2013.csv"
weathframe = create_dataframe(weathfile)

