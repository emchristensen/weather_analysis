create_dataframe = function(datafile) {
  #converts data in csv file to data frame form
  rawdata = read.csv(paste(datafile,sep=''),head=T,sep=',')
  dataframe = data.frame(rawdata)
  return(dataframe)
}

weathfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
weathframe = create_dataframe(weathfile)

#get total yearly (winter) precip
years = unique(weathframe$Year)
for (y in years) {
  
}