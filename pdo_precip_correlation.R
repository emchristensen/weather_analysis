create_dataframe = function(datafile) {
  #converts data in csv file to data frame form
  rawdata = read.csv(paste(datafile,sep=''),head=T,sep=',')
  dataframe = data.frame(rawdata)
  return(dataframe)
}

matrix_to_vector = function(dataframe) {
  #takes pdo data from matrix form to vector
  year = dataframe['YEAR']
  values = dataframe[,-1]
  yr = vector()
  mo = vector()
  index = vector()
  for (row in 1:length(year[,1])) {
    for (month in 1:length(values)) {
      yr = append(yr,year[row,])
      mo = append(mo,month)
      index = append(index,values[row,month])
    }
  }
  index_values = data.frame(yr,mo,index)
  return(index_values)
}

precip_exploratory_plots = function(dataframe) {
  #produces some exploratory plots of precip data
  precip.ts = ts(dataframe$sumprecip,start=c(1980,1),end=c(2013,11),freq=12)
  plot(precip.ts)
  abline(h=mean(precip.ts[!is.na(precip.ts)]),col=2)
  acf(precip.ts,na.action=na.pass)
}  
 
temp_exploratory_plots = function(dataframe) {
  #produces some exploratory plots of temperature data
  maxt.ts = ts(dataframe$maxtemp,start=c(1980,1),end=c(2013,11),freq=12)
  plot(maxt.ts)
  abline(h=mean(maxt.ts[!is.na(maxt.ts)]),col=2)
  mint.ts = ts(dataframe$mintemp,start=c(1980,1),end=c(2013,11),freq=12)
  plot(mint.ts)
  abline(h=mean(mint.ts[!is.na(mint.ts)]),col=2)
  acf(mint.ts,na.action=na.pass)
}

weathfile = "data/Monthly_ppt_1980_2013.csv"
weathframe = create_dataframe(weathfile)

pdofile = "..//Dropbox//Portal_EC//weather//ENSO_PDO//pdo.csv"
pdoframe = create_dataframe(pdofile)

ensofile = "..//Dropbox//Portal_EC//weather//ENSO_PDO//enso.csv"
ensoframe = create_dataframe(ensofile)

precip_exploratory_plots(weathframe)

pdo_index = matrix_to_vector(pdoframe)
enso_index = matrix_to_vector(ensoframe)

pdo.ts = ts(pdo_index$index,start=c(1900,1),end=c(2013,12),freq=12)
enso.ts = ts(enso_index$index,start=c(1950,1),end=c(2013,12),freq=12)
precip.ts = ts(weathframe$sumprecip,start=c(1980,1),end=c(2013,11),freq=12)
pdo.w = window(pdo.ts,start=c(1980,2),end=c(2013,6))
enso.w = window(enso.ts,start=c(1980,2),end=c(2013,6))
precip.w = window(precip.ts,start=c(1980,2),end=c(2013,6))

model1 = lm(precip.w~pdo.w+enso.w+pdo.w*enso.w)
summary(model1)

