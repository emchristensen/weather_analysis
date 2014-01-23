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

weathfile = "..//Dropbox//Portal_EC//Hourly_PPT_mm_1989_present_fixed_date.csv"
weathframe = create_dataframe(weathfile)

pdofile = "..//Dropbox//Portal_EC//weather//ENSO_PDO//pdo.csv"
pdoframe = create_dataframe(pdofile)

pdo_index = matrix_to_vector(pdoframe)