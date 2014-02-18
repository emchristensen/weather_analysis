#correlating precipitation in Portal to ENSO and PDO indecies using methods similar
# to Gutzler 2002

#load function for reading csv data
source('csv_to_dataframe.r')

#run code to calculate yearly winter precipitation (Dec-March) from Portal
source('yearly_winter_precip.r')

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


weathfile = "data/Monthly_ppt_1980_2013.csv"
weathframe = csv_to_dataframe(weathfile)

pdofile = "data/pdo.csv"
pdoframe = csv_to_dataframe(pdofile)

ensofile = "data/enso.csv"
ensoframe = csv_to_dataframe(ensofile)

# working with ENSO-precip correlation ------------------------

#extract enso avg of July-Sept (years are 1950-2012)
enso = ensoframe['JAS']

enso.ts = ts(enso,start=c(1950),end=c(2012))
ppt.ts = ts(winter_ppt$ppt,start=c(1980),end=c(2012))
enso.w = window(enso.ts,start=c(1980),end=c(2012))

pptframe = data.frame(enso=as.vector(enso.w),ppt=as.vector(ppt.ts),year=1980:2012)
plot(pptframe$enso,pptframe$ppt)

lowenso = pptframe[pptframe$enso<(-.5),]
hienso = pptframe[pptframe$enso>.5,]
points(lowenso$enso,lowenso$ppt,col=2)
points(hienso$enso,hienso$ppt,col=3)

model1 = lm(pptframe$ppt~pptframe$enso)
summary(model1)


# Using Pdo to predict precipitation ------------------------------------
pdoindex = matrix_to_vector(pdoframe)
#average pdo index (Sept-Aug)
for (i in length(pdoindex$yr)) {
  if (pdoindex$mo[i] %in% c(1,2,3,4,5,6,7,8)) {
    pdoindex$yr[i] = pdoindex$yr[i] + 1
  }
}
pdoavg = aggregate(pdoindex$index,by=list(pdoindex$yr),FUN=mean)
pdo.ts = ts(pdoavg$x,start=c(1900),end=c(2013))
pdo.w = window(pdo.ts,start=c(1980),end=c(2012))
pptframe['pdo'] = as.vector(pdo.w)

plot(pptframe$pdo,pptframe$ppt)
model2 = lm(pptframe$ppt~pptframe$pdo)
summary(model2)

# Considering PDO and ENSO together ---------------------------
model3 = lm(pptframe$ppt~pptframe$pdo*pptframe$enso)
summary(model3)
