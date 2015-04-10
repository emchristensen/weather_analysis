#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

ensofile = "data/enso.csv"
ensoframe = csv_to_dataframe(ensofile)

enso.vec = as.vector(t(ensoframe[,c(2,3,4,5,6,7,8,9,10,11,12,13)]))
enso.ts = ts(enso.vec,start=c(1950,1),end=c(2013,6),freq=12)

enso.w = window(enso.ts,start=c(1980,1),end=c(2013,6))
plot(enso.w,xlab='',ylab='ENSO')

precip.w = window(sumprecip.smooth,start=c(1980,1),end=c(2013,6))


par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(enso.w) # first plot
par(new = TRUE)
plot(precip.w,col='red',xlab='',ylab='')
