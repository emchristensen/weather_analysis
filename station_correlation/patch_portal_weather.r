#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')


portal4swfile = "data/Monthly_ppt_Portal4sw.csv"
portal4swframe = csv_to_dataframe(portal4swfile)

portalfile = "data/Monthly_ppt_1980_present.csv"
portalframe = csv_to_dataframe(portalfile)

# -----------------------------------------------------------------------------------
adjmaxtemp = -0.23+portal4swframe$maxtemp*1.14
adjmintemp = 6.67+portal4swframe$mintemp*1.1
adjsumprecip = -6.1+portal4swframe$sumprecip*.64
portal4swapprox = data.frame(maxtemp = adjmaxtemp,
                             mintemp = adjmintemp,
                             sumprecip = adjsumprecip,
                             year = portal4swframe$year,
                             month = portal4swframe$month)

merged = merge(portalframe,portal4swapprox,by.x=c('year','month'),by.y=c('year','month'))

# replace NAs with estimates from portal 4sw --------------------------------------------------------
merged$maxtemp.x[is.na(merged$maxtemp.x)]= merged$maxtemp.y[is.na(merged$maxtemp.x)]
merged$mintemp.x[is.na(merged$mintemp.x)]= merged$mintemp.y[is.na(merged$mintemp.x)]
merged$sumprecip.x[is.na(merged$sumprecip.x)]= merged$sumprecip.y[is.na(merged$sumprecip.x)]

portalpatched = data.frame(year = merged$year,
                           month = merged$month,
                           sumprecip = merged$sumprecip.x,
                           maxtemp = merged$maxtemp.x,
                           mintemp = merged$mintemp.x)

portalpatched =  portalpatched[with(portalpatched, order(year,month)), ]


outfile = "data/Monthly_ppt_1980_present_patched.csv"
write.csv(portalpatched,file=outfile,row.names=F)
