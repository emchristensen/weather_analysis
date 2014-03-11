#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

portalfile = "data/Monthly_ppt_1980_present.csv"
portalframe = csv_to_dataframe(portalfile)

portal4swfile = "data/Monthly_ppt_Portal4sw.csv"
portal4swframe = csv_to_dataframe(portal4swfile)

portalppt.ts = ts(portalframe$sumprecip,start=c(1980,1),end=c(2014,3),freq=12)
portal4swppt.ts = ts(portal4swframe$sumprecip,start=c(1966,1),end=c(2013,10),freq=12)

portalppt.w = window(portalppt.ts,start=c(1980,2),end=c(2013,7))
portal4swppt.w = window(portal4swppt.ts,start=c(1980,2),end=c(2013,7))

portalxt.ts = ts(portalframe$maxtemp,start=c(1980,1),end=c(2014,2),freq=12)
portal4swxt.ts = ts(portal4swframe$maxtemp,start=c(1966,1),end=c(2013,10),freq=12)

portalxt.w = window(portalxt.ts,start=c(1980,2),end=c(2013,7))
portal4swxt.w = window(portal4swxt.ts,start=c(1980,2),end=c(2013,7))

portalnt.ts = ts(portalframe$mintemp,start=c(1980,1),end=c(2014,2),freq=12)
portal4swnt.ts = ts(portal4swframe$mintemp,start=c(1966,1),end=c(2013,10),freq=12)

portalnt.w = window(portalnt.ts,start=c(1980,2),end=c(2013,7))
portal4swnt.w = window(portal4swnt.ts,start=c(1980,2),end=c(2013,7))
