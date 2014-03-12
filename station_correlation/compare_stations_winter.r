#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')

#load data --------------------------------------------------------------
portalfile = "data/Monthly_ppt_1980_present.csv"
portalframe = csv_to_dataframe(portalfile)

portal4swfile = "data/Monthly_ppt_Portal4sw.csv"
portal4swframe = csv_to_dataframe(portal4swfile)

sansimonfile = "data/Monthly_ppt_SanSimon.csv"
sansimonframe = csv_to_dataframe(sansimonfile)

#precipitation comparison --------------------------------------------------------
portalppt.ts = ts(portalframe$sumprecip,start=c(1980,1),end=c(2014,3),freq=12)
portal4swppt.ts = ts(portal4swframe$sumprecip,start=c(1966,1),end=c(2013,10),freq=12)
sansimonppt.ts = ts(sansimonframe$sumprecip,start=c(1987,1),end=c(2014,3),freq=12)

totalframe1 = merge(portalframe,sansimonframe,by=c('month','year'))
winterframe1 = totalframe1[totalframe1$month %in% c(1,2,3,12,11,10),]
plot(winterframe1$sumprecip.x,winterframe1$sumprecip.y,xlab='portal',ylab='san simon')
model1 = lm(winterframe1$sumprecip.y~winterframe1$sumprecip.x)
lines(seq(150),seq(150))
lines(seq(150),1.8+seq(150),col='red')

summerframe1 = totalframe1[totalframe1$month %in% c(4,5,6,7,8,9),]
plot(summerframe1$sumprecip.x,summerframe1$sumprecip.y,xlab='portal',ylab='san simon')
model2 = lm(summerframe1$sumprecip.y~summerframe1$sumprecip.x)
lines(seq(150),6+seq(150)*.73,col='red')

totalframe2 = merge(portalframe,portal4swframe,by=c('month','year'))
winterframe2 = totalframe2[totalframe2$month %in% c(1,2,3,12,11,10),]
plot(winterframe2$sumprecip.x,winterframe2$sumprecip.y,xlab='portal',ylab='portal 4sw')
model1a = lm(winterframe2$sumprecip.y~winterframe2$sumprecip.x)
lines(seq(150),9.9+seq(150)*1.3,col='red')

summerframe2 = totalframe2[totalframe2$month %in% c(4,5,6,7,8,9),]
plot(summerframe2$sumprecip.x,summerframe2$sumprecip.y,xlab='portal',ylab='portal 4sw')
model2a = lm(summerframe2$sumprecip.y~summerframe2$sumprecip.x)
lines(seq(150),12.2+seq(150)*1.5,col='red')
