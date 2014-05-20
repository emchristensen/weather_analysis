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

portalppt.w = window(portalppt.ts,start=c(1980,1),end=c(2013,7))
portal4swppt.w = window(portal4swppt.ts,start=c(1980,1),end=c(2013,7))
plot(portalppt.w,portal4swppt.w)
lines(seq(150),seq(150))
model3 = lm(portal4swppt.w~portalppt.w)
lines(seq(150),9.5+seq(150)*1.57,col='red')

portalppt.w2 = window(portalppt.ts,start=c(1987,1),end=c(2013,7))
sansimonppt.w2 = window(sansimonppt.ts,start=c(1987,1),end=c(2013,7))
plot(portalppt.w2,sansimonppt.w2)
lines(seq(150),seq(150))
model3a = lm(sansimonppt.w2~portalppt.w2)
lines(seq(150),4.5+seq(150)*.8,col='red')

portalppt.w1 = window(portalppt.ts,start=c(1989,7),end=c(2001,12))
portal4swppt.w1 = window(portal4swppt.ts,start=c(1989,7),end=c(2001,12))
sansimonppt.w1 = window(sansimonppt.ts,start=c(1989,7),end=c(2001,12))
plot(as.vector(portalppt.w1),as.vector(portal4swppt.w1))
lines(seq(0,150),seq(0,150))
model4 = lm(portal4swppt.w1~portalppt.w1)
lines(seq(150),6.5+seq(150)*1.7,col='red')

plot(as.vector(portalppt.w1),as.vector(sansimonppt.w1))
lines(seq(150),seq(150))
model4a = lm(sansimonppt.w1~portalppt.w1)
lines(seq(150),6+seq(150)*.89,col='red')


portalppt.w3 = window(portalppt.ts,start=c(2003,1),end=c(2007,12))
portal4swppt.w3 = window(portal4swppt.ts,start=c(2003,1),end=c(2007,12))
sansimonppt.w3 = window(sansimonppt.ts,start=c(2003,1),end=c(2007,12))
plot(as.vector(portalppt.w3),as.vector(portal4swppt.w3))
lines(seq(150),seq(150))
model5 = lm(portal4swppt.w3~portalppt.w3)
lines(seq(150),12+seq(150)*1.4,col='red')

plot(as.vector(portalppt.w3),as.vector(sansimonppt.w3))
model5a = lm(sansimonppt.w3~portalppt.w3)
lines(seq(150),3.4+seq(150)*.65,col='red')

portalppt.w4 = window(portalppt.ts,start=c(1980,1),end=c(2009,12))
portal4swppt.w4 = window(portal4swppt.ts,start=c(1980,1),end=c(2009,12))
plot(as.vector(portal4swppt.w4),as.vector(portalppt.w4))
model6 = lm(portalppt.w4~portal4swppt.w4)
lines(seq(150),3.7+seq(150)*.46,col='red')


# max temp comparison ----------------------------------------------------------
portalxt.ts = ts(portalframe$maxtemp,start=c(1980,1),end=c(2014,2),freq=12)
portal4swxt.ts = ts(portal4swframe$maxtemp,start=c(1966,1),end=c(2013,10),freq=12)
sansimonxt.ts = ts(sansimonframe$maxtemp,start=c(1987,1),end=c(2014,3),freq=12)

portalxt.w = window(portalxt.ts,start=c(1980,1),end=c(2009,12))
portal4swxt.w = window(portal4swxt.ts,start=c(1980,1),end=c(2009,12))
plot(portal4swxt.w,portalxt.w)
lines(seq(50),seq(50))
model1 = lm(portalxt.w~portal4swxt.w)
lines(seq(50),1.3+seq(50)*1.1,col='red')

portalxt.w2 = window(portalxt.ts,start=c(1987,1),end=c(2013,7))
sansimonxt.w2 = window(sansimonxt.ts,start=c(1987,1),end=c(2013,7))
plot(portalxt.w2,sansimonxt.w2)
lines(seq(50),seq(50))
model1a = lm(sansimonxt.w2~portalxt.w2)
lines(seq(50),.29+seq(50)*1.04,col='red')

# min temp comparison ------------------------------------------------------
portalnt.ts = ts(portalframe$mintemp,start=c(1980,1),end=c(2014,2),freq=12)
portal4swnt.ts = ts(portal4swframe$mintemp,start=c(1966,1),end=c(2013,10),freq=12)
sansimonnt.ts = ts(sansimonframe$mintemp,start=c(1987,1),end=c(2014,3),freq=12)

portalnt.w = window(portalnt.ts,start=c(1980,1),end=c(2009,12))
portal4swnt.w = window(portal4swnt.ts,start=c(1980,1),end=c(2009,12))
plot(portal4swnt.w,portalnt.w)
lines(seq(-15,20),seq(-15,20))
model2 = lm(portalnt.w~portal4swnt.w)
lines(seq(-15,20),6.4+seq(-15,20)*1.0,col='red')

portalnt.w2 = window(portalnt.ts,start=c(1987,1),end=c(2013,7))
sansimonnt.w2 = window(sansimonnt.ts,start=c(1987,1),end=c(2013,7))
plot(portalnt.w2,sansimonnt.w2)
lines(seq(-15,20),seq(-15,20))
model2a = lm(sansimonnt.w2~portalnt.w2)
lines(seq(-15,20),-3.07+seq(-15,20)*.97,col='red')
