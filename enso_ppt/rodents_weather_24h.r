#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')
source('data/period_dates.r')

# read and clean raw rodent data ------------------------------------------------------------------------
dat = read.csv("data/Rodents.csv", as.is = TRUE,  colClasses = c(note1='character'))

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"

# Only select samples in control plots
dat = dat[dat$plot %in% c(1,2,4,8,9,11,12,14,17,22),]

# Only select rodent species
dat = dat[dat$species %in% c('PP'),]
#dat = dat[dat$species %in% c('AH','BA','DM','DO','DS','DX','NA','NX','OL','OT','OX','PB',
#                             'PE','PF','PH','PI','PL','PM','PP','PX','RF','RM','RO','RX',
#                             'SF','SH','SO','SS','ST','SX','UR',''),]

#remove problematic entries as indicated by 'note1' column
#     4 = plot not trapped
#     7 = animal caught on site but not on plot
#     8 = non-survey
#     9 = outside plot on exterior grid
#     13 = non-target animal
#     14 = plot not completely trapped
dat = dat[!dat$note1 %in% c('4','7','8','9','13','14'),]

# Erroneous data has negative sampling period and is just removed for now
dat = dat[dat$period > 0, ]

# remove entries with no mass measurement
dat = dat[!is.na(dat$wgt),]

# ============================================================================================
# Compute rodent abundance and mean mass for each day of trapping
dat$date = as.Date(paste(dat$yr,dat$mo,dat$dy,sep='-'))
daily_rodents = aggregate(dat$Record_ID,by=list(date=dat$date),FUN=length)
meanmass = aggregate(dat$wgt,by=list(date=dat$date),FUN=mean)
daily_rodents$meanmass = meanmass$x

trapdates = unique(daily_rodents$date)


# compute avg air temp and ppt for 6pm-6am on trapping days------------------------------------
weathfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
weathframe = csv_to_dataframe(weathfile)
weathframe$date = as.Date(paste(weathframe$Year,weathframe$Month,weathframe$Day,sep='-'))

nighttime = weathframe[weathframe$Hour %in% c(1800,1900,2000,2100,2200,2300,2400,100,200,300,400,500),]
premidnight = nighttime$Hour %in% c(1800,1900,2000,2100,2200,2300,2400)
nighttime$newdate = nighttime$date
nighttime$newdate[premidnight] = nighttime$date[premidnight]+1

allnights_weather = aggregate(nighttime$TempAir,by=list(date = nighttime$newdate),FUN=mean)
totalppt = aggregate(nighttime$Precipitation,by=list(date=nighttime$newdate),FUN=sum)
relhumid = aggregate(nighttime$RelHumid,by=list(date=nighttime$newdate),FUN=mean)
tempsoil = aggregate(nighttime$TempSoil,by=list(date=nighttime$newdate),FUN=mean)
allnights_weather$ppt= totalppt$x
allnights_weather$RelHumid = relhumid$x
allnights_weather$TempSoil = tempsoil$x
nightly_weather = allnights_weather[allnights_weather$date %in% trapdates,]
nightly_weather$ppt_bin = rep(0,length(nightly_weather$date))
nightly_weather$ppt_bin[nightly_weather$ppt != 0] = 1

# compare rodent abundance/mass to weather --------------------------------------------------------
daily_rodents = daily_rodents[daily_rodents$date %in% nightly_weather$date,]

rodents_weather = merge(daily_rodents,nightly_weather,by='date')

plot(rodents_weather$x.y,rodents_weather$x.x,xlab='mean temp',ylab='PP abundance')
plot(rodents_weather$x.y,rodents_weather$meanmass,xlab='mean temp',ylab='mean rodent mass')
plot(rodents_weather$ppt,rodents_weather$x.x,xlab='total precip',ylab='rodent abundance')
plot(rodents_weather$ppt,rodents_weather$meanmass,xlab='total precip',ylab='mean rodent mass')
plot(rodents_weather$RelHumid,rodents_weather$x.x,xlab='rel humid',ylab='rodent abundance')
plot(rodents_weather$RelHumid,rodents_weather$meanmass,xlab='rel humid',ylab='mean rodent mass')
plot(rodents_weather$TempSoil,rodents_weather$x.x,xlab='soil temp',ylab='rodent abundance')
plot(rodents_weather$TempSoil,rodents_weather$meanmass,xlab='soil temp',ylab='mean rodent mass')

boxplot(rodents_weather$meanmass~rodents_weather$ppt_bin)
boxplot(rodents_weather$x.x~rodents_weather$ppt_bin,xlab='rain/no rain',ylab='abundance')

# models ------------------------------------------------------------------------------------------
model1 = lm(rodents_weather$x.x~rodents_weather$x.y+rodents_weather$ppt+rodents_weather$RelHumid)
summary(model1)
model1.5 = lm(rodents_weather$x.x~rodents_weather$x.y)
summary(model1.5)
model2 = lm(rodents_weather$meanmass~rodents_weather$x.y+rodents_weather$ppt+rodents_weather$RelHumid)
summary(model2)
model2.5 = lm(rodents_weather$meanmass~rodents_weather$x.y)
summary(model2.5)
model3 = lm(rodents_weather$meanmass~rodents_weather$x.y+rodents_weather$ppt_bin)
summary(model3)
model4 = lm(rodents_weather$x.x~rodents_weather$x.y*rodents_weather$ppt)
summary(model4)


# other crap ---------------------------------------------------------------------------------------
raining = rodents_weather[rodents_weather$ppt_bin==1,]
notraining = rodents_weather[rodents_weather$ppt_bin==0,]
plot(raining$x.y,raining$x.x,xlab='mean temp',ylab='rodent abundance',main='ppt!=0')
plot(notraining$x.y,notraining$x.x,xlab='mean temp',ylab='rodent abundance',main='ppt=0')
plot(raining$x.y,raining$meanmass,xlab='mean temp',ylab='mean rodent mass',main='ppt!=0')
plot(notraining$x.y,notraining$meanmass,xlab='mean temp',ylab='mean rodent mass',main='ppt=0')


model = lm(notraining$meanmass~notraining$x.y)
summary(model)
