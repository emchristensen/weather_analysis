# TODO: Look into better ways of dealing with major errors (negative periods)?

library(reshape2)
library(plyr)

OneYearPlot = function(recent_agg,monthlyppt,year) {
  recent_monthly = monthly_rodents[monthly_rodents$Year==year,]
  recent_agg = aggregate(recent_monthly$x,by=list(Month=recent_monthly$Month),FUN=mean)
  yearppt = monthlyppt[monthlyppt$year==year,]
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(yearppt$month, yearppt$sumprecip, pch=15,  xlab="", ylab="", ylim=c(0,max(yearppt$sumprecip)), 
       axes=FALSE, type="b", col="blue")
  axis(2, ylim=c(0,37),col="blue",las=1,col.axis='blue')  ## las=1 makes horizontal labels
  mtext("Monthly Precip",side=2,line=2.5,col='blue')
  par(new=TRUE)
  plot(recent_agg$Month,recent_agg$x, pch=16, axes=FALSE, ylim=c(0,max(recent_agg$x)), xlab="", ylab="", 
       type="b",col="red", main=year)
 
  ## a little farther out (line=4) to make room for labels
  mtext("Rodent Abundance",side=4,col="red",line=4) 
  axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
  axis(1,pretty(range(yearppt$month),10))
  mtext("Month",side=1,col="black",line=2.5) 
}

RodentPrecipPlot = function(avg_spring_rodents,winter_ppt) {
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(winter_ppt$year[10:33], winter_ppt$ppt[10:33], pch=15,  xlab="", ylab="", ylim=c(0,max(winter_ppt$ppt)), 
       axes=FALSE, type="b", col="blue")
  axis(2, ylim=c(0,max(avg_spring_rodents)),col="blue",las=1,col.axis='blue')  ## las=1 makes horizontal labels
  mtext("Total Winter Precip",side=2,line=2.5,col='blue')
  par(new=TRUE)
  plot(avg_spring_rodents$Year[14:37],avg_spring_rodents$x[14:37], pch=16, axes=FALSE, ylim=c(0,max(avg_spring_rodents$x)), xlab="", ylab="", 
       type="b",col="red")
  
  ## a little farther out (line=4) to make room for labels
  mtext("Rodent Abundance",side=4,col="red",line=4) 
  axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
  axis(1,pretty(range(winter_ppt$year[10:33]),10))
  mtext("",side=1,col="black",line=2.5) 
}

RodentPlantPlot = function(avg_spring_rodents,yearlyavgabund) {
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(yearlyavgabund$Year, yearlyavgabund$x, pch=17,  xlab="", ylab="", ylim=c(0,260), 
       axes=FALSE, type="b", col="darkgreen")
  axis(2, ylim=c(0,260),col="darkgreen",las=1,col.axis='darkgreen')  ## las=1 makes horizontal labels
  mtext("Plant Abundance",side=2,line=2.5,col='darkgreen')
  par(new=TRUE)
  plot(avg_spring_rodents$Year[14:37],avg_spring_rodents$x[14:37], pch=16, axes=FALSE, ylim=c(0,max(avg_spring_rodents$x)), xlab="", ylab="", 
       type="b",col="red")
  
  ## a little farther out (line=4) to make room for labels
  mtext("Rodent Abundance",side=4,col="red",line=4) 
  axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)
  axis(1,pretty(range(yearlyavgabund$Year),10))
  mtext("",side=1,col="black",line=2.5) 
}

PlantPrecipPlot = function(winter_ppt,yearlyavgabund) {
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(winter_ppt$year[10:33], winter_ppt$ppt[10:33], pch=15,  xlab="", ylab="", ylim=c(0,max(winter_ppt$ppt)), 
       axes=FALSE, type="b", col="blue")
  axis(2, ylim=c(0,260),col="blue",las=1,col.axis='blue')  ## las=1 makes horizontal labels
  mtext("Total Winter Precip",side=2,line=2.5,col='blue')
  par(new=TRUE)
  plot(yearlyavgabund$Year, yearlyavgabund$x, pch=17,  xlab="", ylab="", ylim=c(0,260), 
       axes=FALSE, type="b", col="darkgreen")
  
  ## a little farther out (line=4) to make room for labels
  mtext("Plant Abundance",side=4,col="darkgreen",line=4) 
  axis(4, ylim=c(0,7000), col="darkgreen",col.axis="darkgreen",las=1)
  axis(1,pretty(range(yearlyavgabund$Year),10))
  mtext("",side=1,col="black",line=2.5) 
}
  
  
# DATA CITATION: Ecological Archives E090-118-D1
# S. K. Morgan Ernest, Thomas J. Valone, and James H. Brown. 2009. 
#   Long-term monitoring and experimental manipulation of a Chihuahuan Desert 
#   ecosystem near Portal, Arizona, USA. Ecology 90:1708.
#
# Metadata available from http://esapubs.org/archive/ecol/E090/118/metadata.htm
#
# Data from 2002 to 2013 added by Erica Christensen (erica.christensen@weecology.org)

dat = read.csv(
  "commdyn/workflows-and-analysis/portal-rodents/Portal_rodents_19772013.csv", 
  as.is = TRUE,
  colClasses = c(note1='character')
)

# Remove blank rows
dat = dat[!(is.na(dat$mo) | is.na(dat$plot)), ]

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"

# Only select samples in control plots
dat = dat[dat$plot %in% c(1,2,4,8,9,11,12,14,17,22),]

# Only select rodent species
dat = dat[dat$species %in% c('AH','BA','DM','DO','DS','DX','NA','NX','OL','OT','OX','PB',
                             'PE','PF','PH','PI','PL','PM','PP','PX','RF','RM','RO','RX',
                             'SF','SH','SO','SS','ST','SX','UR'),]
#dat = dat[dat$wgt > 35,]
#remove problematic entries as indicated by 'note1' column
#     2 = empty plot
#     4 = plot not trapped
#     7 = animal caught on site but not on plot
#     8 = non-survey
#     9 = outside plot on exterior grid
#     13 = non-target animal
#     14 = plot not completely trapped
dat = dat[!dat$note1 %in% c('2','4','7','8','9','13','14'),]

# Erroneous data has negative sampling period and is just removed for now
dat = dat[dat$period > 0, ]

# Dates -------------------------------------------------------------------

dat$date = paste(dat$yr, dat$mo, dat$dy, sep = "-")

# There is not April 31 or September 21
dat$date = gsub("2000-4-31", "2000-5-1", dat$date)
dat$date = gsub("2000-9-31", "2000-10-1", dat$date)


#aggregate data --------------------------------------------------------------
yearly_rodents = aggregate(dat$yr,by=list(dat$yr),FUN=length)
yearly_rodents_replicates = aggregate(dat$yr,by=list(Year=dat$yr,Plot=dat$plot),FUN=length)
avg_yearly_rodents = aggregate(yearly_rodents_replicates$x,by=list(Year = yearly_rodents_replicates$Year),FUN=mean)


springdat = dat[dat$mo %in% c(1,2,3,4,5,6),]
falldat = dat[dat$mo %in% c(7,8,9,10,11,12),]
summdat = dat[dat$mo %in% c(4,5,6,7,8,9),]
spring_rodents = aggregate(springdat$yr,by=list(springdat$yr),FUN=length)
fall_rodents = aggregate(falldat$yr,by=list(falldat$yr),FUN=length)
summer_rodents = aggregate(summdat$yr,by=list(summdat$yr),FUN=length)



# Plots-------------------------------------------------------------------------
plot(yearly_rodents$Group.1,yearly_rodents$x,xlab='',ylab='Total rodent abundance')
plot(avg_yearly_rodents$Year[14:37],avg_yearly_rodents$x[14:37],xlab='',ylab='rodents/plot',col='red')
points(yearly_rodents_replicates$Year,yearly_rodents_replicates$x)
lines(avg_yearly_rodents$Year[14:37],avg_yearly_rodents$x[14:37],col='red',lwd=2)

# seasonal rodent abundance ----------------------------------------------------------
monthly_rodents = aggregate(dat$yr,by=list(Year = dat$yr,Plot = dat$plot,Month = dat$mo),FUN=length)
plot(monthly_rodents$Month,monthly_rodents$x)
boxplot(monthly_rodents$x~monthly_rodents$Month,ylab='rodents/plot',xlab='Month')
recent_monthly = monthly_rodents[monthly_rodents$Year==2005,]
boxplot(recent_monthly$x~recent_monthly$Month,xlab = 'Month',ylab='Rodent abundance')


monthlyppt = csv_to_dataframe('data/Monthly_ppt_1980_2013.csv')
OneYearPlot(monthly_rodents,monthlyppt,1990)

# plot rodents vs plants and ppt------------------------------------------------------------
#run parts of winter_annuals.R first
plot(winter_ppt$ppt,avg_summer_rodents$x[5:37],xlab='Total winter PPT',ylab = 'rodent abundance')
plot(yearlyavgabund$x,avg_summer_rodents$x[14:36],xlab='annual plant abundance',ylab='rodent abundance')

yearly_rodents_replicates['nextyear'] = yearly_rodents_replicates['Year']-1
rodentsrain = merge(yearly_rodents_replicates,winter_ppt,by.x='nextyear',by.y='year')
plot(rodentsrain$ppt,rodentsrain$x,xlab='Total Winter PPT',ylab='rodent abundance')



#plot summer rodents -----------------------------------------------------------
summer_rodents_replicates = aggregate(summdat$yr,by=list(Year=summdat$yr,Plot=summdat$plot),FUN=length)
avg_summer_rodents = aggregate(summer_rodents_replicates$x,by=list(Year = summer_rodents_replicates$Year),FUN=mean)
plot(summer_rodents$Group.1,summer_rodents$x,xlab='')
plot(avg_summer_rodents$Year[14:37],avg_summer_rodents$x[14:37],xlab='',ylab='rodent abundance')
points(summer_rodents_replicates$Year,summer_rodents_replicates$x)
lines(avg_summer_rodents$Year[14:37],avg_summer_rodents$x[14:37],col='red',lwd=2)


#plot spring rodents -----------------------------------------------------------
spring_rodents_replicates = aggregate(springdat$yr,by=list(Year=springdat$yr,Plot=springdat$plot),FUN=length)
avg_spring_rodents = aggregate(spring_rodents_replicates$x,by=list(Year = spring_rodents_replicates$Year),FUN=mean)
plot(spring_rodents$Group.1,spring_rodents$x,xlab='')
plot(avg_spring_rodents$Year[14:37],avg_spring_rodents$x[14:37],xlab='',ylab='rodent abundance')
points(spring_rodents_replicates$Year,spring_rodents_replicates$x)
lines(avg_spring_rodents$Year[14:37],avg_spring_rodents$x[14:37],col='red',lwd=2)

plot(winter_ppt$ppt[10:33],avg_spring_rodents$x[14:37],xlab='Total Winter Precip',ylab = 'Rodent Abundance')
reg = lm(avg_spring_rodents$x[14:37]~winter_ppt$ppt[10:33])
abline(reg,col='red',lwd=2)
summary(reg)

plot(yearlyavgabund$x,avg_spring_rodents$x[14:36],xlab='Plant Abundance',ylab='Rodent Abundance')
reg = lm(avg_spring_rodents$x[14:36]~yearlyavgabund$x)
abline(reg,col='red',lwd=2)
summary(reg)

# double plots -----------------------------------------------------------------
RodentPrecipPlot(avg_spring_rodents,winter_ppt)
RodentPlantPlot(avg_spring_rodents,yearlyavgabund)
PlantPrecipPlot(winter_ppt,yearlyavgabund)
