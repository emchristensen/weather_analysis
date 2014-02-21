#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')


#function copied from stack overflow (user Ari B. Friedman)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}

plantfile = 'data/SummerAnnualQuadrats.csv'
splantframe = csv_to_dataframe(plantfile)

#calculate total abundance for each quadrat in each plot in each year
stotalquads = aggregate(splantframe$Abundance,list(Year=splantframe$Year,Plot=splantframe$Plot,Stake=splantframe$Stake),FUN=sum)
#calculate average abundance per quadrat for each plot in each year
sabundperquad = aggregate(stotalquads$x,list(Year=stotalquads$Year,Plot=stotalquads$Plot),FUN=mean, na.action= na.omit)

#select only control plots
sabundperquad2 = sabundperquad[sabundperquad$Plot %in% c(2,4,11,12,14,17,22),]
#starting in 1983 because of severe errors/change in data collection
sabundperquad3 = sabundperquad2[sabundperquad2$Year >1989,]
sabundperquad3 = sabundperquad3[sabundperquad3$x != 'NA',]

#yearly average
syearlyavgabund = aggregate(sabundperquad3$x,list(Year=sabundperquad3$Year),FUN=mean)
syearlyavgabund = insertRow(syearlyavgabund,c(2012,NA),19)
syearlyavgabund = insertRow(syearlyavgabund,c(2010,NA),19)
syearlyavgabund = insertRow(syearlyavgabund,c(2009,NA),19)
syearlyavgabund = insertRow(syearlyavgabund,c(2003,NA),14)

#plot abundance vs year ----------------------------------------------------------------------

points(sabundperquad3$Year,sabundperquad3$x,xlab='year',ylab='abundance (stems/quadrat)',col='red',pch=16)
plot(syearlyavgabund$Year,syearlyavgabund$x,xlab='',ylab='Avg abundance (stems/quadrat)')


lines(syearlyavgabund$Year,syearlyavgabund$x,col='red',lwd=2)
points(syearlyavgabund$Year,syearlyavgabund$x,col='red',pch=16)

#plotting abundance vs precip -------------------------------------------------------------

source('portal_weather/yearly_summer_precip.r')
sabundperquad3['nextyear'] = sabundperquad3['Year']-1
sabundperquad4 = merge(sabundperquad3,summer_ppt,by.x='nextyear',by.y='year')
plot(sabundperquad4$ppt,sabundperquad4$x,xlab='Total Summer PPT',ylab='Avg Annual Abundance (stems/quadrat)')


syearlyvariance = aggregate(sabundperquad3$x,list(Year=sabundperquad3$Year),FUN=var)
syearlyvariance['nextyear'] = syearlyvariance['Year']-1
svariancemerge = merge(syearlyvariance,summer_ppt,by.x='nextyear',by.y='year')
plot(svariancemerge$ppt,svariancemerge$x,xlab='Total Summer PPT',ylab='Variance')


#ppt vs plant abundance 1990-2012
plot(summer_ppt$ppt[10:32],syearlyavgabund$x,xlab='Total Summer PPT',ylab='Avg Annual Abundance (stems/quadrat)')
#ppt vs plant abundance 1995-2012
plot(summer_ppt$ppt[15:32],syearlyavgabund$x[6:23],xlab='Total Summer PPT',ylab='Avg Annual Abund. (stems/quadrat')

reg1 = lm(syearlyavgabund$x~summer_ppt$ppt[10:32])
abline(reg1,col='red')
summary(reg1)
