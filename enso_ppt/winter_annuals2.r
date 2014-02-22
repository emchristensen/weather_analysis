#load function for reading csv data
source('portal_weather/csv_to_dataframe.r')


#function copied from stack overflow (user Ari B. Friedman)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}

plantfile = 'data/WinterAnnualQuadrats.csv'
plantframe = csv_to_dataframe(plantfile)

#calculate total abundance for each quadrat in each plot in each year
totalquads = aggregate(plantframe$Abundance,list(Year=plantframe$Year,Plot=plantframe$Plot,Stake=plantframe$Stake),FUN=sum)
#calculate average abundance per quadrat for each plot in each year
abundperquad = aggregate(totalquads$x,list(Year=totalquads$Year,Plot=totalquads$Plot),FUN=mean, na.action= na.omit)

#select only control plots
abundperquad2 = abundperquad[abundperquad$Plot %in% c(1,2,4,8,9,11,12,14,17,22),]
#starting in 1989 because of severe errors/change in data collection
abundperquad3 = abundperquad2[abundperquad2$Year >1989,]
abundperquad3 = abundperquad3[abundperquad3$x != 'NA',]

#yearly average
yearlyavgabund = aggregate(abundperquad3$x,list(Year=abundperquad3$Year),FUN=mean)
yearlyavgabund = insertRow(yearlyavgabund,c(2011,NA),19)
yearlyavgabund = insertRow(yearlyavgabund,c(2010,NA),19)
yearlyavgabund = insertRow(yearlyavgabund,c(2000,NA),10)
yearlyavgabund = insertRow(yearlyavgabund,c(1996,NA),7)

#plot abundance vs year ----------------------------------------------------------------------

points(abundperquad3$Year,abundperquad3$x,xlab='year',ylab='abundance (stems/quadrat)',col='red',pch=16)
plot(yearlyavgabund$Year,yearlyavgabund$x,xlab='',ylab='Avg abundance (stems/quadrat)')


lines(yearlyavgabund$Year,yearlyavgabund$x,col='red',lwd=2)
points(yearlyavgabund$Year,yearlyavgabund$x,col='red',pch=16)

#plotting abundance vs precip -------------------------------------------------------------

source('portal_weather/yearly_winter_precip.r')
abundperquad3['nextyear'] = abundperquad3['Year']-1
abundperquad4 = merge(abundperquad3,winter_ppt,by.x='nextyear',by.y='year')
plot(abundperquad4$ppt,abundperquad4$x,xlab='Total Winter PPT',ylab='Avg Annual Abundance (stems/quadrat)')


yearlyvariance = aggregate(abundperquad3$x,list(Year=abundperquad3$Year),FUN=var)
yearlyvariance['nextyear'] = yearlyvariance['Year']-1
variancemerge = merge(yearlyvariance,winter_ppt,by.x='nextyear',by.y='year')
plot(variancemerge$ppt,variancemerge$x,xlab='Total Winter PPT',ylab='Variance')


#ppt vs plant abundance 1990-2012
plot(winter_ppt$ppt[10:32],yearlyavgabund$x,xlab='Total Winter PPT',ylab='Avg Annual Abundance (stems/quadrat)')
#ppt vs plant abundance 1995-2012
plot(winter_ppt$ppt[15:32],yearlyavgabund$x[6:23],xlab='Total Winter PPT',ylab='Avg Annual Abund. (stems/quadrat')

reg1 = lm(yearlyavgabund$x~winter_ppt$ppt[10:32])
abline(reg1,col='red')
summary(reg1)
