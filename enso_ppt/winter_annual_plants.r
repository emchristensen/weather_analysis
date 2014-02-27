# This script takes plant abundance data and munges it into yearly counts that can be then analyzed with
# rodent and precip data


# Functions ------------------------------------------------------------------------------------

source('portal_weather/csv_to_dataframe.r')

insertRow <- function(existingDF, newrow, r) {
  #function copied from stack overflow (user Ari B. Friedman)
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}

# File names -------------------------------------------------------------------------------------

plantfile = 'data/WinterAnnualQuadrats.csv'
plantframe = csv_to_dataframe(plantfile)

# calculate average abundance per plot ------------------------------------------------------------
plantframe = plantframe[plantframe$Abundance != 'NA',]
#select only control plots
plantframe2 = plantframe[plantframe$Plot %in% c(1,2,4,8,9,11,12,14,17,22),]
totalquads = aggregate(plantframe2$Abundance,list(Year=plantframe2$Year,
                                                 Plot=plantframe2$Plot,
                                                 Stake=plantframe2$Stake),FUN=sum)

abundperplot = aggregate(totalquads$x,list(Year=totalquads$Year,
                                          Plot=totalquads$Plot),FUN=mean)

quadsperplot = aggregate(totalquads$x,list(Year=totalquads$Year,
                                           Plot=totalquads$Plot),FUN=length)

#yearly average plants/quadrat/year
yearlyavgabund = aggregate(abundperplot2$x,list(Year=abundperplot2$Year),FUN=mean)
#introduce NAs for years with no census
yearlyavgabund = insertRow(yearlyavgabund,c(2011,NA),30)
yearlyavgabund = insertRow(yearlyavgabund,c(2010,NA),30)
yearlyavgabund = insertRow(yearlyavgabund,c(2000,NA),21)
yearlyavgabund = insertRow(yearlyavgabund,c(1996,NA),7)
yearlyavgabund = insertRow(yearlyavgabund,c(1980,NA),3)
