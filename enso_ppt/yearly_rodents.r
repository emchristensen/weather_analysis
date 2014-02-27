# This script takes rodent data and arranges it as yearly total abundance for comparison with
# plant and precip data

# functions ----------------------------------
SeasonalAvg = function(dat,months) {
  #this function calculates total rodent abundance for a subset of the year
  seasonal_dat = dat[dat$mo %in% months,]
  year_plot = aggregate(seasonal_dat$yr,
                        by=list(Year=seasonal_dat$yr, Plot=seasonal_dat$plot),
                        FUN=length)
  seasonal_rodents = aggregate(year_plot$x,
                               by=list(Year = year_plot$Year),
                               FUN = mean)
  return(seasonal_rodents)
}

AggregateByYear = function(dat) {
  #this function calculates total rodent abundance (avg per plot) per year
  empties = dat[dat$note1 == '2',c(4,5,6)]
  empties$x = rep(0,length(empties$yr))
  nonempties = dat[dat$note1 != '2',]
  rodents_plots = aggregate(nonempties$yr,
                                   by=list(yr=nonempties$yr,plot=nonempties$plot,period=nonempties$period),
                                   FUN=length)
  yearly_rodents_plots = rbind(empties,rodents_plots)
  yearly_rodents = aggregate(yearly_rodents_plots$x,
                             by=list(Year = yearly_rodents_plots$yr),
                             FUN=mean)
  return(yearly_rodents)
}

# read and clean raw data ------------------------------------------------------------------------
dat = read.csv("data/Rodents1977_2014.csv", as.is = TRUE,  colClasses = c(note1='character'))

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"

# Only select samples in control plots
dat = dat[dat$plot %in% c(1,2,4,8,9,11,12,14,17,22),]

# Only select rodent species
dat = dat[dat$species %in% c('AH','BA','DM','DO','DS','DX','NA','NX','OL','OT','OX','PB',
                             'PE','PF','PH','PI','PL','PM','PP','PX','RF','RM','RO','RX',
                             'SF','SH','SO','SS','ST','SX','UR',''),]

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

#aggregate data --------------------------------------------------------------
yearly_rodents = AggregateByYear(dat)

summer_rodents = SeasonalAvg(dat,c(4,5,6,7,8,9))
