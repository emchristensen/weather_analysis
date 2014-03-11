YearlyAvg = function(dat) {
  #this function calculates rodent abundance (avg per plot) per year
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

YearlyTotal = function(dat) {
  #this function calculates total rodent abundance per year
  yearly_rodents = aggregate(dat$Record_ID,by=list(Year = dat$yr),FUN=length)
  return(yearly_rodents)
}

MonthlyTotal = function(dat) {
  #this function calculates total rodent abundance per period
  monthly_rodents = aggregate(dat$Record_ID,by=list(Year=dat$period),FUN=length)
  return(monthly_rodents)
}

# read in data ------------------------------------------------------------------------------
dat = read.csv("data/Rodents1977_2014.csv", as.is = TRUE,  colClasses = c(note1='character'))

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"

# Erroneous data has negative sampling period and is just removed for now
dat = dat[dat$period > 0, ]

# Only select samples in control plots
#dat = dat[dat$plot %in% c(1,2,4,8,9,11,12,14,17,22),]

# Only select rodent species
sigmodon = dat[dat$species %in% c('SH','SF','SO','SX'),]
sf = dat[dat$species %in% c('SF'),]
sh = dat[dat$species %in% c('SH'),]
so = dat[dat$species %in% c('SO'),]
sx = dat[dat$species %in% c('SX'),]

pf = dat[dat$species %in% c('PF'),]

# functions ------------------------------------------------------------------------------------
total_sig = YearlyTotal(sigmodon)
total_sf = YearlyTotal(sf)
total_sh = YearlyTotal(sh)
total_so = YearlyTotal(so)
total_sx = YearlyTotal(sx)

total_pf = YearlyTotal(pf)

month_sig = MonthlyTotal(sigmodon)
month_sf = MonthlyTotal(sf)
month_sh = MonthlyTotal(sh)
month_so = MonthlyTotal(so)
month_sx = MonthlyTotal(sx)
