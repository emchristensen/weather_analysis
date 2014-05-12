source('portal_weather/get_period_dates.r')

library(sqldf)

periods_to_ts = function(dat) {
  # This function takes rodent data taken at irregular "periods" and puts it in timeseries form
  # with regular spacing between time steps
  pframe = get_period_dates(dat)
  #pframe$date = as.Date(paste(pframe$yr,pframe$mo,pframe$dy,sep='-'))
  start = pframe[1,]
  end = pframe[length(pframe$mo),]
  desired_dates = data.frame(date=seq.Date(from=as.Date('1977-07-15'),
                      to=as.Date(paste(end$yr,end$mo,end$dy,sep='-')),
                      by='month'))
  
  p = vector()
  for (ind in seq(length(desired_dates$date))) {
    d = desired_dates$date[ind]
    p[ind] = max(0,pframe$period[which(pframe$date<=d+15 & pframe$date>=d-14)])
  }
  desired_dates$period = p
  
  count_period = aggregate(dat$yr,
                           by=list(period=dat$period),
                           FUN=length)
  
  count_ts = sqldf("SELECT * 
                      FROM desired_dates
                      LEFT JOIN count_period USING(period)")
  return(count_ts)
}