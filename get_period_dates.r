

get_period_dates = function(dat) {
  # This function extracts the dates of each trapping period (for periods > 0)
 periods = dat[,2:5]                         # extract mo/dy/yr/period
 periods = periods[periods$period > 0,]      # remove non-census trapping events (period < 0)
 periods = unique(periods)                   # reduce to unique mo/dy/yr/period lines
 pframe = data.frame()
 for (p in unique(periods$period)) {
   pframe = rbind(pframe,periods[which.max(periods$period==p),])
 }
 return(pframe)
}