#load function for reading csv data
source('csv_to_dataframe.r')

weathfile = "data/Hourly_PPT_mm_1989_present_fixed_withgaps.csv"
weathframe = csv_to_dataframe(weathfile)

source('enso_ppt/munge_portal_rodents2.r')

daily_rodents = aggregate(dat$date,by=list(dat$date),FUN=length)
recent_rodents = daily_rodents[daily_rodents$Group.1 >= as.Date('1990-1-1'),]
