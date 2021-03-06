#visualization functions

#print out general stats by year
general_stats <- function(trips, graph){
  trips$year <- as.numeric(format(trips$startdate, "%Y"))
  #generating visualizations
  stats <- sqldf('select year,
                        avg(duration) as avg_duration,
                        count(distinct startstation) as num_stations,
                        count(distinct bike) as num_bikes,
                        avg(registered) as avg_reg,
                        count(*) as num_trips
               from trips
               group by year')
  
  g <- ggplot(stats, aes(x=year, y=num_trips, group='identity')) + geom_bar(stat='identity')+
    ggtitle('Number of Trips') + labs(x='Year', y='Trips') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'num_trips.pdf'))
  plot(g)
  dev.off()
  
  
  g <- ggplot(stats, aes(x=year, y=num_stations, group='identity')) + geom_bar(stat='identity')+
    ggtitle('Number of Stations') + labs(x='Year', y='Stations') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'num_stations.pdf'))
  plot(g)
  dev.off()
  
  g <- ggplot(stats, aes(x=year, y=num_bikes, group='identity')) + geom_bar(stat='identity')+
    ggtitle('Number of Bikes') + labs(x='Year', y='Bikes') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'num_bikes.pdf'))
  plot(g)
  dev.off()
  
  g <- ggplot(stats, aes(x=year, y=avg_reg, group='identity')) + geom_bar(stat='identity')+
    ggtitle('Average Registered') + labs(x='Year', y='Averge Registered') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'avg_reg.pdf'))
  plot(g)
  dev.off()
  
}


#print out general stats with line graphs by user type on a monthly basis
general_stats_bytype <- function(trips, graph){
  trips$month <- as.Date(paste0(format(trips$startdate, "%Y-%m"),'-01'))
  #generating visualizations
  stats <- sqldf('select month,
                        registered,
                        avg(duration) as avg_duration,
                        count(distinct startstation) as num_stations,
                        count(distinct bike) as num_bikes,
                        avg(registered) as avg_reg,
                        count(*) as num_trips
               from trips
               group by month, registered')
  
  stats <- subset(stats,!is.na(month))
  stats <- subset(stats, month<as.Date('2014-09-01'))#cut off to remove outlier
  stats$registered <- factor(stats$registered, levels=c(0,1), labels=c('Casual','Registered'))
  g <- ggplot(stats, aes(x=month, y=num_trips, group=registered, colour=registered)) + geom_line()+
    ggtitle('Number of Trips') + labs(x='Year', y='Trips') + scale_y_continuous(labels = comma)+
    theme(legend.title=element_blank())
  g <- common_format(g)
  pdf(paste0(graph,'num_trips_bytype.pdf'), width=14, height=7)
  plot(g)
  dev.off()
  
  g <- ggplot(stats, aes(x=month, y=avg_duration/60, group=registered, colour=registered)) + geom_line()+
    ggtitle('Average Duration of Trip') + labs(x='Year', y='Duration (Minutes)') + scale_y_continuous(labels = comma)+
    theme(legend.title=element_blank())
  g <- common_format(g)
  pdf(paste0(graph,'avg_duration_bytype.pdf'), width=14, height=7)
  plot(g)
  dev.off()
  
}


#print stats about time of highest rides by rider type
freq_bytime <- function(trips, graph){
  trips$weekday <- weekdays(trips$startdate)
  registered <- sqldf('select weekday, count(*) as rides from trips where registered==1 group by weekday')
  casual <- sqldf('select weekday, count(*) as rides from trips where registered==0 group by weekday')
  
  prct <- registered$rides/sum(registered$rides)
  pdf(paste0(graph, 'registered_weekday.pdf'))
  pie(registered$rides, labels=paste(registered$weekday,percent(prct)), main="Registered Rides by Weekday")
  dev.off()
  
  prct <- casual$rides/sum(casual$rides)
  pdf(paste0(graph, 'casual_weekday.pdf'))
  pie(casual$rides, labels=paste(casual$weekday, percent(prct)), main="Casual Rides by Weekday")
  dev.off()
}