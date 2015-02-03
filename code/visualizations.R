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
  stats$registered <- as.factor(stats$registered)
  g <- ggplot(stats, aes(x=month, y=num_trips, group=registered, colour=registered)) + geom_line()+
    ggtitle('Number of Trips') + labs(x='month', y='Trips') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'num_trips_bytype.pdf'))
  plot(g)
  dev.off()
  
  g <- ggplot(stats, aes(x=month, y=avg_duration/60, group=registered, colour=registered)) + geom_line()+
    ggtitle('Average Duration of Trip') + labs(x='month', y='Duration (Minutes)') + scale_y_continuous(labels = comma)
  g <- common_format(g)
  pdf(paste0(graph,'avg_duration_bytype.pdf'))
  plot(g)
  dev.off()
  
}