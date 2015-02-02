#visualization functions

#print out general stats by year
general_stats <- function(trips){
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

