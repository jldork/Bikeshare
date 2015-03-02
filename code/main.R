#main file to analyze capital bikeshare trips data

rm(list=ls())

#directory structure
proj_dir <- '~/Desktop/Bikeshare/'
src <- paste0(proj_dir, 'code/')
graph <- paste0(proj_dir, 'graphs/')
sub <- paste0(proj_dir, 'submission/')
tab <- paste0(proj_dir, 'tables/')
raw <- paste0(proj_dir, 'Data/')

#libaries and functions
source(paste0(src,'utils.R'))
source(paste0(src,'visualizations.R'))
library(sqldf)
library(ggplot2)
library(zoo)

generate_date <- 0

if(generate_date==1){
  #aggregate many csv files
  trips <- aggregate_data(raw, output=paste0(raw,'aggregate_trips.RData'))
  
  #clean data and add variables
  trips <- clean_data(trips)
  
  save(trips, file=paste0(raw,'aggregate_trips_clean.RData'))
  
}

load(paste0(raw,'aggregate_trips_clean.RData'))
trips <- subset(trips, !is.na(trips$startdate))

#print out general stats by year
#general_stats(trips, graph)

general_stats_bytype(trips, graph)

#histogram of average duration
#duration_hist(trips, graph)

freq_bytime(trips, graph)

#riders by days of the week

#Most popular destinations-station pairs

#Rate revene for bikeshare

#casual membership rides vs registered members

#average lifespan of a bike - how old is the current fleet?

#average speed a traveler travels from one place to another - google maps data needed for estimated routes