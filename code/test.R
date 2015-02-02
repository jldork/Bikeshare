#Explore trips dataset found on capital bikeshare website - generate some graphs
rm(list=ls())

#directory structure
proj_dir <- '~/Desktop/Bikeshare/'
src <- paste0(proj_dir, 'code/')
graph <- paste0(proj_dir, 'graphs/')
sub <- paste0(proj_dir, 'submission/')
tab <- paste0(proj_dir, 'tables/')
raw <- paste0(proj_dir, 'Data/')

#libaries and functions
library(data.table)
source(paste0(src,'utils.R'))

load(paste0(raw,'aggregate_trips.RData'))

trips <- trips[1:1000,]
trips$duration <- parse_duration(trips$duration)
trips$starttime <- parse_time(trips$startdate)

data <- data.table(trips)
data[,parse_duration(duration)][1:5,]