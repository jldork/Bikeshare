library(ggplot2)
library(scales)
library(grid)
library(chron)

common_format <- function(g){
  #increasing margins to give space between labels and graph
  g <- g+ theme(plot.margin=unit(c(1,1,1,1),"cm"))+
    theme(plot.title= element_text(lineheight=3, size=24, vjust=3))+
    theme(axis.title.y= element_text(vjust=1))+
    #theme(axis.text.x= element_text( angle=20, hjust=1))+
    #increase spacing between graph and axis titles
    theme(axis.title.x= element_text( vjust=-2))+
    theme(text= element_text(size=16))+
    #remove margins in between data and axis
    #scale_y_continuous(expand=c(0,0)) +
    #remove background grid and add axis lines
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  return(g)
}



#Input vector string format - x*h x*min x*sec
#return duration in seconds
parse_duration <- function(vec){
  vec <- as.character(vec)
  result <- unlist(lapply(vec,duration_str))
  return(result)
}


#return duration in seconds given a string
duration_str <- function(str){
  temp <- strsplit(str, ' ')
  hours <- as.numeric(gsub('\\D','',temp[[1]][1]))
  mins <- as.numeric(gsub('\\D','',temp[[1]][2]))
  secs <- as.numeric(gsub('\\D','',temp[[1]][3]))
  total <- hours*60*60 + mins*60 + secs
  return(total)
}


#input - vector string format - %m/%d/%Y %h:%s
#output - seconds from midnight
parse_time <- function(vec){
  vec <- as.character(vec)
  result <- unlist(lapply(vec, time_str))
  return(result)
}


time_str <- function(str){
  temp <- strsplit(str, ' ')
  time <- temp[[1]][2]
  temp <- strsplit(time, ':')
  hours <- as.numeric(temp[[1]][1])
  mins <- as.numeric(temp[[1]][2])
  total <- hours*60*60 + mins*60
  return(total)
}

aggregate_data <- function(raw, output){
  #Merge all time period data into one dataset
  files <- c('2010-Q4',
             '2011-Q1','2011-Q2','2011-Q3','2011-Q4',
             '2012-Q1','2012-Q2','2012-Q3','2012-Q4',
             '2013-Q1','2013-Q2','2013-Q3','2013-Q4',
             '2014-Q1','2014-Q2','2014-Q3','2014-Q4')
  files <- paste0(files, '-Trips-History-Data.csv')
  
  trips <- data.frame()
  col_names <- c('duration','startdate','enddate','startstation','endstation','bike','type')
  for(i in files){
    print(paste('Loading File:',i))
    print(paste0(raw,i))
    tmp <- read.csv(paste0(raw,i))
    #member type is changed to type in 2012
    tmp_names <- gsub('Subscription.','',names(tmp)) #type for 2011
    tmp_names <- gsub('Member.','',tmp_names) #type for 2012Q1
    tmp_names <- gsub('Bike.Key','Type',tmp_names) #type for 2012Q2
    tmp_names <- gsub('Subscriber.','',tmp_names) #type for 2012Q2
    tmp_names <- gsub('Start.time','Start.date',tmp_names) #startdate for 2013Q2
    tmp_names <- gsub('\\W','',tmp_names) #remove all dot notation in names
    names(tmp) <- tolower(tmp_names)
    tmp <- tmp[col_names] #order columns before merging
    trips <- rbind(trips, tmp)
  }
  return(trips)
}

clean_data <- function(trips){
  #clean up parts of data
  trips$starttime <- parse_time(trips$startdate)
  trips$startdate <- as.Date(as.character(trips$startdate), "%m/%d/%Y")
  trips$duration <- parse_duration(trips$duration)
  trips$registered <- 1 - as.numeric(trips$type=='Casual') #dummy indicating if rider is a registered user
  trips$enddate <- NULL
  trips$type <- NULL
  
  return(trips)
  
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}