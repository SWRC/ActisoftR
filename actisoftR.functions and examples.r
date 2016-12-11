
install.packages("dplyr", "ggplot2", "scales", "readr", "lubridate")

library("dplyr")
library("ggplot2")
library("scales") # used for the x-axis tick values 
library("readr")
library("lubridate")

# *** Please, save the 'data' folder in your working directory

# Example of setting my working directory
setwd("C:/Edgar/Google Drive/Jobs/Sleep/2016-12-09") 


# *** This first part will import the data ***

# Importing Actiware data (for Philips-Respironics actigraphs)
# Importing all the CSV files simultaneously.  
files <- list.files( path = "data\\Actiwatch", pattern = "*.csv") # list all the csv files 
dat <- do.call(rbind, lapply(paste("data\\Actiwatch\\", files, sep = ""), function(x) read.csv(x, sep = ",", header = TRUE, skip = 0)))

dat <- tbl_df(dat) # table dataframe 

dat <- dat[, 1 : 21] # not sure here why till 21. Not relevant the other variables? 
dat$datime_start <- paste( as.POSIXct( strptime( dat$start_date, format = "%d/%m/%Y"), tz = "UTC"), dat$start_time)
dat$datime_end   <- paste( as.POSIXct( strptime( dat$end_date, format = "%d/%m/%Y"), tz = "UTC"), dat$end_time)

fil <- filter(dat, interval_type == "SLEEP" | interval_type == "REST") # selecting only sleep and rest periods
fil <- distinct(fil, subject_id, interval_type, interval_number, datime_start, datime_end) # for now using just few columns 
#print(tbl_df(fil), n = 400)


# Importing from AMI 
files2 <- list.files( path = "data\\AMI", pattern = "*.csv") # list all the csv files 
dat2 <- do.call(rbind, lapply(paste("data\\AMI\\", files2, sep = ""), function(x) read.csv(x, sep = ",", header = TRUE, skip = 0)))

dat2 <- tbl_df(dat2) # table dataframe 
#print(tbl_df(dat2), n = 400)


# Importing flight Sleep-Work data type

flight <- read.csv(file = 'data\\Delta\\Delta.csv', sep = ",", header = TRUE, skip = 0)

flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))

flight <- flight[,-c(2,3)]
colnames(flight) <- c("subject_id", "interval_type", "datime_start", "datime_end") # using unique variables system

flight <- tbl_df(flight) # table dataframe 


################################################
# *** This section contains some ActisoftR functions ***

# Darwent plot (Alternative 1: local plot) 
plot.Darwent <- function(x, acolor, shade = TRUE, ...){
    foo <- as.data.frame(x)
    foo <- mutate(foo, datime_start = as.POSIXct(datime_start,tz = "UTC"), datime_end = as.POSIXct(datime_end,tz = "UTC")) 
    
    if(missing(acolor)) {acolor = c("black", "#56B4E9")} # Defining the colors
    
    part <- nrow(distinct(x,subject_id)) # number of participants. It defines the height of the plot
    
    p <- ggplot() + 
      geom_segment(data = foo, aes(colour = interval_type, x = datime_start, xend = datime_end, y = subject_id, yend = subject_id),
                   size = 15) + { # to add the shade periods
      if(shade == TRUE) geom_rect(data = local.night.shade(x = x), aes(xmin = as.POSIXct(shadow.start, tz = "UTC"),
                                                     xmax = as.POSIXct(shadow.end, tz = "UTC"), ymin = 0, ymax = Inf), alpha = 0.075, fill = "green") } +
      theme_bw() +
      xlab("Date-Time") +
      ylab("Participant(s)") +
      theme_classic() +
      scale_color_manual(values = acolor) +
      scale_fill_manual(name = "Act")
    
    # resizing the plotting area 
    windows()
    resize.win <- function(Width = 12, Height = 5){dev.off(); 
      windows(record = TRUE, width = Width, height = Height)}
    resize.win(14, 2 * part)
    
    p + scale_x_datetime(breaks = date_breaks("12 hour"),  # "12 hour" or "1 day"
                         minor_breaks = date_breaks("12 hour"),
                         labels = date_format("%y-%m-%d %H.%M", 
                                            tz = "UTC")) +   
      theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) + 
      theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
      theme(plot.title = element_text(color = "black", 
                                      #face = 'bold', 
                                      size = 18, 
                                      hjust = 0.5)) +
      theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
      ggtitle("Darwent plot")
} 


# Slice a data.frame 
portion <- function(x, from , to , all = TRUE, ...){
    part <- as.vector (distinct(x, subject_id) ) #participant
    mat2 <-  NULL
    
    for (i in 1 : length(from)){
      mat <- filter(x, subject_id == part[[i,1]], datime_start >= from[i], datime_end <= to[i] )
      mat2 <- rbind(mat2 , mat)
      mat2 <- na.omit(mat2)
    }
   mat2
  }



# Local period sleep time
# used by the Darwent plot
local.night.shade <- function(x, shadow.start = "10:00:00", shadow.end = "10:00:00", ...){
  first.last <- c(min(x$datime_start), max(x$datime_end))
  first.last.date <- as.Date(first.last)
  seqdates <- seq.Date(first.last.date[1], first.last.date[2],1)
  shadow.startend <- data.frame(shadow.start = as.POSIXct(paste(seqdates, "10:00:00"), format="%Y-%m-%d %H:%M:%S"),
                                shadow.end = as.POSIXct(paste(seqdates, "20:00:00"), format="%Y-%m-%d %H:%M:%S"))
  shadow.startend
}

# Example
a <- local.night.shade(x = flight)


# Summary of the total amount of sleep in a period
amount.sleep <- function(x, from , to , ...){
    aportion <- portion(x, from,  to) # Select a portion of the data.frame 
    aportion = filter(aportion, interval_type == "SLEEP")
    aportion$dif <- paste(difftime(aportion$datime_end, aportion$datime_start, units = "hours") ) 
    aportion <- as.data.frame(aportion)
    aportion$dif <- as.numeric(aportion$dif)
  
    aportion %>% 
      group_by(subject_id) %>% 
      summarise(total.amount.sleep = sum(dif))
}


################################################
# *** Some examples ***

# Example: selecting a portion of the data.frame  
start.period <- c("2016-07-10", "2016-07-10")
end.period <- c("2016-07-20", "2016-07-20")
mystudy <- portion(fil, from = start.period,  to = end.period)

# Example: amount of sleep
start.period <- c("2016-07-06", "2016-07-06")
end.period <- c("2016-07-22", "2016-07-22")
amount.sleep(fil, from = start.period,  to = end.period)
  
# Example: Darwent plot
# using flight data
# It will open graphics device
plot.Darwent(x = flight, acolor = c("#56B4E9", "#990000")) 

# plotting the same without night period shades
plot.Darwent(x = flight, acolor = c("#56B4E9", "#990000"), shade = FALSE) 

# Example Respironics study
# plotting flight study without night period shade
plot.Darwent(x = fil, shade = FALSE) 

# plotting just period of time  
plot.Darwent(x = mystudy) 



