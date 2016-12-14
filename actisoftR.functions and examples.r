install.packages("dplyr", "ggplot2", "scales", "readr", "lubridate", "tibble")

library("dplyr")
library("ggplot2")
library("scales") # used for the x-axis tick values 
library("readr")
library("tibble")
library("lubridate")

# *** Please, save the 'data' folder in your working directory

# Example of setting my working directory
setwd("C:/Edgar/Google Drive/Jobs/Sleep/2016-12-12") 


# *** This first part will import the data ***

# Importing Actiware data (for Philips-Respironics actigraphs)
# Importing all the CSV files simultaneously.  
files <- list.files( path = "data\\Actiwatch", pattern = "*.csv") # list all the csv files 
dat <- do.call(rbind, lapply(paste("data\\Actiwatch\\", files, sep = ""), function(x) read.csv(x, sep = ",", header = TRUE, skip = 0)))
dat <- tbl_df(dat) # table dataframe 

# selecting the useful variables. 
# [remove this comment final version] These are the variables marked in red in the Variable names dictionary ActisoftR file
useful <- c("analysis_name", "subject_id", "interval_type", "interval_number", "start_date", 
            "start_time", "end_date", "end_time", "duration", "efficiency", "sleep_time")
dat <- select_(dat, .dots = useful)
dat <- add_column(dat, actigraph = rep("Actiwatch",nrow(dat)), .before = 1)
dat$bad <- "NaN" 


# Importing from AMI actigraphs 
files2 <- list.files( path = "data\\AMI", pattern = "*.csv") # list all the csv files 
dat2 <- do.call(rbind, lapply(paste("data\\AMI\\", files2, sep = ""), function(x) read.csv(x, sep = ",", header = TRUE, skip = 0)))

dat2 <- tbl_df(dat2) # table dataframe 
#print(tbl_df(dat2), n = 400)
dat2 <- dat2[!is.na(dat2$IntNum),] # removing row containing NA's (Those giving the mean and the sd)

# [remove this comment final version] Variables marked in red in the Variable names dictionary ActisoftR file
useful2 <- c("ID",  "IntName", "IntNum", "sdate", "stime", "edate", "etime", "dur", "pslp", "smin", "bad")

dat2 <- select_(dat2, .dots = useful2)
dat2 <- add_column(dat2, actigraph = rep("AMI", nrow(dat2)), .before = 1)
dat2 <- add_column(dat2, analysis_name = rep("NaN",nrow(dat2)), .before = 2)

# using the variables names from Actiware
dat2 <- rename(dat2, subject_id = ID, interval_type = IntName, interval_number = IntNum, start_date = sdate, start_time = stime,
       end_date = edate, end_time = etime, duration = dur, sleep_time = smin, efficiency = pslp)

alldata <- rbind(dat,dat2) # combining both datasets
alldata$datime_start <- paste( as.POSIXct( strptime( alldata$start_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$start_time)
alldata$datime_end   <- paste( as.POSIXct( strptime( alldata$end_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$end_time)

# Standarizing the interval_type
alldata$interval_type[alldata$interval_type == "Down"] <- as.factor("REST")
alldata$interval_type[alldata$interval_type == "O - O"] <- as.factor("SLEEP")
alldata$interval_type[alldata$interval_type == "Up"] <- as.factor("ACTIVE")

alldata2 <- alldata %>%
  group_by(interval_number, start_date, start_time, duration) %>%
  filter(all(interval_type != "24-Hr")) # removing intervals = "24-Hr"

View(alldata2)




# Importing flight Sleep-Work data type (Not real data)
# this data is from the excel sheet Example01 example02 Darwent Plot v3-1.xls

flight <- read.csv(file = 'data\\Delta\\Delta.csv', sep = ",", header = TRUE, skip = 0)

flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))

flight <- flight[,-c(2,3)]
colnames(flight) <- c("subject_id", "interval_type", "datime_start", "datime_end") # using unique variables system

flight <- tbl_df(flight) # table dataframe 



################################################
# *** This section contains some ActisoftR functions ***

# Darwent plot (Alternative 1: local plot) 
plot.Darwent <- function(x, acolor, shade = TRUE, datebreaks = "12 hour", ...){
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
    
    p + scale_x_datetime(breaks = date_breaks(datebreaks),  # "12 hour" or "1 day"
                         minor_breaks = date_breaks(datebreaks),
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
    part <- as.matrix (distinct(x, subject_id) ) #participant
    mat2 <-  NULL
    
    if(length(from) != length(to))
    stop("the variables from and to must have same lenght ")
    
    for (i in 1 : length(part)){
      mat <- filter(x, subject_id == part[[i,1]], datime_start >= from[i], datime_end <= to[i] )
      mat2 <- rbind(mat2 , mat)
      mat2 <- na.omit(mat2)
    }
   mat2
  }



# Local period sleep time
# used by the Darwent plot
local.night.shade <- function(x, shadow.start = "10:00:00", shadow.end = "20:00:00", ...){
  first.last <- c(min(x$datime_start), max(x$datime_end))
  first.last.date <- as.Date(first.last)
  seqdates <- seq.Date(first.last.date[1], first.last.date[2],1)
  shadow.startend <- data.frame(shadow.start = as.POSIXct(paste(seqdates, "10:00:00"), format="%Y-%m-%d %H:%M:%S"),
                                shadow.end = as.POSIXct(paste(seqdates, "20:00:00"), format="%Y-%m-%d %H:%M:%S"))
  shadow.startend
}

# Example
a <- local.night.shade(x = flight)




################################################
# *** Some examples ***

fil <- filter(alldata2, interval_type == "SLEEP" | interval_type == "REST") # selecting only sleep and rest periods
fil <- tbl_df(fil) 
fil <- select(fil, subject_id, interval_type, interval_number, datime_start, datime_end)

# Example Respironics study
# plotting flight study without night period shade
plot.Darwent(x = fil, shade = FALSE, datebreaks = "1 day") 

# Example: Darwent plot
# using flight data
# It will open graphics device
plot.Darwent(x = flight, acolor = c("#56B4E9", "#990000")) 

# plotting the same without night period shades
plot.Darwent(x = flight, acolor = c("#56B4E9", "#990000"), shade = FALSE) 


# Example: selecting a portion of the data.frame  
start.period <- rep("2016-07-10", 4)
end.period <- rep("2016-07-20", 4)
mystudy <- portion(fil, from = start.period,  to = end.period)
# plotting just period of time  
plot.Darwent(x = mystudy) 



