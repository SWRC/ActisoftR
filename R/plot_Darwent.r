#' Plots participants' SLEEP/WAKE intervals.
#'
#' @param x a dataframe.
#' @param shade if TRUE, it will draw in light green the home night period. FALSE by default.
#' @param local.shade if TRUE, it will draw in gray the local night period. FALSE by default.
#' @param datebreaks is the distance between breaks in the x-axis. "12 hour" by default.
#' @param acolor specifies the colors for the interval_type.
#' @param decal is a parameter for shifting the start date.
#' @param export if TRUE, it will export the data as CSV.
#' @param base matches the participants at the same start date.
#' @param si defines the size of the geom_segment, 8 by default.
#' @param tz is the times zone. tz = "UTC" by default.
#' @param ... optional parameters.
#' @return a plot.
#'
#' @examples
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' library("dplyr")
#' fil <- dplyr::filter(acti_data, interval_type == "SLEEP" | interval_type == "REST", actigraph_brand == "Actiware")
#' plot_Darwent(x = fil, shade = FALSE, datebreaks = "1 day")
#' # Example # 2
#' # using flight data
#' flight <- read.csv(file = 'C:\\1\\EXAMPLE_DATA\\Work\\work.csv', sep = ",",header = TRUE, skip = 0)
#' flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight <- flight[,-c(3,4)]
#' colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type","datime_start", "datime_end")
#' flight <- tbl_df(flight)
#' part_homeTZ <- data.frame (subject_ID = c("example01", "example01_AMI", "example02"), homeTZ = rep(2,3))
#' plot_Darwent(x = flight, acolor = c("#56B4E9", "#990000"), shade = TRUE, local.shade = TRUE, datebreaks = "12 hour")
#'
#' # Example # 3
#' decal = data.frame(subject_ID = c("participant01" ,"participant02"), dec = c(0,-2))
#' plot_Darwent(x = fil, shade = FALSE, datebreaks = "12 hours", decal = decal)
#'
#'# Example # 4 reversing the order in the y axis from top to bottom
#'fil$subject_ID = with(fil, factor(subject_ID, levels = rev(levels(subject_ID))))
#' q <- plot_Darwent(x = fil, shade = FALSE, datebreaks = "1 day", show_plot = FALSE)
#' datebreaks = "1 day"
#' q + scale_x_datetime(breaks = date_breaks(datebreaks),  # scale_x_datetime "12 hour" or "1 day"
#'                      minor_breaks = date_breaks(datebreaks),
#'                      labels = date_format("%H.%M", tz = "UTC")) +
#'   theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) +
#'   theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
#'   theme(plot.title = element_text(color = "black", size = 18, hjust = 0.5)) +
#'   theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
#'   xlab("Date-Time2") + ylab("Part.") +
#'   ggtitle("Darwent plot with reverse order in y axis")
#'
#'
#' @export
#' @importFrom grDevices dev.new dev.off x11 windows
#' @importFrom dplyr distinct left_join
#' @importFrom scales date_breaks date_format
#' @import ggplot2
#' @rdname plot.Darwent


plot_Darwent <- function(x, shade = FALSE, local.shade = FALSE, datebreaks = "12 hour", base = "TRUE", acolor, decal, export = FALSE, show_plot = TRUE, si, shadow.start = "20:00:00", shadow.end = "06:00:00", tz = "UTC",...){
  subject_ID <- datime_start <- interval_type <- datime_end <- grayzone.start <- grayzone.end <- dec <- NULL

  #part_homeTZ <- interval_type <- grayzone.start <- grayzone.end <- subject_ID <- NULL
  foo <- as.data.frame(x)
  foo <- foo[!is.na(foo$datime_start),] # remove the NA's
  foo <- foo[!is.na(foo$datime_end),]

  foo$datime_start <- as.POSIXct(foo$datime_start, tz = tz)
  foo$datime_end = as.POSIXct(foo$datime_end, tz = tz)

  foo <- arrange(foo, interval_type)
  
  # number of participants. It defines the height of the plot
  part <- length(unique(x$subject_ID))
  participant <- as.factor(unique(x$subject_ID))
  participant <- droplevels(participant)
activ <- length(unique(x$interval_type))
if( any (unique(x$interval_type) %in% c("ACTIVE" ,"EXCLUDED" ,"REST", "SLEEP" , "WORK" ) == FALSE) ){
  warning("enter the argument acolor manually for each interval_type")
 }
  
  if(missing(decal)){ decal = data.frame(subject_ID = participant, dec = rep(0, part))}

  if(base == "TRUE"){ # matching the participants at the same start date
    #foo <- as.data.frame(foo)
    #foo$datime_start <- as.POSIXct(foo$datime_start, tz = tz)
    #foo$datime_end = as.POSIXct(foo$datime_end,tz = tz)

    foo2 <-  foo %>%
      group_by(subject_ID) %>%
      summarise(datime_start = min(datime_start))

    foo2$datime_start <- round(foo2$datime_start,"days")

    minim <- min(foo2$datime_start)
    minim <- round(minim,"days")

    foo2$minim <- minim
    foo2$base <- foo2$minim - foo2$datime_start

    foo2$datime_start <- as.Date(foo2$datime_start)
    foo2$minim <- as.Date(foo2$minim)

    foo3 <- foo %>% left_join(foo2, by = "subject_ID")

    foo3$adj <- foo3$datime_start.x + lubridate::seconds(foo3$base)
    foo3$adj2 <- foo3$datime_end + lubridate::seconds(foo3$base)

    foo3$datime_start <- foo3$adj
    foo3$datime_end <- foo3$adj2

    foo3 <- droplevels(foo3)
    foo3$subject_ID <- as.factor(foo3$subject_ID)

    foo4 <- foo3 %>% left_join(decal, by = "subject_ID")
    foo4$decal <- foo4$dec * 60 * 60 * 24

    foo4$datime_start <- foo4$datime_start + foo4$decal
    foo4$datime_end <- foo4$datime_end + foo4$decal
    foo <- foo4
  }

  # to export as csv 
  if (export == TRUE){
    usef <- c("subject_ID", "datime_start", "datime_end", "interval_type")
    Darw_data <- foo[, usef]
    Darw_data2 <-  Darw_data %>% left_join(decal, by = "subject_ID")
    write.csv(Darw_data2, "Darw_data.csv")
  }


  foo$interval_type <- as.factor(as.character(foo$interval_type))
  #foo <- arrange(foo,desc(interval_type)) #subject_ID, datime_start

#if(missing(acolor)) {acolor = c("#009E73", "#D55E00", "black", "#56B4E9","#F0E442", "#CC79A7")} # Defining the colors
#("black", "#56B4E9", "#009E73", "#D55E00", "#F0E442", "#CC79A7" )
if(missing(si)){ si = rep(8, activ)}

  p <- ggplot2::ggplot() +
    geom_segment(data = foo, aes(colour = interval_type, x = datime_start, xend = datime_end, y = subject_ID, yend = subject_ID,
                 size = interval_type)) +
    scale_size_manual(values = si)  + { #seq(si, si + length(unique(x$interval_type)) - 1,length.out = length(unique(x$interval_type))) # adding the shade periods
    # scale_size_discrete(range = si)

                   if(shade == TRUE) geom_rect(data = home.night.shade(x = foo, shadow.start, shadow.end, ...), aes(xmin = shadow.start,
                                                                                   xmax = shadow.end,
                                                                                   ymin = 0, ymax = part + 0.5), alpha = 0.175, fill = "green")
                   } + {
                  if(local.shade == TRUE) {geom_segment(data = local.night.shade(x = foo, part_homeTZ = part_homeTZ),
                                                       aes(colour = interval_type, x = as.POSIXct(grayzone.start,tz = tz),
                                                           xend = as.POSIXct(grayzone.end, tz = tz),
                                                           y = subject_ID, yend = subject_ID), size = 12, col = "black", alpha = 0.22)}
                                                                                     else part_homeTZ <- NULL} +
    theme_bw() + xlab("Time") +  ylab("Participant(s)") +
    theme_classic() +  
    { if(missing(acolor)) {scale_color_manual(values = c("ACTIVE" = "#009E73", "EXCLUDED" = "#D55E00", "REST" =  "black",
                                                     "SLEEP" = "#56B4E9", "WORK" = "#CC79A7")) } } +
    scale_fill_manual(name = "Act")

  # resizing the plotting area

  if(show_plot == TRUE){
  x11() #dev.new()
  resize.win <- function(Width = 12, Height = 5){dev.off();
    dev.new(record = TRUE, width = Width, height = Height)}
  resize.win(14, 2 * part)

  p + scale_x_datetime(breaks = date_breaks(datebreaks),  # scale_x_datetime "12 hour" or "1 day"
                       minor_breaks = date_breaks(datebreaks),
                       labels = date_format("%H:%M", #"%y-%m-%d %H.%M"
                                            tz = tz)) +
    theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
    theme(plot.title = element_text(color = "black",
                                    size = 18,
                                    hjust = 0.5)) +
    theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
    ggtitle("Darwent plot")

  }

  else
    p + scale_x_datetime(breaks = date_breaks(datebreaks),  # scale_x_datetime "12 hour" or "1 day"
                         minor_breaks = date_breaks(datebreaks),
                         labels = date_format("%H:%M", # "%y-%m-%d %H.%M"
                                              tz = tz)) +
    theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
    theme(plot.title = element_text(color = "black",
                                    #face = 'bold',
                                    size = 18,
                                    hjust = 0.5)) +
    theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
    ggtitle("Darwent plot")
}



#' Plots SLEEP intervals.
#'
#' @param dat a dataframe.
#' @param acolor the color of the lines
#' @param si defines the size of the geom_segment, 1.25 by default.
#' @param ... Optional parameters
#' @return a plot
#'
#' @examples
#' # Example 1
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' library("dplyr")
#' fil <- dplyr::filter(acti_data, interval_type == "SLEEP" | interval_type == "REST", actigraph_brand == "Actiware", subject_ID=="participant01")
#' fil <- dplyr::tbl_df(fil)
#' fil <- dplyr::select(fil, subject_ID, interval_type, interval_number, datime_start, datime_end)
#' #dat <- read.csv("C:\\1\\EXAMPLE_DATA\\AMI\\example01_AMI.csv")
#' plot_long (dat = fil)

#' # Example 2
#' fil2 <- dplyr::filter(acti_data, subject_ID=="participant01")
#' plot_long (dat = fil2)

#' @export
#' @importFrom grDevices dev.new dev.off x11 windows
#' @importFrom dplyr distinct
#' @importFrom scales date_breaks date_format
#' @importFrom lubridate seconds
#' @import ggplot2
#' @rdname plot_long

plot_long <- function(dat, acolor, si, ...){
  sinceMidnight_end <- sinceMidnight_start <- interval_type <- NULL
  ###############################################
  # Filename: plot_long.R
  # Purpose:  Plot sleep and wake data (long)
  # Created:  2016-12-12 LJW, minor edits by ESF
  # Updated:  2016-04-24
  ###############################################

  # import sleep/wake data
  #dat <- read.table(file = "C:\\Users\\lwu1\\Dropbox\\ActisoftR\\EXAMPLE_DATA\\example01_AMI.txt", sep = "\t", header = TRUE, skip = 0)
  dat <- dat[!is.na(dat$datime_start),]
  dat <- dat[!is.na(dat$datime_end),]
  
  dat <- arrange(dat, interval_type)
  
  
  dat$subject_ID <- gsub("_.*$", "", dat$subject_ID)
  #dat <- dat[!is.na(dat$interval_number), ] # get rid of the 2x summary lines

  # make a datetime variable
  dat$datime_start <- as.POSIXct(dat$datime_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  dat$datime_end <- as.POSIXct(dat$datime_end, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  #dat$datime_end   <- as.POSIXct(paste(as.POSIXct(strptime(dat$edate, format = "%d/%m/%Y"), tz = "UTC"), dat$etime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  # extract the date and make into factor counter
  dat$sequence <- as.numeric(factor(as.Date(dat$datime_start)))

  # make time into mins since midnight - make the sleeps that cross midnight plot across 2 lines
  dat$sinceMidnight_start <- as.numeric(difftime(dat$datime_start, trunc(dat$datime_start, "day"), units = "mins"))
  dat$sinceMidnight_end   <- as.numeric(difftime(dat$datime_end,   trunc(dat$datime_end,   "day"), units = "mins"))

  # how many go across midnight? 17/34
  dat %>% filter(sinceMidnight_end < sinceMidnight_start)

#if(missing(acolor)) {acolor = c("black", "#56B4E9")} # Defining the color
if(missing(acolor)) {acolor = c("black", "#56B4E9", "#009E73", "#D55E00")} # Defining the colors
activ <- length(unique(dat$interval_type))
  if(missing(si)){ si = rep(1.25, activ)}
  p <- ggplot(data = dat)
  # plot only the easy ones
  #p <- p + geom_segment(aes(x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence), colour = interval_type, size = 1.25, data = dat %>% filter(sinceMidnight_end > sinceMidnight_start))
  #p <- p + geom_segment(aes(x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence), size = 8, data = dat %>% filter(sinceMidnight_end > sinceMidnight_start))
  #p <- p + geom_segment(aes(colour = interval_type, x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence), size = 8, data = dat %>% filter(sinceMidnight_end > sinceMidnight_start))

  if(nrow(dat %>% filter(sinceMidnight_end > sinceMidnight_start)) > 0){
  p <- p + geom_segment(aes(colour = interval_type, x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence, size = interval_type), data = dat %>% filter(sinceMidnight_end > sinceMidnight_start))}
  # plot the x-midnight segments in 2 goes: one for the bit between sleep start and midnight
  if(nrow(dat %>% filter(sinceMidnight_end < sinceMidnight_start)) > 0){
  p <- p + geom_segment(aes(colour = interval_type, x = sinceMidnight_start, xend = 1440, y = sequence, yend = sequence, size = interval_type), data = dat %>% filter(sinceMidnight_end < sinceMidnight_start))
  # second bit for midnight until sleep offset
  p <- p + geom_segment(aes(colour = interval_type, x = 0, xend = sinceMidnight_end, y = sequence + 1, yend = sequence + 1, size = interval_type), data = dat %>% filter(sinceMidnight_end < sinceMidnight_start))}

  p <- p + scale_size_manual(values = si)
  # flip it upside down so newest dates are on the top of the figure
  p <- p + scale_y_reverse()
  p <- p + theme_bw()
  p <- p + ggtitle(gsub("_.*$", "", dat$subject_ID))
  p <- p + labs(x = "Sleep", y = "Days")
  p <- p + scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 360),
                              expand = c(0, 0), labels = c("00:00", "06:00", "12:00", "18:00", "00:00"))
  p <- p + scale_color_manual(values = acolor)
  p <- p + theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_blank(), # no grids
    panel.grid.minor.x = element_blank(), # no grids
    plot.margin = unit(c(5.5, 12.5, 5.5, 5.5), "points"),
    axis.title.y = element_text(size = 10, colour = "black", face = "bold", margin = margin(0, 0, 0, 0, unit = "points")),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold", margin = margin(10, 0, 0, 0, unit = "points")),
    axis.text  = element_text(size = 10, colour = "black", face = "bold")) # make y axis text pretty

  #x11()
  p
  
}


  #dev.new()
  #resize.win <- function(Width = 12, Height = 5){dev.off();
  #  dev.new(record = TRUE, width = Width, height = Height)}
  #resize.win(14, 5)

  ## add work periods
  #work_dat <- read.table(header = TRUE, sep = "\t",
  #                       text = "subject_ID	start_datime	end_datime
  #                       example01	4/07/2016 10:00	4/07/2016 18:00
  #                       example01	5/07/2016 10:00	5/07/2016 18:00
  #                       example01	6/07/2016 10:00	6/07/2016 18:00
  #                       example01	7/07/2016 10:00	7/07/2016 18:00
  #                       example01	8/07/2016 10:00	8/07/2016 18:00
  #                       example01	11/07/2016 10:00	11/07/2016 18:00
  #                       example01	12/07/2016 10:00	12/07/2016 18:00
  #                       example01	13/07/2016 10:00	13/07/2016 18:00
  #                       example01	14/07/2016 10:00	14/07/2016 18:00
  #                       example01	15/07/2016 10:00	15/07/2016 18:00
  #                       example01	18/07/2016 10:00	18/07/2016 18:00
  #                       example01	19/07/2016 10:00	19/07/2016 18:00
  #                       example01	20/07/2016 10:00	20/07/2016 18:00
  #                       example01	21/07/2016 10:00	21/07/2016 18:00
  #                       example01	22/07/2016 10:00	22/07/2016 18:00
  #                       example01	25/07/2016 10:00	25/07/2016 18:00
  #                       example01	26/07/2016 10:00	26/07/2016 18:00
  #                       example01	27/07/2016 10:00	27/07/2016 18:00
  #                       example01	28/07/2016 10:00	28/07/2016 18:00
  #                       example01	29/07/2016 10:00	29/07/2016 18:00")

  #str(work_dat)

  # make a datetime variable
  #work_dat$start_datime <- as.POSIXct(work_dat$start_datime, format = "%d/%m/%Y %H:%M", tz = "UTC")
  #work_dat$end_datime   <- as.POSIXct(work_dat$end_datime, format = "%d/%m/%Y %H:%M", tz = "UTC")
  #work_dat$type <- as.factor("work")


# save an output
#ggsave(filename = "C:\\1\\long.png", plot = p, width = 25, height = 20, units = c("cm") )
#head(work_dat)

# EOF


