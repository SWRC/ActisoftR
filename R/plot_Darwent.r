#' Plots participants' SLEEP/WAKE intervals.
#'
#' @param x a dataframe.
#' @param shade if TRUE, it will draw in light green the home night period. FALSE by default.
#' @param local.shade if TRUE, it will draw in gray the local night period. FALSE by default.
#' @param datebreaks is the distance between breaks in the x-axis. "12 hour" by default.
#' @param acolor specifies the colors for the interval_type.
#' @param ... Optional parameters
#' @return a plot
#'
#' @examples
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' library("dplyr")
#' #fil <- acti_data[acti_data$interval_type == "SLEEP" | acti_data$interval_type == "REST",]
#' #fil <- fil[fil$actigraph == "Actiware",]
#' fil <- dplyr::filter(acti_data, interval_type == "SLEEP" | interval_type == "REST", actigraph == "Actiware")
#' fil <- dplyr::tbl_df(fil)
#' fil <- dplyr::select(fil, subject_ID, interval_type, interval_number, datime_start, datime_end)
#' plot_Darwent(x = fil, shade = FALSE, datebreaks = "1 day")
#' # Example # 2
#' # using flight data
#' flight <- read.csv(file = 'C:\\1\\EXAMPLE_DATA\\Work\\work.csv', sep = ",",header = TRUE, skip = 0)
#' flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight <- flight[,-c(3,4)]
#' colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type","datime_start", "datime_end")
#' flight <- tbl_df(flight)
#' homeTZ <- data.frame (subject_ID = c("example01", "example01_AMI", "example02"), homeTZ = rep(2,3), decal = c(0,0,5))
#' head(flight)
#' #plot_Darwent(x = flight, acolor = c("#56B4E9", "#990000"), shade = TRUE, datebreaks = "1 day")
#' part_homeTZ <- data.frame (subject_ID = c("example01", "example01_AMI", "example02"), homeTZ = rep(2,3), decal = c(0,0,5))
#' plot_Darwent(x = flight, acolor = c("#56B4E9", "#990000"), shade = TRUE, local.shade = TRUE, datebreaks = "1 day")
#' @export
#' @importFrom grDevices dev.new dev.off x11 windows
#' @importFrom dplyr distinct
#' @importFrom scales date_breaks date_format
#' @import ggplot2
#' @rdname plot.Darwent



plot_Darwent <- function(x, acolor, shade = FALSE, local.shade = FALSE, datebreaks = "12 hour", ...){
  #part_homeTZ <- interval_type <- grayzone.start <- grayzone.end <- subject_ID <- NULL
  foo <- as.data.frame(x)
  #foo <- dplyr::mutate(foo, datime_start = as.POSIXct(foo$datime_start,tz = "UTC"), datime_end = as.POSIXct(foo$datime_end,tz = "UTC"))

  foo$datime_start <- as.POSIXct(foo$datime_start, tz = "UTC")
  foo$datime_end = as.POSIXct(foo$datime_end,tz = "UTC")

  if(missing(acolor)) {acolor = c("black", "#56B4E9")} # Defining the colors

  #part <- nrow(distinct(x,x$subject_ID)) # number of participants. It defines the height of the plot
  part <- length(unique(x$subject_ID))

  p <-ggplot2::ggplot() +
    geom_segment(data = foo, aes(colour = interval_type, x = datime_start, xend = datime_end, y = subject_ID, yend = subject_ID),
                 size = 8) + { # to add the shade periods
                   #if(missing(TZ)) TZ = 0
                   #if(missing(shadow.start)) shadow.start = "22:00:00"
                   #if(missing(shadow.end)) shadow.end = "08:00:00"
                   shadow.start = "22:00:00"; shadow.end = "08:00:00"
                   if(shade == TRUE) geom_rect(data = home.night.shade(x = x, shadow.start, shadow.end, ...), aes(xmin = as.POSIXct(shadow.start, tz = "UTC"),
                                                                                   xmax = as.POSIXct(shadow.end, tz = "UTC"),
                                                                                   ymin = 0, ymax = Inf), alpha = 0.175, fill = "green") } + {
                  if(local.shade == TRUE) geom_segment(data = local.night.shade(x = x, part_homeTZ = part_homeTZ),
                                                       aes(colour = interval_type, x = as.POSIXct(grayzone.start,tz = "UTC"), #ymd_hms
                                                           xend = as.POSIXct(grayzone.end,tz = "UTC"),
                                                           y = subject_ID, yend = subject_ID), size = 12, col = "black", alpha = 0.22)} +
    theme_bw() +
    xlab("Date-Time") +
    ylab("Participant(s)") +
    theme_classic() +
    scale_color_manual(values = acolor) +
    scale_fill_manual(name = "Act")

  # resizing the plotting area
  x11() #dev.new()
  resize.win <- function(Width = 12, Height = 5){dev.off();
    dev.new(record = TRUE, width = Width, height = Height)}
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

