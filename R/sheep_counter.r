#' Sheep counter
#'
#' Computes the empirical probability that a group or an individual are sleeping at a given time of the day.
#' @param dat a data frame.
#' @param tz the time zone.
#' @param interv interval of time, set as 10min.
#' @param datebreaks is the distance between breaks in the x-axis. "2 hour" by default.
#' @param work_data optional parameter used to import work data.
#' @return a plot.
#'
#' @examples
#'# Example 1:
#' dat <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_6")
#' sheep_counter(dat)
#'
#'# Example 2: using work data (randomly generated)
#' datime_start <- rep( seq(ISOdatetime(2012,05,12,8,00,00, tz = "UTC"),(ISOdatetime(2012,05,12,8,00,00, tz = "UTC") + days(3)), length.out = 4), length(unique(dat$subject_ID)) )
#' datime_end <- rep( seq(ISOdatetime(2012,05,12,16,00,00, tz = "UTC"),(ISOdatetime(2012,05,12,16,00,00, tz = "UTC") + days(3)), length.out = 4), length(unique(dat$subject_ID)) )
#' df <- data.frame(datime_start = datime_start, datime_end = datime_end, subject_ID = rep(unique(dat$subject_ID),4))
#' dif1 <- rnorm(nrow(df), 1,1)
#' dif2 <- rnorm(nrow(df), 1,1)
#' df$datime_start <- df$datime_start + (3600*dif)
#' df$datime_end <- df$datime_end + (3600*dif2)
#' sheep_counter(dat, work_data = df)
#'
#'# Example 3:
#' dat2 <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_7")
#' sheep_counter(dat2)





#' @export
#' @importFrom grDevices dev.new dev.off x11 windows
#' @importFrom dplyr distinct left_join
#' @importFrom scales date_breaks date_format
#' @importFrom methods hasArg
#' @import ggplot2


sheep_counter <- function(dat, tz = "UTC", interv = "10 mins", datebreaks = "2 hour", work_data ){
  tab <- freq <- prop <- interval_type <- NULL
  tab2 <- rep(0, 60 * 24 / as.numeric(gsub("([0-9]*).*","\\1",interv)) )

  dat$datime_start <- as.POSIXct(dat$datime_start , format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  dat$datime_end <- as.POSIXct(dat$datime_end , format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  dat <- dat[!is.na(dat$datime_start ),]

  dat2 <- dat[dat$interval_type == "SLEEP",]
  particip <- unique(dat2$subject_ID)

  y <- 0
  for (i in 1 : length(particip)){
    dat3 <- dat2[dat2$subject_ID==particip[i],]

    for (j in 1 : nrow(dat3)){
      y <- y + 1
      row <- dat3[j,]
      row$datime_start <- as.POSIXct(row$datime_start, format = "%Y-%m-%d %H:%M:%S", tz = tz)

      int <- seq.POSIXt(strptime(row$datime_start, format="%Y-%m-%d", tz = tz), strptime(row$datime_start, format="%Y-%m-%d", tz = tz) + hours(24) - as.numeric(gsub("([0-9]*).*","\\1",interv)), interv)
      int1 <- strftime(int, format="%H:%M:%S", tz = tz)
      int12 <- as.POSIXct(paste("1970-01-01", int1), format = "%Y-%m-%d %H:%M:%S", tz = tz)

      if(row$datime_start<row$datime_end) {int2 <- seq.POSIXt((row$datime_start), (row$datime_end), interv)}
      else {int2 <- NA}

      int22 <- strftime(int2, format="%H:%M:%S", tz = tz)
      int3 <- as.POSIXct(paste("1970-01-01", int22), format = "%Y-%m-%d %H:%M:%S", tz = tz)

      tab<- table(cut(c(int12, int3), breaks = interv))
      tab <- tab - 1
      tab <- as.data.frame(tab)

      tab2 <- as.vector(tab$Freq) + tab2

    }
  }

  tab3 <- tab2 / y #prop.table(tab2)
  tab3 <- data.frame(int=int[1 : length(tab3)],freq=tab3, interval_type = "SLEEP")


  p <-ggplot2::ggplot() +
    geom_line(data = tab3, aes(colour = interval_type, x = int,  y = freq))


  if (hasArg(work_data) == TRUE){
    tab22 <- rep(0, 60 * 24 / as.numeric(gsub("([0-9]*).*","\\1",interv)) )

    dat2 <- work_data

    yy <- 0
    for (ii in 1 : length(particip)){
      dat3 <- dat2[dat2$subject_ID==particip[ii],]

      for (jj in 1 : nrow(dat3)){
        yy <- yy + 1
        row <- dat3[jj,]
        row$datime_start <- as.POSIXct(row$datime_start, format = "%Y-%m-%d %H:%M:%S", tz = tz)

        int <- seq.POSIXt(strptime(row$datime_start, format="%Y-%m-%d", tz = tz), strptime(row$datime_start, format="%Y-%m-%d", tz = tz) + hours(24) - as.numeric(gsub("([0-9]*).*","\\1",interv)), interv)
        int1 <- strftime(int, format="%H:%M:%S", tz = tz)
        int12 <- as.POSIXct(paste("1970-01-01", int1), format = "%Y-%m-%d %H:%M:%S", tz = tz)

        if(row$datime_start<row$datime_end) {int2 <- seq.POSIXt((row$datime_start), (row$datime_end), interv)}
        else {int2 <- NA}

        int22 <- strftime(int2, format="%H:%M:%S", tz = tz)
        int3 <- as.POSIXct(paste("1970-01-01", int22), format = "%Y-%m-%d %H:%M:%S", tz = tz)

        tab<- table(cut(c(int12, int3), breaks = interv))
        tab <- tab - 1
        tab <- as.data.frame(tab)

        tab22 <- as.vector(tab$Freq) + tab22
        #print((tab22))

      }
    }

    tab32 <- tab22 / yy #prop.table(tab2)
    tab32 <- data.frame(int=int[1 : length(tab32)],freq=tab32, interval_type = "WORK")

    tab32$int <-  as.POSIXct(paste(strftime(tab3$int, format="%Y-%m-%d", tz = tz), strftime(tab32$int, format="%H:%M:%S", tz = tz)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  p <- p + geom_line(data = tab32, aes(colour = interval_type, x = int,  y = freq))
    #geom_line(data = work, aes(colour = "WORK", x = int,  y = prop))


  }



  x11()
  resize.win <- function(Width = 12, Height = 5){dev.off();
    dev.new(record = TRUE, width = Width, height = Height)}
  resize.win(8, 6)

  p + scale_x_datetime(breaks = date_breaks(datebreaks),
                       minor_breaks = date_breaks(datebreaks),
                       labels = date_format("%H.%M", tz = "UTC")) +
    xlab("Time (UTC) ") +
    ylab("proportion")

}
