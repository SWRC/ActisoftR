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
#' data(act)
#' # Prob of being sleeping for ID 1 during the baseline sleep
#' b <- act[act$subject_ID==1 & act$datime_end < as.POSIXct("2017-12-15 00:00:00", tz = "UTC"),]
#' sheep_counter(b)

#'



#' @export
#' @importFrom dplyr distinct left_join
#' @importFrom scales date_breaks date_format
#' @importFrom methods hasArg
#' @importFrom lubridate days
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
      }
    }

    tab32 <- tab22 / yy
    tab32 <- data.frame(int=int[1 : length(tab32)],freq=tab32, interval_type = "WORK")

    tab32$int <-  as.POSIXct(paste(strftime(tab3$int, format="%Y-%m-%d", tz = tz), strftime(tab32$int, format="%H:%M:%S", tz = tz)), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    p <- p + geom_line(data = tab32, aes(colour = interval_type, x = int,  y = freq))

  }


  p + scale_x_datetime(breaks = date_breaks(datebreaks),
                       minor_breaks = date_breaks(datebreaks),
                       labels = date_format("%H.%M", tz = "UTC")) +
    xlab("Time (UTC) ") +
    ylab("proportion") + theme_bw()

}
