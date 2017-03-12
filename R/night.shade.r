
#' Generates home night time periods
#'
#' This function is used by the Darwent plot
#'
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param TZ participant's home time zone
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'
#' @examples
#'#  home.night.shade(x = flight, shadow.start = "22:00:00",shadow.end = "08:00:00")

#' @export
#' @importFrom lubridate hours
#' @importFrom dplyr distinct
#
# used by the Darwent plot
home.night.shade <- function(x, shadow.start = "22:00:00", shadow.end = "08:00:00", TZ = 0, tz = "UTC",...){
  first.last <- c(min(x$datime_start), max(x$datime_end))
  first.last.date <- as.Date(first.last)
  seqdates <- seq.Date(first.last.date[1], first.last.date[2],1)
  if(shadow.start < shadow.end) {seqdates2 <- seqdates}
  else { seqdates2 <- seqdates + 1}
  shadow.startend <- data.frame(shadow.start = as.POSIXct(paste(seqdates, shadow.start), format="%Y-%m-%d %H:%M:%S", tz = tz) + lubridate::hours(TZ),
                                shadow.end = as.POSIXct(paste(seqdates2 , shadow.end), format="%Y-%m-%d %H:%M:%S", tz = tz) + lubridate::hours(TZ))
  shadow.startend
}






#' Generates local night time periods
#'
#' This function is used by the Darwent plot
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param part_localTZ a dataframe containing the local time zone by participant.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'
#' @examples
#' flight <- read.csv(file = 'C:\\1\\EXAMPLE_DATA\\Work\\work.csv', sep = ",",header = TRUE, skip = 0)
#' flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' flight <- flight[,-c(3,4)]
#' colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type","datime_start", "datime_end")
#' part_localTZ <- data.frame (subject_ID = c("example01", "example01_AMI", "example02"), homeTZ = rep(2,3))
#' sh <- local.night.shade(x = flight, part_localTZ = part_localTZ, shadow.start = "22:00:00", shadow.end = "08:00:00")

#' @export
#' @importFrom dplyr distinct filter tbl_df
#'
local.night.shade <- function(x, part_localTZ, shadow.start = "20:00:00",
                              shadow.end = "06:00:00", tz = "UTC",...){ #

  y <- x[which(!is.na(x$startTZ)),]
  particip <- as.vector(t(unique(x$subject_ID))) #as.vector(t(distinct(y, y$subject_ID)))
  if(missing(part_localTZ)){part_localTZ <- data.frame (subject_ID = particip, homeTZ = rep(0,length(particip)))}
  u <- NULL
  for (ii in 1 : length(particip)){
      v <- part_localTZ[part_localTZ$subject_ID == particip[ii],]
      z <- dplyr::filter(y, y$subject_ID == particip[ii], y$startTZ != v$homeTZ | y$endTZ != v$homeTZ )
      z$grayzone.start <- lubridate::ymd_hms(paste(round.POSIXt(z$datime_start, units = "days" ), lubridate::hms(shadow.start) )) - lubridate::hours(z$startTZ)
      z$grayzone.end <- lubridate::ymd_hms(paste(round.POSIXt(z$datime_end, units = "days"), lubridate::hms(shadow.end) )) - lubridate::hours(z$endTZ)
      u <- rbind(u , z)
  }
u
}




