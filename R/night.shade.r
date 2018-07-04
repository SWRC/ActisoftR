#' Generates home night time periods
#'
#' This function is used by the Darwent plot
#'
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param TZ participant's home time zone. A data frame containing for each participant
#' corresponding home time zone.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'
#' @examples
#' #flight <- read.csv(file = 'C:\\1\\EXAMPLE_DATA\\Work\\work.csv', sep = ",",header = TRUE, skip = 0)
#' #flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' #flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' #flight <- flight[,-c(3,4)]
#' #colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type","datime_start", "datime_end")
#' #homeTZ = data.frame(subject_ID = c("example01", "example01_AMI", "example02"), TZ = c(10,11,12))
#' #home.night.shade(x = flight, shadow.start = "22:00:00", shadow.end = "08:00:00", homeTZ = homeTZ, tz = "UTC")

#' #home.night.shade(x = flight, shadow.start = "12:00:00", shadow.end = "22:00:00", homeTZ = homeTZ , tz = "UTC")


#' @export
#' @importFrom lubridate hours hms
#' @importFrom dplyr distinct if_else "%>%"
#' @importFrom tidyr unnest
#' @importFrom magrittr "%>%"
#
# used by the Darwent plot
home.night.shade <- function(x, shadow.start = "20:00:00", shadow.end = "06:00:00", homeTZ , tz = "UTC",...){
  shadow.start <- hms(shadow.start)
  shadow.end <- hms(shadow.end)
  nightdur <- if_else(shadow.start > shadow.end, shadow.end + hours(24) - shadow.start, shadow.end - shadow.start)

  first.last <- x %>% dplyr::group_by(subject_ID) %>%
    summarise(min_date = as.Date(min(datime_start, na.rm = TRUE), tz = tz),
              max_date = as.Date(max(datime_start, na.rm = TRUE), tz = tz))

  first.last <- first.last %>%
    left_join(homeTZ, by = c("subject_ID" = "subject_ID")) %>%
    group_by(subject_ID) %>%
    mutate(date = list(seq(min_date, max_date, by ='1 day') ), created_at = NULL) %>%
    unnest()

  shadow.startend <- data.frame(subject_ID = first.last$subject_ID, datime_start = ymd_hms(paste(first.last$date, shadow.start), tz = tz) + lubridate::hours(first.last$TZ),
                                datime_end = ymd_hms(paste(first.last$date, shadow.start), tz = tz) + lubridate::hours(first.last$TZ) + nightdur)

  shadow.startend
}





#' Generates local night time periods
#'
#' This function is used by the Darwent plot
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param localTZ a dataframe containing the local time zone by participant during the days worked.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'
#' @examples
#' #flight <- read.csv(file = 'C:\\1\\EXAMPLE_DATA\\Work\\work.csv', sep = ",",header = TRUE, skip = 0)
#' #flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' #flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#' #flight <- flight[,-c(3,4)]
#' #colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type","datime_start", "datime_end")
#' # create a data.frame with the worked days and the tz per ID.
#' #localTZ <- filter(flight, interval_type == "Work") %>% select(subject_ID, datime_start, datime_end, startTZ)
#' #names(localTZ)[4] <- "TZ"
#' #local.night.shade(localTZ = localTZ, shadow.start = "22:00:00", shadow.end = "08:00:00")

#' @export
#' @importFrom dplyr distinct filter tbl_df if_else "%>%"
#' @importFrom lubridate hours hms
#' @importFrom magrittr "%>%"
#'
local.night.shade <- function(localTZ, shadow.start = "20:00:00",
                              shadow.end = "06:00:00", tz = "UTC",...){

  shadow.start <- hms(shadow.start)
  shadow.end <- hms(shadow.end)
  nightdur <- if_else(shadow.start > shadow.end, shadow.end + hours(24) - shadow.start, shadow.end - shadow.start)

  shadow.startend <- data.frame(subject_ID = localTZ$subject_ID, datime_start = ymd_hms(paste(as.Date(localTZ$datime_end), shadow.start)) + hours(localTZ$TZ),
                                datime_end = ymd_hms(paste(as.Date(localTZ$datime_end), shadow.start)) + hours(localTZ$TZ) + nightdur)

  shadow.startend
}

