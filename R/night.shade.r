
#' Generates home night time periods
#'
#' This function is used by the Darwent plot
#'
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param homeTZ participant's home time zone. A data frame containing for each participant
#' corresponding home time zone.
#' @param ... Optional parameters
#' @param tz is the time zone
#'
#' @return a dataframe
#'
#'
#' @examples
#' library("dplyr")
#' library("lubridate")
#' data(act)
#' dat <- act %>% group_by(subject_ID) %>%
#' mutate(start = min(datime_start)) %>%
#' ungroup %>% filter(datime_start <= start + lubridate::days(5),
#' subject_ID == 1, interval_type != "ACTIVE")
#' homeTZ = data.frame(subject_ID = "1",
#' TZ = "Pacific/Auckland", stringsAsFactors = FALSE)
#' home.night.shade(dat, shadow.start = "20:00:00", shadow.end = "06:00:00",
#' homeTZ = homeTZ)

#' @export
#' @importFrom lubridate hours hms
#' @importFrom dplyr distinct if_else rowwise "%>%"
#' @importFrom tidyr unnest
#' @importFrom magrittr "%>%"
#' @rdname home.night.shade
#
# used by the Darwent plot
home.night.shade <- function(x, shadow.start = "20:00:00", shadow.end = "06:00:00", homeTZ , tz = "UTC",...){
  min_date <- max_date <- subject_ID <- subject_ID <- datime_start <- TZ <- NULL
  shadow.start <- hms(shadow.start)
  shadow.end <- hms(shadow.end)
  nightdur <- if_else(shadow.start > shadow.end, shadow.end + hours(24) - shadow.start, shadow.end - shadow.start)

  x$subject_ID <- factor(as.character(x$subject_ID))

  if(missing(homeTZ)) {
    homeTZ <- data.frame(subject_ID = unique(x$subject_ID), TZ = rep("UTC", length(unique(x$subject_ID))), stringsAsFactors = FALSE)
  }
  first.last <- x %>% dplyr::group_by(subject_ID) %>%
    summarise(min_date = as.Date(min(datime_start, na.rm = TRUE), tz = tz),
              max_date = as.Date(max(datime_start, na.rm = TRUE), tz = tz))

  homeTZ$subject_ID <- factor(as.character(homeTZ$subject_ID))
  first.last <- first.last %>%
    dplyr::left_join(homeTZ, by = c("subject_ID" = "subject_ID")) %>%
    dplyr::group_by(subject_ID) %>%
    dplyr::mutate(date = list(data.frame(date = seq(min_date, max_date, by ='1 day') )), created_at = NULL) %>%
    tidyr::unnest()

  first.last <- first.last %>% dplyr::rowwise() %>%
    dplyr::mutate(datime_start =  ymd_hms(paste(date, shadow.start), tz = TZ), TZ) %>%
    dplyr::mutate(datime_end =  ymd_hms(paste(date, shadow.start), tz = TZ) + nightdur)

  first.last <- data.frame(first.last)
  first.last
}





#' Generates local night time periods
#'
#' This function is used by the Darwent plot
#' @param x a dataframe
#' @param shadow.start starting time of the home night, 22:00:00 by defaults
#' @param shadow.end ending time of the home night, 08:00:00 by defaults
#' @param localTZ a dataframe containing the local time zone by participant during the days worked.
#' @param tz the time zone
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#' @examples
#' library("dplyr")
#' library("lubridate")
#' data(act)
#' # Selecting the date/time arrival to Amsterdam for subject_ID 1
#' start_date <- act[act$subject_ID==1 & act$interval_type == "FLIGHT" &
#' act$comments == "SIN-AMS",]$datime_end
#' sel <- act[act$datime_start >= start_date & act$datime_end <= start_date + lubridate::days(10),]
#' localTZ <- data.frame(subject_ID = "1",
#'                       TZ = "Europe/Amsterdam", stringsAsFactors = FALSE)
#' local.night.shade(sel, shadow.start = "20:00:00",
#'                   shadow.end = "06:00:00", localTZ = localTZ)
#'
#'
#' @export
#' @importFrom lubridate hours hms
#' @importFrom dplyr distinct if_else rowwise "%>%"
#' @importFrom tidyr unnest
#' @importFrom magrittr "%>%"
#' @rdname local.night.shade
#'
local.night.shade <- function(x, shadow.start = "20:00:00",
                              shadow.end = "06:00:00", localTZ,  tz = "UTC", ...){
  min_date <- max_date <- subject_ID <- subject_ID <- datime_start <- TZ <- NULL
  shadow.start <- hms(shadow.start)
  shadow.end <- hms(shadow.end)
  nightdur <- if_else(shadow.start > shadow.end, shadow.end + hours(24) - shadow.start, shadow.end - shadow.start)

  x$subject_ID <- factor(as.character(x$subject_ID))

  if(missing(localTZ)) {
    localTZ <- data.frame(subject_ID = unique(x$subject_ID), TZ = rep("UTC", length(unique(x$subject_ID))), stringsAsFactors = FALSE)
  }
  first.last <- x %>% dplyr::group_by(subject_ID) %>%
    summarise(min_date = as.Date(min(datime_start, na.rm = TRUE), tz = tz),
              max_date = as.Date(max(datime_start, na.rm = TRUE), tz = tz))

  localTZ$subject_ID <- factor(as.character(localTZ$subject_ID))
  first.last <- first.last %>%
    dplyr::left_join(localTZ, by = c("subject_ID" = "subject_ID")) %>%
    dplyr::group_by(subject_ID) %>%
    dplyr::mutate(date = list(data.frame(date = seq(min_date, max_date, by ='1 day') )), created_at = NULL) %>%
    tidyr::unnest()

  first.last <- first.last %>% dplyr::rowwise() %>%
    dplyr::mutate(datime_start =  ymd_hms(paste(date, shadow.start), tz = TZ), TZ) %>%
    dplyr::mutate(datime_end =  ymd_hms(paste(date, shadow.start), tz = TZ) + nightdur)

  first.last <- data.frame(first.last)
  first.last

}

