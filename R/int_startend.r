

#' Returns the starting/ending point of a report
#'
#' @param x a dataframe
#' @param tz is the time zone
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'
#' @export
#' @importFrom dplyr group_by slice select_
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms
#'
int_startend <- function(x, tz = "UTC", ...){
  subject_ID <- summary_start_datime <- summary_end_datime <- NULL
  use <- c("subject_ID", "summary_start_datime", "summary_end_datime")
  use2 <- c("summary_start_datime", "summary_end_datime")
  y <- x[, use]
mini <- as.POSIXct(apply(y[,use2], 1, min), tz = tz)
maxi <- as.POSIXct(apply(y[,use2], 1, max), tz = tz)
y$summary_start_datime <- mini
y$summary_end_datime <- maxi

  from <- y  %>% group_by(subject_ID) %>% slice(which.min(summary_start_datime))
  from <- select_(from, .dots = c("subject_ID", "summary_start_datime"))
  to <- y %>% group_by(subject_ID) %>% slice(which.max(summary_end_datime))
  to <- select_(to, .dots = c("subject_ID", "summary_end_datime"))
  from$summary_end_datime <-  to$summary_end_datime
  from
}
