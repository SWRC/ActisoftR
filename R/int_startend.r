
#' Returns the starting/ending point of a report
#'
#' @param x a dataframe
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
int_startend <- function(x, ...){
  subject_ID <- summary_start_datime <- summary_end_datime <- NULL
# starting datime
  use <- c("subject_ID", "summary_start_datime", "summary_end_datime")
  use2 <- c("summary_start_datime", "summary_end_datime")
  y <- x[, use]
mini <- as.POSIXct(apply(y[,use2], 1, min))#lubridate::ymd_hms(apply(y[,use2], 1, min))
maxi <- as.POSIXct(apply(y[,use2], 1, max))#lubridate::ymd_hms(apply(y[,use2], 1, max))
y$summary_start_datime <- mini
y$summary_end_datime <- maxi

  from <- y  %>% group_by(subject_ID) %>% slice(which.min(summary_start_datime))
  from <- select_(from, .dots = c("subject_ID", "summary_start_datime"))
  # ending datime
  to <- y %>% group_by(subject_ID) %>% slice(which.max(summary_end_datime))
  to <- select_(to, .dots = c("subject_ID", "summary_end_datime"))
  from$summary_end_datime <-  to$summary_end_datime
  from
#int_startend(period)
}
