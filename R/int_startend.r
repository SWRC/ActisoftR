
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
#'
int_startend <- function(x, ...){

# starting datime
  from <- x  %>% group_by(actigraphy_file) %>% slice(which.min(summary_start_datime))
  from <- select_(from, .dots = c("actigraphy_file", "summary_start_datime"))
  # ending datime
  to <- x %>% group_by(actigraphy_file) %>% slice(which.max(summary_end_datime))
  to <- select_(to, .dots = c("actigraphy_file", "summary_end_datime"))
  from$summary_end_datime <-  to$summary_end_datime
  from
#int_startend(period)
}

