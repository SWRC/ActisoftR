#' #Slices a data.frame containing date-time type columns.
#' @param x at dataframe
#' @param from a vector of lenght = participants
#' @param to a vector of lenght = participants
#' @param ... Optional parameters
#' @return a dataframe
#'
#' @details it does not include overlaps periods: periods that started (ended) before (after) start.period (end.period)
#'
#' @examples
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' start.period <- rep("2016-07-10", 3)
#' end.period <- rep("2016-07-20", 3)
#' mystudy <- portion(acti_data, from = start.period,  to = end.period)
#'
#' @export
#' @importFrom lubridate interval int_overlaps
#' @importFrom dplyr tbl_df filter
#'
portion <- function(x, from , to , ...){ #all = TRUE,
  int <- NULL
  x <- tbl_df(x)
  part <- length(unique(x$subject_ID))
  mat2 <-  NULL

  if(length(from) != length(to))
    stop("the variables from and to must have same length ")

  for (i in 1 : length(part)){
    mat <- filter(x, x$subject_ID == part[[i]], x$datime_start >= from[i], x$datime_end <= to[i] )
    mat2 <- rbind(mat2 , mat)
  }
  mat2
}




#' Slices a data.frame containing date-time type columns including overlaping periods.
#'
#' @param x a dataframe
#' @param from a vector of lenght = participants
#' @param to a vector of lenght = participants
#' @param tz is the time zone
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#' @details it does include overlaps periods: periods that started (ended) before (after) start.period (end.period)
#'
#' @examples
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' start.period <- rep("2016-07-10 12:00", 3)
#' end.period <- rep("2016-07-20 12:00", 3)
#' mystudy <- portion_withoverlaps(acti_data, from = start.period,  to = end.period)
#' @export
#' @importFrom lubridate interval int_overlaps
#' @importFrom dplyr select

# Slice a data.frame with overlaps
portion_withoverlaps <- function(x, from , to , tz = "UTC", ...){
  int <- NULL
  x <- tbl_df(x)
  x$int <- lubridate::interval(x$datime_start,x$datime_end)
  part <- as.vector(unique(x$subject_ID))
  mat2 <-  NULL

  if(length(from) != length(to))
    stop("the variables from and to must have same length ")

  for (i in 1 : length(part)){
    int2 <- lubridate::interval(from[i], to[i], tz = tz)

    mat <- dplyr::filter(x, x$subject_ID == part[[i]], lubridate::int_overlaps(x$int,int2) %in% c(TRUE, NA) )
    mat <- mat %>% select(-int)
    mat2 <- rbind(mat2 , mat)
  }
  mat2
}

