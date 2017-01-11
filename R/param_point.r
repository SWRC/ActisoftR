#' Input reports based on points.
#'
#' @param filepath the path to the file
#' @return a dataframe
#'
#' @examples
#' #fp2 <- 'input parameters example point.csv'
#' #input.param.point <- param_point (filepath = fp2)
#' #input.param.point$time_point_datime <- dmy_hm(input.param.point$time_point_datime,  tz = "UTC")
#' #input.param.point
#' @export

param_point <- function(filepath){
  read.csv(file = paste(filepath), sep = ",", header = TRUE, skip = 0)
}



