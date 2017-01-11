#'Imports parameter periods
#'
#' @param filepath the path to the file
#' @return a dataframe
#' @export


param_period <- function(filepath){
    read.csv(file = paste(filepath), sep = ",", header = TRUE, skip = 0)
}

#fp <- 'input parameters example period.csv'
#intput.param.period <- param_period (filepath = fp)
