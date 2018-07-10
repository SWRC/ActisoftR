
#' Imports a CSV file and attaches the file path as a column.
#'
#' @param filename file path
#'
#' @return a dataframe
#'
#' @examples
#'  \dontrun{read_csv_filename("C:\\1\\EXAMPLE_DATA\\Actiware\\example1.csv")}
#' @export

read_csv_filename <- function(filename){
  mycsv <- read.csv(filename, sep = ",", header = TRUE, skip = 0, na.strings = c("NA", "NaN", ".", "*", ""), fileEncoding = "UTF-8-BOM")
  mycsv$file_name <- filename
  mycsv
}
