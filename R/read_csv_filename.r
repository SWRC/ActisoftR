
#' Imports a CSV file and attaches the file path as a column.
#'
#' @param filename file path
#'
#' @return a dataframe
#'
#' @examples
#' read_csv_filename("C:\\1\\EXAMPLE_DATA\\Actiware\\example1.csv")
#' @export

read_csv_filename <- function(filename){
 # mycsv <- read.csv(filename, sep = ",", header = TRUE, skip = 0)
  mycsv <- read.csv(filename, sep = ",", header = TRUE, skip = 0, fileEncoding="UTF-8-BOM")
  mycsv$file_name <- filename
  mycsv
}


#####

# Importing flight Sleep-Work data type (Not real data)
# this data is from the excel sheet Example01 example02 Darwent Plot v3-1.xls

#flight <- read.csv(file = 'EXAMPLE_DATA\\Work\\work.csv', sep = ",", header = TRUE, skip = 0)
#flight$datime_start <- paste( as.POSIXct( strptime( flight$StartDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))
#flight$datime_end <- paste( as.POSIXct( strptime( flight$EndDatime, format = "%d/%m/%Y %H:%M"), tz = "UTC"))

#flight <- flight[,-c(3,4)]
#colnames(flight) <- c("subject_ID", "startTZ", "endTZ", "interval_type", "datime_start", "datime_end") # using unique variables system

#flight <- tbl_df(flight) # table dataframe
