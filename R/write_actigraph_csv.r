#' Exports data as CSV.
#'
#' @param x the file path
#' @param path is the path for saving the file
#' @param ... Optional parameters
#' @return a dataframe
#' @export
#' @importFrom dplyr tbl_df filter mutate select_ rename group_by
#' @importFrom data.table rbindlist setnames setattr
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#' @importFrom utils read.csv write.csv
#'
#' @examples
#' x <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' write_actigraph_csv(x, path = "C:\\1\\data_Darwent.csv")
#'

write_actigraph_csv <- function(x, path = "~",...){
  useful <- c("analysis_name","subject_ID", "datime_start", "datime_end", "interval_type")
  x <- x[, useful]
  write.csv(x, path)
}

