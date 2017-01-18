#' ActisoftR: Parsing actigraphy data
#'
#' A toolbox for importing and processing actigraph like data. It allows plotting and generating multiple reports.
#'
#'
#'
#' @docType package
#' @name ActisoftR
#' @useDynLib ActisoftR
#' @importFrom dplyr tbl_df filter mutate select_ rename group_by slice
#' @importFrom data.table rbindlist
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @importFrom lubridate hours days
#' @importFrom grDevices dev.new dev.off windows
#' @importFrom stats setNames update
#' @importFrom scales date_breaks date_format

