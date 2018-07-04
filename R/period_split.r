#' @importFrom lubridate hours days

period_split <- function(period, ...){
  period <- tbl_df(period)
  particip <- as.vector(t(distinct(period, period$actigraphy_file)))

  colName <- colnames(period)
  report <- data.frame(matrix(vector(), nrow(period), length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
  y <- 1

  for (ii in 1 : length(particip)){
    tab <- filter(period, period$actigraphy_file == particip[ii])
     if (days(tab$summary_end_datime[1] - tab$summary_start_datime[1]) > days(1) ) {
       seqdates <- seq.Date(tab$summary_start_datime[1], tab$summary_end_datime[1])
     }

  }
}
