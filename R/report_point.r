
#' Generates reports based on input points.
#'
#' @param period a dataframe containing participants, start and end date/time interval
#' @param acti_data a dataframe of the form "acti_data".
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#' @examples

#' input.param.point <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA\\input parameters example point.csv"), sep = ",", header = TRUE, skip = 0)
#' library("lubridate")
#' input.param.point$time_point_datime <- dmy_hm(input.param.point$time_point_datime,  tz = "UTC")
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' rep3 <- report_point(period = input.param.point, acti_data = acti_data)
#' View (rep3)

#' @export
#' @importFrom dplyr filter mutate summarise n
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days



report_point <- function(period, acti_data, ...){
  particip <- as.vector(t(distinct(period, actigraphy_file)))

  acti_data$datime_start <- lubridate::ymd_hms(acti_data$datime_start)
  acti_data$datime_end <- lubridate::ymd_hms(acti_data$datime_end)

  bypart <- dplyr::filter(acti_data, analysis_name %in% particip, interval_type %in% c("REST", "SLEEP"))
  bypart <- dplyr::tbl_df(bypart)

  colName <- c("Actisoft_ID",	"period_number",	"time_since_up", "time_awake",	"last_rest_end",	"last_sleep_end",	"with_excluded_bad",
               "with_forced_wake",	"with_forced_sleep",	"with_custom_interval", "point_overlap_sleep",	"point_overlap_bed")

  report = data.frame(matrix(vector(), nrow(period), length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)

  report$last_rest_end <- as.POSIXct((report$last_rest_end))
  report$last_sleep_end <- as.POSIXct((report$last_sleep_end))

  y <- 1
  for (ii in 1 : length(particip)){
    tab1 <- dplyr::filter(bypart, analysis_name == particip[ii])
    tab2 <- dplyr::filter(period, actigraphy_file == particip[ii])

    for (jj in 1 : nrow(tab2)){
      mat <- dplyr::filter(tab1, datime_end <= tab2$time_point_datime[jj])
      mat <- tbl_df(mat)

      mat2 <- mat %>%
        group_by(interval_type) %>%
        summarise(datime_end  = max(datime_end)
        )

      mat3 <- dplyr::filter(tab1, as.Date(datime_end) == as.Date(tab2$time_point_datime[jj]))
      mat4 <- mat3 %>%
        group_by(interval_type) %>%
        summarise(datime_end  = max(datime_end),
                  point_overlap = ifelse(datime_end >= tab2$time_point_datime[jj], TRUE, FALSE)
        )

      report$Actisoft_ID[y] <- y
      report$period_number[y] <- jj
      report$time_since_up[y] <- difftime(tab2$time_point_datime[jj] , mat2$datime_end[1], units = "mins" )
      report$time_awake[y] <- difftime(tab2$time_point_datime[jj] , mat2$datime_end[2], units = "mins" )
      report$last_rest_end[y] <- ymd_hms( mat2$datime_end[1])
      report$last_sleep_end[y] <- mat2$datime_end[2]
      report$point_overlap_bed[y] <- mat4$point_overlap[1]
      report$point_overlap_sleep[y] <- mat4$point_overlap[2]

      y <- y +  1
    }

  }

  report2 <- cbind(period, report)
  tbl_df(report2)
}
