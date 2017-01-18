
#' Generates reports based on input points.
#'
#' @param period a dataframe containing participants, start and end date/time interval
#' @param acti_data a dataframe of the form "acti_data".
#' @param tz is the time zone. Optional argument. tz = "UTC" by default.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'@details Periods partially scored as 'bad' or 'EXCLUDED' are due to off-wrist or other reasons.
#' In that case the column ‘with_excluded_bad’ will show TRUE. See examples in the Vignettes.
#'
#' @examples
#'# Example 1
#' input.param.point <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA\\input parameters example point.csv"), sep = ",", header = TRUE, skip = 0)
#' library("lubridate")
#' input.param.point$time_point_datime <- dmy_hm(input.param.point$time_point_datime,  tz = "UTC")
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' rep <- report_point(period = input.param.point, acti_data = acti_data)
#' View (rep)

#'# Example 2 with Excluded periods. Result row 4 with_excluded_bad = TRUE
#' acti_data2 <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_3")
#' q2 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA_3\\input parameters example point with Excluded.csv"),
#'  sep = ",", header = TRUE, skip = 0)
#' q2$time_point_datime <- dmy_hm(q2$time_point_datime,  tz = "UTC")
#' rep2 <- report_point(period = q2, acti_data = acti_data2)
#' View(rep2)
#'


#' @export
#' @importFrom dplyr filter mutate summarise n
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days


report_point <- function(period, acti_data, tz = "UTC",...){
period <- with_tz(period, tz = tz)
  particip <- as.vector(t(distinct(period, actigraphy_file)))

  acti_data$datime_start <- lubridate::ymd_hms(acti_data$datime_start)
  acti_data$datime_end <- lubridate::ymd_hms(acti_data$datime_end)
acti_data <- with_tz(acti_data, tz = tz)

#bypart <- dplyr::filter(acti_data, analysis_name %in% particip, interval_type %in% c("REST", "SLEEP"))
bypart <- dplyr::filter(acti_data, analysis_name %in% particip,
                        interval_type %in% c("REST", "SLEEP", "EXCLUDED", "FORCED SLEEP", "FORCED WAKE", "CUSTOM"))

  bypart <- dplyr::tbl_df(bypart)

  colName <- c("Actisoft_ID",	"period_number",	"time_since_up", "time_awake",	"last_rest_end",	"last_sleep_end",	"with_excluded_bad",
               "with_forced_wake",	"with_forced_sleep",	"with_custom_interval", "point_overlap_sleep",	"point_overlap_bed")

  report <- data.frame(matrix(vector(), nrow=1, length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
  report$last_rest_end <- as.POSIXct((report$last_rest_end))
  report$last_sleep_end <- as.POSIXct((report$last_sleep_end))

  y <- 1
  report2 = NULL
  for (ii in 1 : length(particip)){
    tab1 <- dplyr::filter(bypart, analysis_name == particip[ii])
    tab2 <- dplyr::filter(period, actigraphy_file == particip[ii])

    for (jj in 1 : nrow(tab2)){
mat0 <- dplyr::filter(tab1, datime_end <= tab2$time_point_datime[jj])
mat <- dplyr::filter(mat0, interval_type %in% c("REST", "SLEEP"))
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

      report$Actisoft_ID <- y
      report$period_number <- jj
      report$time_since_up <- difftime(tab2$time_point_datime[jj] , mat2$datime_end[1], units = "mins" )
      report$time_awake <- difftime(tab2$time_point_datime[jj] , mat2$datime_end[2], units = "mins" )
      report$last_rest_end <- mat2$datime_end[1]
      report$last_sleep_end <- mat2$datime_end[2]
      report$point_overlap_bed <- mat4$point_overlap[1]
      report$point_overlap_sleep <- mat4$point_overlap[2]
report$with_excluded_bad <- FALSE

if(sum(mat$bad, na.rm = TRUE) > 0){ # For AMI, removing sleep period is partially scored as Bad
report$with_excluded_bad <- TRUE
}

ex <- mat3[mat3$interval_type == "EXCLUDED",]
mat3p <- dplyr::filter(mat3, interval_type %in% c("REST", "SLEEP")) #,

rem = FALSE
if(nrow(ex) > 0){
  for (ll in 1 : nrow(ex)){
  for (kk in 1 : nrow(mat3p)){
    i1 <- interval(mat3p[kk,]$datime_start, mat3p[kk,]$datime_end)
    i2 <- interval(ex[ll,]$datime_start, ex[ll,]$datime_end)
    if(lubridate::int_overlaps(i1,i2) == TRUE) {rem = TRUE}
  }
  }
}

if(rem == TRUE){ # For Actiwatch, removing sleep period with Excluded
  report$with_excluded_bad <- TRUE}

report$with_forced_sleep <- ifelse(nrow(acti_data[acti_data$interval_type == "FORCED SLEEP",]) > 0, TRUE, FALSE)
report$with_forced_wake <- ifelse(nrow(acti_data[acti_data$interval_type == "FORCED WAKE",]) > 0, TRUE, FALSE)
report$with_custom_interval <- ifelse(nrow(acti_data[acti_data$interval_type == "CUSTOM",]) > 0, TRUE, FALSE)


      report2 <- rbind(report2, report)
      y <- y +  1
    }

  }

  report2 <- cbind(period, report2)
  tbl_df(report2)
}