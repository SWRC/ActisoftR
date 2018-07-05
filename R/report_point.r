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
#'library(lubridate)
#'par_point <- data.frame(subject_ID = c(1,1),
#'                        time_point_datime = ymd_hms(c("2017-12-06 00:00:00 UTC",
#'                        "2017-12-07 00:00:00 UTC")))
#'report_point(period = par_point, acti_data = act)

#' @export
#' @importFrom dplyr filter mutate summarise n
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days with_tz


report_point <- function(period, acti_data, tz = "UTC",...){

  with_tz <- subject_ID <- interval_type <- datime_end <- po <- NULL
  period <- with_tz(period, tz = tz)
  particip <- as.vector(t(distinct(period, subject_ID)))

  acti_data$datime_start <- lubridate::ymd_hms(acti_data$datime_start)
  acti_data$datime_end   <- lubridate::ymd_hms(acti_data$datime_end)
  acti_data <- with_tz(acti_data, tz = tz)

  bypart <- dplyr::filter(acti_data, subject_ID %in% particip,
                          interval_type %in% c("REST", "SLEEP", "EXCLUDED", "FORCED SLEEP", "FORCED WAKE", "CUSTOM"))

  bypart <- dplyr::tbl_df(bypart)

  colName <- c("Actisoft_ID",	"period_number",	"time_since_up", "time_awake",	"last_rest_end",	"last_sleep_end",	"with_excluded_bad",
               "with_forced_wake",	"with_forced_sleep",	"with_custom_interval", "point_overlap_sleep",	"point_overlap_bed")

  report <- data.frame(matrix(vector(), nrow=1, length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
  report$last_rest_end <- as.POSIXct((report$last_rest_end))
  report$last_sleep_end <- as.POSIXct((report$last_sleep_end))

  y <- 1
  report1 <- report2 <- NULL
  for (ii in 1 : length(particip)){
    tab1 <- dplyr::filter(bypart, subject_ID == particip[ii])
    tab2 <- dplyr::filter(period, subject_ID == particip[ii])

    for (jj in 1 : nrow(tab2)){

      mat0 <- dplyr::filter(tab1, datime_end <= tab2$time_point_datime[jj])
      mat  <- dplyr::filter(mat0, interval_type %in% c("REST", "SLEEP"))

      tab1_sec <- portion_withoverlaps (mat0, from = tab2$time_point_datime[jj] - lubridate::hours(48) ,  to = tab2$time_point_datime[jj] )

      matex <- dplyr::filter(tab1_sec, interval_type %in% c("REST", "EXCLUDED"))
      matex[matex$interval_type == "EXCLUDED",]$duration <- difftime(matex[matex$interval_type == "EXCLUDED",]$datime_end, matex[matex$interval_type == "EXCLUDED",]$datime_start, units = "mins")

      ex <- matex[matex$interval_type == "EXCLUDED",]
      last_int <- ifelse(nrow(matex) > 0, as.character(matex[which(matex$datime_end == max(matex$datime_end)),]$interval_type), "null")

      mat <- tbl_df(mat)

      mat2 <- mat %>%
        group_by(interval_type) %>%
        summarise(datime_end  = max(datime_end)
        )


      matex2 <- matex %>%
        group_by(interval_type) %>%
        summarise(datime_end  = max(datime_end)
        )

      mat3 <- dplyr::filter(tab1, as.Date(datime_end) < as.Date(tab2$time_point_datime[jj]) + 1)
      mat3$po <- ifelse(mat3$datime_end >= tab2$time_point_datime[jj],ifelse(mat3$datime_start <= tab2$time_point_datime[jj], TRUE , FALSE) , FALSE)


      mat4 <- mat3 %>%
        group_by(interval_type) %>%
        summarise(
                  point_overlap = ifelse(any(po) == TRUE, TRUE, FALSE)
        )

      report$Actisoft_ID <- y
      report$period_number <- jj

      report$time_since_up <- ifelse(nrow(matex) > 0, difftime(tab2$time_point_datime[jj] , max(matex2$datime_end), units = "mins"), NA)
      report$time_awake <- ifelse(nrow(matex) > 0, ifelse(last_int == "REST", difftime(tab2$time_point_datime[jj] , mat2$datime_end[2], units = "mins" ), NA), NA)
      report$last_rest_end <- as.POSIXct(ifelse(nrow(matex) > 0, max(matex2$datime_end), NA), origin = "1970-01-01", tz = tz)
      report$last_sleep_end <- mat2$datime_end[2]
      report$point_overlap_bed <- mat4$point_overlap[1]
      report$point_overlap_sleep <- mat4$point_overlap[2]
      report$with_excluded_bad <- FALSE

      if(sum(tab1_sec$bad, na.rm = TRUE) > 0){
        report$with_excluded_bad <- TRUE
      }


      mat3p <- dplyr::filter(mat3, interval_type %in% c("REST", "SLEEP")) #,

      rem = FALSE
      if(last_int == "EXCLUDED"){
        rem = TRUE
      }

      if(rem == TRUE){ # For Actiwatch, removing sleep period with Excluded
        report$with_excluded_bad <- TRUE}

      report$with_forced_sleep <- ifelse(nrow(tab1_sec[tab1_sec$interval_type == "FORCED SLEEP",]) > 0, TRUE, FALSE)
      report$with_forced_wake <- ifelse(nrow(tab1_sec[tab1_sec$interval_type == "FORCED WAKE",]) > 0, TRUE, FALSE)
      report$with_custom_interval <- ifelse(nrow(tab1_sec[tab1_sec$interval_type == "CUSTOM",]) > 0, TRUE, FALSE)

      report1 <- cbind(tab2[jj,], report)
      report2 <- rbind(report1, report2)
      y <- y +  1

      }

  }

  tbl_df(report2)
}
