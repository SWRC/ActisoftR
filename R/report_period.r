#' Generates reports based on input periods.
#'
#'
#' @param period a dataframe containing participants and the start and end date/time periods.
#' @param acti_data a dataframe of the form 'acti_data'.
#' @param remove_bad a logical value used to indicate if 'bad' intervals or 'EXCLUDED' are removed. Set as TRUE.
#' @param tz is the time zone. Optional argument. tz = "UTC" by default.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'@details Periods partially scored as 'bad' or 'EXCLUDED' are due to off-wrist or other reasons.
#' In that case all summary estimates except ‘time in bed’ will be set to NA,
#' and the column ‘with_excluded_bad’ will show a TRUE. See examples in the Vignettes.
#' This can be avoided using remove_bad = FALSE.
#' Sleep efficiency will be zero when there is an attempt to sleep (no sleep achieved in the period) within a rest.
#' However, the sleep efficiency will be NA when there is not REST or SLEEP with in an interval of time.
#'
#' @examples
#'# Example 1
#' library("lubridate")
#' data("act")
#' par <- data.frame(subject_ID = 1,
#' summary_duration_h = 24,
#' summary_type = "sequential",
#' summary_start_datime = ymd_hms("2017-12-05 00:00:00 UTC"),
#' summary_end_datime = ymd_hms("2017-12-15 00:00:00 UTC"))
#' report_period(period = par , acti_data = act)

#' @export
#' @importFrom dplyr filter mutate summarise n
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days with_tz

report_period <- function(period, acti_data, remove_bad = TRUE, tz = "UTC", ...){
  with_tz <- summary_type <- subject_ID <- duration_adj <- sleep_time_adj <- interval_number <- NULL


  period <- with_tz(period, tz = tz)
  period <- tbl_df(period)
  actigraphy_file <- analysis_name <- interval_type <- exact3 <- duration <- sleep_time <- efficiency <- art <- NULL
  pos_sum_types = c("first", "daily", "sequential", "time_to_time") # possible summary types

  if (sum( match(x = pos_sum_types, table = period$summary_type), na.rm = TRUE ) == 0){
    stop("summary_type incorrectly defined. It must be set to: 'first', 'daily', 'sequential' or 'time_to_time'")}

  period <- deagg(period)
  sum_types <- as.vector(t(distinct(period, summary_type))) # actual summary types
  startend <- tbl_df(int_startend(period))
  particip <- as.vector(t(distinct(period, subject_ID)))

  acti_data$datime_start <- ymd_hms(acti_data$datime_start)
  acti_data$datime_end <- ymd_hms(acti_data$datime_end)
  acti_data <- with_tz(acti_data, tz = tz)

  acti_data$duration <- as.numeric(as.character(acti_data$duration))
  acti_data$sleep_time <- as.numeric(as.character(acti_data$sleep_time))

  acti_data <- tbl_df(dplyr::filter(acti_data, subject_ID %in% particip, interval_type %in% c("REST", "SLEEP", "EXCLUDED", "FORCED SLEEP", "FORCED WAKE", "CUSTOM")))

  colName <- c("Actisoft_ID",	"period_number",
               "report_duration_m",	"number_of_rests",	"number_of_sleeps",	"number_of_rests_exact", "number_of_sleeps_exact",	"total_time_in_bed",
               "total_sleep",	"sleep_efficiency",	"longest_sleep_period",	"shortest_sleep_period",	"with_forced_wake",	"with_forced_sleep",
               "with_custom_interval",	"with_excluded_bad")

  report2 <- NULL
  y <- 1
  for (ii in 1 : length(particip)){

    tab1 <- dplyr::filter(acti_data, subject_ID == particip[ii])
    tab2 <- dplyr::filter(startend, subject_ID == particip[ii])
    tab1_sec <- portion_withoverlaps (tab1, from = as.POSIXct(tab2$summary_start_datime, tz = tz),  to = as.POSIXct(tab2$summary_end_datime, tz = tz))
    tab1_sec <- tab1_sec[!is.na(tab1_sec$datime_start),]

    tab3 <- dplyr::filter(period, subject_ID == particip[ii])

    for (jj in 1 : nrow(tab3)){

      row <- tab3[jj,]
      if(row$summary_start_datime > row$summary_end_datime) { art <- row$summary_start_datime; row$summary_start_datime <-
        row$summary_end_datime; row$summary_end_datime <- art }
      report <- data.frame(matrix(vector(), nrow=1, length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
      mat <- portion_withoverlaps(tab1_sec, from = row$summary_start_datime,  to = row$summary_end_datime)

      if(any(mat$interval_type == "EXCLUDED")){
        mat[mat$interval_type == "EXCLUDED",]$duration <- as.numeric(difftime(mat[mat$interval_type == "EXCLUDED",]$datime_end, mat[mat$interval_type == "EXCLUDED",]$datime_start, units = "mins"))
      }
      ex <- mat[mat$interval_type == "EXCLUDED",]

      rem = FALSE
      if(nrow(ex) > 0){rem = TRUE}

      mat$exact1 <- ifelse(mat$datime_start < row$summary_start_datime, 1 - (((as.numeric(difftime(row$summary_start_datime, mat$datime_start, units = "mins")))) / as.numeric(as.character(mat$duration))), 1)#row$summary_start_datime - mat$datime_start
      mat$exact2 <- ifelse(mat$datime_end > row$summary_end_datime, (((as.numeric(difftime(row$summary_end_datime, mat$datime_start, units = "mins")))) / as.numeric(as.character(mat$duration))), 1) #row$summary_end_datime - mat$datime_start
      mat$exact3 <-  mat$exact1 + mat$exact2 - 1
      mat$duration_adj <- mat$exact3 * as.numeric(mat$duration)
      mat$sleep_time_adj <- mat$exact3 * mat$sleep_time
      mat$efficiency <- as.numeric(as.character(mat$efficiency))

      matex <- dplyr::filter(mat, interval_type %in% c("REST", "EXCLUDED"))
      mat0 <- dplyr::filter(mat, interval_type %in% c("REST", "SLEEP"))

      # Note: efficiency will be under "REST" for AMI and under "SLEEP" for Actiware
  if(all(mat0$actigraph_brand == "Actiware")){
      if(nrow(mat0) > 0){
        byint2 <- NULL
        inu <- unique(dplyr::filter(mat0, interval_type %in% c("REST"))$interval_number)
        for (k in 1 : length(inu)){
          byint <- dplyr::filter(mat0, interval_number == inu[k])
          byint[byint$interval_type == "REST",]$efficiency <- ifelse( nrow(byint[byint$interval_type == "SLEEP",]) > 0,
                                                                      byint[byint$interval_type == "SLEEP",]$efficiency, NA )
          byint2 <- rbind(byint2, byint)
        }
      mat0 <- byint2
      }
  }

      mat2 = NULL
      mat2 <-  mat0 %>%
        group_by(interval_type) %>%
        summarise(interval_number = n(),
                  number_exact = sum(as.numeric(as.character(exact3)), na.rm = T),
                  Duration = sum(duration_adj, na.rm = T),
                  Sleep_time = sum(sleep_time_adj, na.rm = T),
                  sleep_efficiency = sum(prop.table(duration_adj[!is.na(duration_adj)]) * efficiency[!is.na(efficiency)]),
                  longest_period = max(sleep_time_adj, na.rm = T),
                  shortest_period = min(sleep_time_adj, na.rm = T)
        )

      report$Actisoft_ID <- y
      report$period_number <- jj
      report$report_duration_m <- abs(as.numeric(row$summary_duration_h) * 60)
      report$number_of_rests_exact <- as.numeric(as.character(mat2$number_exact[1]))
      report$number_of_sleeps_exact <- as.numeric(as.character(mat2$number_exact[2]))
      report$number_of_rests <-  nrow(matex)
      report$number_of_sleeps <- mat2$interval_number[2]
      report$total_time_in_bed <- as.numeric(sum(matex$duration_adj, na.rm = T))
      report$total_sleep <- mat2$Sleep_time[2]
      report$sleep_efficiency <- ifelse(mat$actigraph_brand[1] == "AMI", round(mat2$sleep_efficiency[1],2), round(mat2$sleep_efficiency[2],2) )
      report$longest_sleep_period <- mat2$longest_period[2]
      report$shortest_sleep_period <- mat2$shortest_period[2]
      report$with_excluded_bad <- FALSE
      y <- y +  1

      if (remove_bad == TRUE){
        if(sum(mat$bad, na.rm = TRUE) > 0){ # For AMI, removing sleep period is partially scored as Bad
          report$number_of_rests_exact <- report$number_of_sleeps_exact <- report$total_sleep <- report$sleep_efficiency <-
          report$longest_sleep_period <- report$shortest_sleep_period <- NA
          report$with_excluded_bad <- TRUE
        }

        if(rem == TRUE){ # For Actiwatch, removing sleep period with Excluded
           report$sleep_efficiency <- report$number_of_sleeps_exact <- report$total_sleep <- report$number_of_sleeps <-
          report$longest_sleep_period <- report$shortest_sleep_period <- NA
          report$with_excluded_bad <- TRUE
          report$number_of_rests_exact <- sum(report$number_of_rests_exact, sum(mat[mat$interval_type=="EXCLUDED",]$exact3, na.rm = T), na.rm = TRUE)
         report$number_of_sleeps <- ifelse(nrow(ex) == nrow(mat), NA, report$number_of_sleeps)
        report$number_of_sleeps_exact <- ifelse(nrow(ex) == nrow(mat), NA, report$number_of_sleeps_exact)
        }
      }

# when there's a rest period without sleep AND no excluded or bad period, then total_sleep = sleep_efficiency = 0
if(nrow(dplyr::filter(mat0, interval_type == "SLEEP")) > 0){ # if there's at least one sleep
  if(rem == FALSE && all(mat0$actigraph_brand == "Actiware")){
    if( all(is.na( dplyr::filter(mat0, interval_type == "SLEEP")$datime_start)) == TRUE){report$shortest_sleep_period <- report$longest_sleep_period <- report$number_of_sleeps <- report$number_of_sleeps_exact <- report$total_sleep <- report$sleep_efficiency <- 0}

    }
#remove_bad == FALSE
  if (sum(mat$bad, na.rm = TRUE) == 0 && all(mat0$actigraph_brand == "AMI")){ # no bad period and actigraph = AMI
    if(all(dplyr::filter(mat0, interval_type == "SLEEP")$duration == 0)){report$shortest_sleep_period <- report$longest_sleep_period <- report$number_of_sleeps <- report$number_of_sleeps_exact <- report$total_sleep <- report$number_of_sleeps <- report$sleep_efficiency <- 0} }
  }


if(nrow(dplyr::filter(mat0, interval_type == "SLEEP")) == 0){ # if no sleep or rest with in the interval
  if(rem == FALSE){ # if no excluded
    report$sleep_efficiency <- NA
    report$number_of_sleeps <- report$number_of_rests <- report$number_of_sleeps_exact <- report$number_of_rests_exact <- report$total_time_in_bed <- report$total_sleep <- report$shortest_sleep_period <- report$longest_sleep_period <- 0
  }
}

      report$with_forced_sleep <- ifelse(nrow(mat0[mat0$interval_type == "FORCED SLEEP",]) > 0, TRUE, FALSE)
      report$with_forced_wake <- ifelse(nrow(mat0[mat0$interval_type == "FORCED WAKE",]) > 0, TRUE, FALSE)
      report$with_custom_interval <- ifelse(nrow(mat0[mat0$interval_type == "CUSTOM",]) > 0, TRUE, FALSE)

      if(as.numeric(row$summary_duration_h) < 0){ art <- row$summary_start_datime; row$summary_start_datime <-
        row$summary_end_datime; row$summary_end_datime <- art }

      report <- cbind(row, report)
      report$summary_duration_h <- as.numeric(report$summary_duration_h)
      report2 <- rbind(report2, report)

    }

  }

  report2
}



