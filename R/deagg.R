#' Deaggregation of reports for summary_type sequential or daily.
#'
#' @param period a dataframe containing participants, start and end date/time period
#'
#' @return a dataframe
#'
#' @examples
#' library("lubridate")
#' library("dplyr")
#' p4 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA_2\\input parameters example period mixed.csv"), sep = ",", header = TRUE, skip = 0)
#' p4$summary_start_datime <- lubridate::dmy_hm(p4$summary_start_datime,  tz = "UTC")
#' p4$summary_end_datime <- lubridate::dmy_hm(p4$summary_end_datime,  tz = "UTC")
#' d <- deagg(period = p4)
#'
#' @export
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days seconds_to_period
#'
#'
deagg <-  function(period){ #deaggregation

  df <- d2 <- particip <- ds0 <- ds <- matr <- df2 <- subject_ID <- summary_type <- NULL
  period <- tbl_df(period)

  if( length(period$summary_end_datime[is.na(period$summary_end_datime)] > 0)){
    period$summary_end_datime[is.na(period$summary_end_datime)] <- period$summary_start_datime[is.na(period$summary_end_datime)] + hours(period$summary_duration_h[is.na(period$summary_end_datime)])
  }
  period$summary_duration_h[!is.na(period$summary_duration_h)] <- as.numeric(period$summary_duration_h[!is.na(period$summary_duration_h)]) * 3600

  period$summary_duration_h[is.na(period$summary_duration_h)] <-
    as.numeric(difftime(period$summary_end_datime[is.na(period$summary_duration_h)],  period$summary_start_datime[is.na(period$summary_duration_h)], units="secs"))

  particip <- as.vector(t(dplyr::distinct(period, subject_ID)))

  for (ii in 1 : length(particip)){
    ds0 <- ds <- matr <- NULL
    ds0 <- dplyr::tbl_df(filter(period, subject_ID == particip[ii], summary_type %in% c("first", "time_to_time")))
    ds <- dplyr::tbl_df(filter(period, subject_ID == particip[ii], summary_type %in% c("sequential", "daily"))) #daily and sequential

  if(nrow(ds) > 0){
    for (ll in 1 : nrow(ds)){
      dsl <- ds[ll,]

      if (abs(as.numeric(difftime(dsl$summary_end_datime[1] , dsl$summary_start_datime[1], units = "days"))) > 1 ) {
        dsl2 <- dsl
        dsl2$summary_end_datime <- dsl$summary_start_datime + dsl$summary_duration_h

        dif <- as.numeric(dsl$summary_end_datime[1] - dsl2$summary_start_datime[1])
        dsl2 <- dsl2[rep(1 : nrow(dsl2), times = abs(dif)),]

        a <- ifelse (dsl$summary_start_datime[1] > dsl$summary_end_datime[1], -1, 1)
        dsl2$summary_start_datime <- seq.POSIXt(dsl$summary_start_datime[1], dsl$summary_end_datime[1] - a * days(1), paste (a, "days"))
        dsl2$summary_end_datime <- dsl2$summary_start_datime + dsl2$summary_duration_h

        matr <- rbind(matr, dsl2)
          }
      else matr <- rbind(matr, dsl)
      }
    }
    df <- dplyr::bind_rows(ds0, matr, df)

  }


 df$summary_duration_h <- df$summary_duration_h/3600
 df
}

