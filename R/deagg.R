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
#' #View(d)


#'
#'
#' @export
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days seconds_to_period
#'
#'
deagg <-  function(period){ #deaggregation

  df <- d2 <- particip <- ds0 <- ds <- matr <- df2 <- NULL
  period <- tbl_df(period)
  #period$summary_duration_h <- lubridate::hours(period$summary_duration_h)

  if( length(period$summary_end_datime[is.na(period$summary_end_datime)] > 0)){
    period$summary_end_datime[is.na(period$summary_end_datime)] <- period$summary_start_datime[is.na(period$summary_end_datime)] + hours(period$summary_duration_h[is.na(period$summary_end_datime)])
  }
  period$summary_duration_h[!is.na(period$summary_duration_h)] <- as.numeric(period$summary_duration_h[!is.na(period$summary_duration_h)]) * 3600

  period$summary_duration_h[is.na(period$summary_duration_h)] <-
    as.numeric(difftime(period$summary_end_datime[is.na(period$summary_duration_h)],  period$summary_start_datime[is.na(period$summary_duration_h)], units="secs")) #lubridate::seconds_to_period

  #period$summary_duration_h[is.na(period$summary_duration_h)] <- as.numeric(difftime(period$summary_end_datime[is.na(period$summary_duration_h)],  period$summary_start_datime[is.na(period$summary_duration_h)], units="hours"))
  #period$summary_end_datime[is.na(period$summary_duration_h)] -  period$summary_start_datime[is.na(period$summary_duration_h)]

  particip <- as.vector(t(dplyr::distinct(period, subject_ID))) #actigraphy_file

  for (ii in 1 : length(particip)){

    ds0 <- dplyr::tbl_df(filter(period, subject_ID == particip[ii], summary_type %in% c("first", "time_to_time")))
    ds <- dplyr::tbl_df(filter(period, subject_ID == particip[ii], summary_type %in% c("sequential", "daily"))) #daily and sequential


    for (ll in 1 : nrow(ds)){
      dsl <- ds[ll,]

      if (abs(as.numeric(difftime(dsl$summary_end_datime[1] , dsl$summary_start_datime[1], units = "days"))) > 1 ) {
        dsl2 <- dsl
        dsl2$summary_end_datime <- dsl$summary_start_datime + dsl$summary_duration_h #days(dsl)

        dif <- as.numeric(dsl$summary_end_datime[1] - dsl2$summary_start_datime[1])
        dsl2 <- dsl2[rep(1 : nrow(dsl2), times = abs(dif)),]

        a <- ifelse (dsl$summary_start_datime[1] > dsl$summary_end_datime[1], -1, 1)
        dsl2$summary_start_datime <- seq.POSIXt(dsl$summary_start_datime[1], dsl$summary_end_datime[1] - a * days(1), paste (a, "days"))
        dsl2$summary_end_datime <- dsl2$summary_start_datime + dsl2$summary_duration_h

        matr <- rbind(matr, dsl2)
          }
      else matr <- rbind(matr, dsl)
      }

    #ll <- list(ds0, matr)
    #all <- data.table::rbindlist(ll, use.names = TRUE, fill = TRUE, idcol=FALSE)
    #ds0$summary_duration_h <- hms(ds0$summary_duration_h)
    #rbind(ds0, matr)
    df <- dplyr::bind_rows(ds0, matr)

    #alist <- list(ds0, matr)
    #ddtt <- data.table::rbindlist(alist, use.names = TRUE, fill = FALSE, idcol=FALSE)

  }

 df2 = dplyr::bind_rows(df, df2)
 df2$summary_duration_h <- lubridate::seconds_to_period(df2$summary_duration_h)
 df2
}



