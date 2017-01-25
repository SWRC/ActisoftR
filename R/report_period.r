
#' Generates reports based on input periods.
#'
#'
#' @param period a dataframe containing participants, start and end date/time period
#' @param acti_data a dataframe of the form 'acti_data'.
#' @param remove_bad a logical value used to indicate if the 'bad' or 'EXCLUDED' are removed. Set as TRUE.
#' @param tz is the time zone. Optional argument. tz = "UTC" by default.
#' @param ... Optional parameters
#'
#' @return a dataframe
#'
#'@details Periods partially scored as 'bad' or 'EXCLUDED' are due to off-wrist or other reasons.
#' In that case all summary estimates except ‘time in bed’ will be set to NA,
#' and the column ‘with_excluded_bad’ will show TRUE. See examples in the Vignettes.
#' This can be avoided using remove_bad = FALSE.
#'
#' @examples
#'# Example 1
#' library(lubridate)
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")
#' p <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA\\input parameters example period.csv"),
#'  sep = ",", header = TRUE, skip = 0)
#' p$summary_start_datime <- dmy_hm(p$summary_start_datime,  tz = "UTC")
#' p$summary_end_datime <- dmy_hm(p$summary_end_datime,  tz = "UTC")
#' rep1 <- report_period(period = p , acti_data = acti_data)
#' View(rep1)


#'# Example 2
#' acti_data2 <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_2")
#' p2 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA\\input parameters example period2.csv"),
#'  sep = ",", header = TRUE, skip = 0)
#' p2$summary_start_datime <- lubridate::dmy_hms(p2$summary_start_datime,  tz = "UTC")
#' p2$summary_end_datime <- lubridate::dmy_hms(p2$summary_end_datime,  tz = "UTC")
#' rep2 <- report_period(period = p2 , acti_data = acti_data2, remove_bad = TRUE)
#' View(rep2)


#'# Example 3
#' acti_data2 <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_2")
#' p3 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA_2\\input parameters example period for MVB_09-05-2016_163631_AWedited2.csv"),
#'  sep = ",", header = TRUE, skip = 0)
#' p3$summary_start_datime <- lubridate::dmy_hm(p3$summary_start_datime,  tz = "UTC")
#' p3$summary_end_datime <- lubridate::dmy_hm(p3$summary_end_datime,  tz = "UTC")
#' rep3 <- report_period(period = p3 , acti_data = acti_data2)
#' View(rep3)


#'# Example 4 mixed summary_type
#'# for 'first' summary type only summary_start_datime and the duration will be used.
#' acti_data2 <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_2")
#' p4 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA_2\\input parameters example period mixed.csv"), sep = ",", header = TRUE, skip = 0, na.strings=c("NA","NaN", " "))
#' p4$summary_start_datime <- lubridate::dmy_hm(p4$summary_start_datime,  tz = "UTC")
#' p4$summary_end_datime <- lubridate::dmy_hm(p4$summary_end_datime,  tz = "UTC")
#' rep4 <- report_period(period = p4 , acti_data = acti_data2)
#' View(rep4)


#'# Example 5 with Excluded periods
#' acti_data <- read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA_3")
#' p5 <- read.csv(file = paste("C:\\1\\EXAMPLE_DATA_3\\input parameters example period with Excluded.csv"),
#'  sep = ",", header = TRUE, skip = 0)
#' p5$summary_start_datime <- dmy_hm(p5$summary_start_datime,  tz = "UTC")
#' p5$summary_end_datime <- dmy_hm(p5$summary_end_datime,  tz = "UTC")
#' rep5 <- report_period(period = p5 , acti_data = acti_data)
#' View(rep5)


#' @export
#' @importFrom dplyr filter mutate summarise n
#' @importFrom lubridate dmy_hm dmy_hms ymd_hms days

report_period <- function(period, acti_data, remove_bad = TRUE, tz = "UTC",...){
  period <- with_tz(period, tz = tz)
  period <- tbl_df(period)
  actigraphy_file <- analysis_name <- interval_type <- exact3 <- duration <- sleep_time <- efficiency <- art <- NULL
  pos_sum_types = c("first", "daily", "sequential", "time_to_time") # possible summary types

  if (sum( match(x = pos_sum_types, table = period$summary_type), na.rm = TRUE ) == 0){
    stop("summary_type incorrectly defined. It must be set to: 'first', 'daily', 'sequential' or 'time_to_time'")}

  #period$summary_duration_h <- lubridate::hours(period$summary_duration_h)
  #period$summary_end_datime[which(is.na(period$summary_end_datime))] <- period$summary_start_datime[which(is.na(period$summary_end_datime))] +
  #period$summary_duration_h[which(is.na(period$summary_end_datime))]

  period <- deagg(period)
  sum_types <- as.vector(t(distinct(period, summary_type))) # actual summary types
  startend <- tbl_df(int_startend(period))
  particip <- as.vector(t(distinct(period, subject_ID)))

  acti_data$datime_start <- ymd_hms(acti_data$datime_start)
  acti_data$datime_end <- ymd_hms(acti_data$datime_end)
  acti_data <- with_tz(acti_data, tz = tz)

#acti_data <- tbl_df(filter(acti_data, analysis_name %in% particip, interval_type %in% c("REST", "SLEEP")))
acti_data <- tbl_df(filter(acti_data, subject_ID %in% particip, interval_type %in% c("REST", "SLEEP", "EXCLUDED", "FORCED SLEEP", "FORCED WAKE", "CUSTOM")))

  colName <- c("Actisoft_ID",	"period_number",
               "report_duration_m",	"number_of_rests",	"number_of_sleeps",	"number_of_rests_exact", "number_of_sleeps_exact",	"total_time_in_bed",
               "total_sleep",	"sleep_efficiency",	"longest_sleep_period",	"shortest_sleep_period",	"with_forced_wake",	"with_forced_sleep",
               "with_custom_interval",	"with_excluded_bad")

  report2 <- NULL
  y <- 1
  for (ii in 1 : length(particip)){

    tab1 <- filter(acti_data, subject_ID == particip[ii])
    tab2 <- filter(startend, subject_ID == particip[ii])
    tab1_sec <- portion_withoverlaps (tab1, from = tab2$summary_start_datime,  to = tab2$summary_end_datime)
    tab3 <- filter(period, subject_ID == particip[ii])

    for (jj in 1 : nrow(tab3)){

      row <- tab3[jj,] #participant
      if(row$summary_start_datime > row$summary_end_datime) { art <- row$summary_start_datime; row$summary_start_datime <-
        row$summary_end_datime; row$summary_end_datime <- art }
      report <- data.frame(matrix(vector(), nrow=1, length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
mat0 <- portion_withoverlaps(tab1_sec, from = row$summary_start_datime,  to = row$summary_end_datime)
mat <- dplyr::filter(mat0, interval_type %in% c("REST", "SLEEP"))

ex <- mat0[mat0$interval_type == "EXCLUDED",]

# to do: adapt for cases with more than two EXLUDED periods
rem = FALSE
if(nrow(ex) > 0){
#for (kk in 1 : nrow(mat)){
#  i1 <- interval(mat[kk,]$datime_start, mat[kk,]$datime_end)
#  i2 <- interval(ex$datime_start, ex$datime_end)
#  if(lubridate::int_overlaps(i1,i2) == TRUE) {rem = TRUE}
#  }
  rem = TRUE
  }

matex <- dplyr::filter(mat0, interval_type %in% c("REST", "EXCLUDED"))
matex[matex$interval_type == "EXCLUDED",]$duration <- difftime(matex[matex$interval_type == "EXCLUDED",]$datime_end, matex[matex$interval_type == "EXCLUDED",]$datime_start)

      # I need to check this
      mat$exact1 <- ifelse(mat$datime_start < row$summary_start_datime, 1 - ((row$summary_start_datime - mat$datime_start) / mat$duration), 1)
      mat$exact2 <- ifelse(mat$datime_end > row$summary_end_datime, ((row$summary_end_datime - mat$datime_start ) / mat$duration), 1)
      mat$exact3 <-  mat$exact1 + mat$exact2 - 1

      mat$duration_adj <- mat$exact3 * mat$duration
      mat$sleep_time_adj <- mat$exact3 * mat$sleep_time

mat$efficiency <- as.numeric(as.character(mat$efficiency))

      mat2 <-  mat %>%
        group_by(interval_type) %>%
        summarise(interval_number = n(),
                  number_exact = sum(as.numeric(exact3)),
                  Duration = sum(duration_adj),
                  Sleep_time = sum(sleep_time_adj),
                  sleep_efficiency = sum(prop.table(duration_adj) * efficiency), # not sure here. Verify
                  longest_period = max(sleep_time_adj),
                  shortest_period = min(sleep_time_adj)
        )

      report$Actisoft_ID <- y
      report$period_number <- jj
      report$report_duration_m <- abs(as.numeric(row$summary_duration_h)/60) 
      report$number_of_rests_exact <- as.numeric(mat2$number_exact[1])
      report$number_of_sleeps_exact <- as.numeric(mat2$number_exact[2])
      report$number_of_rests <-  nrow(matex) 
      report$number_of_sleeps <- mat2$interval_number[2]
      report$total_time_in_bed <- sum(matex$duration) 
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
  report$number_of_rests_exact <- report$number_of_sleeps_exact <- report$total_sleep <- report$sleep_efficiency <-
  report$longest_sleep_period <- report$shortest_sleep_period <- NA
  report$with_excluded_bad <- TRUE
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



  report2$summary_duration_h <- report2$summary_duration_h/3600
  tbl_df(report2)
}



