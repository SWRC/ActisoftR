#' Imports scored actigraphy data from different actigraph brand output files.
#'
#' Specify the path to the files. So far it supports CSV files only.
#' The imported files are merged creating an organic single file.
#' If the CSV files have different number of columns the function will fill missing columns with NAs.
#'
#' @param x the file path
#' @param tz the time zone
#' @param ... Optional parameters
#' @return a dataframe
#' @export
#' @importFrom dplyr tbl_df filter mutate select_ rename group_by
#' @importFrom data.table rbindlist setnames setattr
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA")}
#'


read_actigraph_csv <- function(x = "PATH_TO_DATA", tz = "UTC", ...){
interval_type <- rem_tag <- NULL
  rem_tag <- FALSE
  if (dir.exists(paths = paste(x,"//AMI", sep = "")) == FALSE &
      dir.exists(paths = paste(x,"//Actiware", sep = "")) == FALSE){
      files0 <- list.files( path = x, pattern = "*.csv")

      dir.create(path = paste(x,"//AMI", sep = ""), showWarnings = TRUE, recursive = FALSE, mode = "0777")
      dir.create(path = paste(x,"//Actiware", sep = ""), showWarnings = TRUE, recursive = FALSE, mode = "0777")
      rem_tag <- TRUE

      for (ii in 1 : length(files0)){
        one <- data.table::rbindlist(lapply(paste(x,"\\",files0[ii], sep = ""),
                                            function(x) read_csv_filename(x)), use.names = TRUE, fill = TRUE )

        if (any(colnames(one) == "ID")){ write.csv(one, paste(x,"\\AMI\\",files0[ii], sep = "") ) }
        if (any(colnames(one) == "analysis_name")){ write.csv(one, paste(x,"\\Actiware\\",files0[ii], sep = "") ) }
        if ( any(colnames(one) == "ID") == FALSE  && any(colnames(one) == "analysis_name") == FALSE){
          print("there is at least a file that is not of the from AMI or Actiware in the folder. File(s) where not
                imported")
        }
        }

  }

  files <- list.files( path = paste(x,"\\Actiware", sep=""), pattern = "*.csv")

  if (length(files) > 0){
  dat <- data.table::rbindlist(lapply(paste(x, "\\Actiware\\", files, sep = ""), function(x) read_csv_filename(x)), use.names = TRUE, fill = TRUE )
  dat <- dplyr::tbl_df(dat)

  useful <- c("analysis_name", "subject_id", "interval_type", "interval_number", "start_date",
              "start_time", "end_date", "end_time", "duration", "efficiency", "sleep_time", "file_name")
  dat <- dat[, useful]
  dat <- tibble::add_column(dat, actigraph_brand = rep("Actiware",nrow(dat)), .before = 1)
  data.table::setnames(dat, old = "subject_id", new = "subject_ID")
  dat$bad <- NA
  }
  else dat <- NULL

  ## default bad set to NA -- bad variable is equivalent to having an EXCLUDED interval overalap with a user requested interval
  ## if there is an overlap, the bad variable will be filled in with something other than NA and 0 in the report

  # Importing from AMI actigraphs
  files2 <- list.files( path = paste(x,"\\AMI", sep=""), pattern = "*.csv")
  if (length(files2) > 0){
  dat2 <- data.table::rbindlist(lapply(paste(x, "\\AMI\\", files2, sep = ""), function(x) read_csv_filename(x)), use.names = TRUE, fill = TRUE)
  dat2 <- dplyr::tbl_df(dat2)
  dat2 <- dat2[!is.na(dat2$IntNum),]

  useful2 <- c("ID",  "IntName", "IntNum", "sdate", "stime", "edate", "etime", "dur", "pslp", "smin", "bad", "file_name")

  dat2 <- dat2[, useful2]
  dat2 <- tibble::add_column(dat2, actigraph_brand = rep("AMI", nrow(dat2)), .before = 1)
  dat2 <- tibble::add_column(dat2, subject_ID = rep(NA, nrow(dat2)), .before = 2)

  # using the variables names from Actiware
  data.table::setnames(dat2, old = c("ID", "IntName", "IntNum", "sdate", "stime", "edate", "etime", "dur", "smin", "pslp" ), new = c("analysis_name", "interval_type", "interval_number", "start_date", "start_time", "end_date", "end_time", "duration", "sleep_time", "efficiency"))

  dat2$subject_ID <- gsub("_.*$", "", dat2$analysis_name)
  dat2$efficiency <- ifelse(dat2$interval_type == "Down", dat2$efficiency, NA)
  # AMI sleep efficiency is "pslp" where IntType == "Down" ONLY and NOT "pslp" where IntType == "O - O"
  }

  else dat2 <- NULL

  if(rem_tag == TRUE){
    # will remove the folder created above
    unlink( paste(x,"//AMI", sep = ""), recursive = T)
    unlink( paste(x,"//Actiware", sep = ""), recursive = T)
  }

  ll <- list(dat, dat2)
  alldata <- data.table::rbindlist(ll, use.names = TRUE, fill = TRUE, idcol=FALSE)
  alldata <- alldata %>% dplyr::mutate(interval_type = factor(interval_type, levels = c("REST", "SLEEP", "ACTIVE","EXCLUDED", "Down","O - O","Up", "24-Hr" )))

  alldata$datime_start <- paste( as.POSIXct( strptime( alldata$start_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$start_time)
  alldata$datime_end   <- paste( as.POSIXct( strptime( alldata$end_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$end_time)

  alldata$datime_start[which(as.character(alldata$datime_start) == "NA NA")] <- NA
  alldata$datime_end[which(as.character(alldata$datime_end) == "NA NA")] <- NA

  alldata$interval_type[alldata$interval_type == "Down"] <- as.factor("REST")
  alldata$interval_type[alldata$interval_type == "O - O"] <- as.factor("SLEEP")
  alldata$interval_type[alldata$interval_type == "Up"] <- as.factor("ACTIVE")

  alldata$sleep_time[alldata$interval_type == "REST"] <- NA
  alldata$sleep_time[alldata$interval_type == "ACTIVE"] <- NA

  alldata$efficiency[which(alldata$efficiency == ".") ] <- NA
  alldata$efficiency <- as.numeric(as.character(alldata$efficiency))

  alldata$efficiency[alldata$interval_type == "ACTIVE"] <- NA
  alldata2 <- alldata[alldata$interval_type != "24-Hr", ]

  # In AMI, setting the interval_type == SLEEP periods to BAD when the BAD column != 0.
  if( nrow(alldata2[alldata2$interval_type == "SLEEP" & alldata2$bad > 0 & !is.na(alldata2$bad),]) > 0){
    alldata2$interval_type <- factor(alldata2$interval_type, levels = c(levels(alldata2$interval_type), "BAD"))
    alldata2[alldata2$interval_type == "SLEEP" & alldata2$bad > 0 & !is.na(alldata2$bad),]$interval_type <- "BAD"
  }

  alldata2 <- droplevels(alldata2)

  alldata2
  }

