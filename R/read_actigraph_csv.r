#' Imports scored actigraphy data from different actigraph brand output files.
#'
#' Place in your working directory the folder EXAMPLE_DATA containing the subfolders 'Actiware' and 'AMI'.
#' It imports all the CSV files from the above folders and so far, it supports CSV files only. A standard file name is not required.
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
#' head(read_actigraph_csv(x = "C:\\1\\EXAMPLE_DATA"))
#'


read_actigraph_csv <- function(x = "EXAMPLE_DATA", tz = "UTC", ...){
interval_type <- NULL
if (dir.exists(paths = paste(x,"//AMI", sep = "")) == FALSE &
      dir.exists(paths = paste(x,"//Actiware", sep = "")) == FALSE){ # for different files in a single folder
    files0 <- list.files( path = x, pattern = "*.csv") # list all the csv files

    dir.create(path = paste(x,"//AMI", sep = ""), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    dir.create(path = paste(x,"//Actiware", sep = ""), showWarnings = TRUE, recursive = FALSE, mode = "0777")

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

    files <- list.files( path = paste(x,"\\Actiware", sep=""), pattern = "*.csv") # list all the csv files

  if (length(files) > 0){
  #dat <- do.call(rbind, lapply(paste("EXAMPLE_DATA\\Actiware\\", files, sep = ""), function(x) read.csv(x, sep = ",", header = TRUE, skip = 0)))
  dat <- data.table::rbindlist(lapply(paste(x, "\\Actiware\\", files, sep = ""), function(x) read_csv_filename(x)), use.names = TRUE, fill = TRUE ) #, sep = ",", header = TRUE, skip = 0
  dat <- dplyr::tbl_df(dat) # table dataframe

  # selecting the useful variables.
  useful <- c("analysis_name", "subject_id", "interval_type", "interval_number", "start_date",
              "start_time", "end_date", "end_time", "duration", "efficiency", "sleep_time", "file_name")
  #dat <- dplyr::select_(dat, .dots = useful)
  dat <- dat[, useful]
  dat <- tibble::add_column(dat, actigraph_brand = rep("Actiware",nrow(dat)), .before = 1)
  #dat <- dplyr::rename(dat, subject_id = subject_ID) #subject_ID = dat$subject_id
  data.table::setnames(dat, old = "subject_id", new = "subject_ID")
  dat$bad <- NA
  }
  else dat <- NULL

  ## default bad set to NA -- bad variable is equivalent to having an EXCLUDED interval overalp with a user requested interval
  ## if there is an overlap, the bad variable will be filled in with something other than NA and 0 in the report

  # Importing from AMI actigraphs

  files2 <- list.files( path = paste(x,"\\AMI", sep=""), pattern = "*.csv") # list all the csv files

  if (length(files2) > 0){

  ## Did you change all files to .csv before importing?
  # dat2 <- do.call(rbind.fill, lapply(paste("EXAMPLE_DATA\\AMI\\", files2, sep = ""), function(x) read_csv_filename(x, sep = ",", header = TRUE, skip = 0)))
  # dat2 <- do.call(bind_rows, lapply(paste("EXAMPLE_DATA\\AMI\\", files2, sep = ""), function(x) read_csv_filename(x, sep = ",", header = TRUE, skip = 0)))
  dat2 <- data.table::rbindlist(lapply(paste(x, "\\AMI\\", files2, sep = ""), function(x) read_csv_filename(x)), use.names = TRUE, fill = TRUE) #, sep = ",", header = TRUE, skip = 0
  dat2 <- dplyr::tbl_df(dat2) # table dataframe
  dat2 <- dat2[!is.na(dat2$IntNum),] # removing row containing NA's (Those giving the mean and the sd)

  useful2 <- c("ID",  "IntName", "IntNum", "sdate", "stime", "edate", "etime", "dur", "pslp", "smin", "bad", "file_name")

  #dat2 <- select_(dat2, .dots = useful2)
  dat2 <- dat2[, useful2]
  dat2 <- tibble::add_column(dat2, actigraph_brand = rep("AMI", nrow(dat2)), .before = 1)
  dat2 <- tibble::add_column(dat2, subject_ID = rep(NA, nrow(dat2)), .before = 2)

  # using the variables names from Actiware

  #dat2 <- dplyr::rename(dat2, ID = analysis_name , IntName = interval_type ,  IntNum= interval_number , sdate = start_date , stime = start_time ,
  #                     edate = end_date , etime = end_time , dur = duration , smin = sleep_time , pslp = efficiency )

  #dat2 <- dplyr::rename(dat2, analysis_name = dat2$ID, interval_type = dat2$IntName, interval_number = dat2$IntNum, start_date = dat2$sdate, start_time = dat2$stime,end_date = dat2$edate, end_time = dat2$etime, duration = dat2$dur, sleep_time = dat2$smin, efficiency = dat2$pslp)
  data.table::setnames(dat2, old = c("ID", "IntName", "IntNum", "sdate", "stime", "edate", "etime", "dur", "smin", "pslp" ), new = c("analysis_name", "interval_type", "interval_number", "start_date", "start_time", "end_date", "end_time", "duration", "sleep_time", "efficiency"))

  dat2$subject_ID <- gsub("_.*$", "", dat2$analysis_name)
  dat2$efficiency <- ifelse(dat2$interval_type == "Down", dat2$efficiency, NA)
  ## note that AMI sleep efficiency is "pslp" where IntType == "Down" ONLY and NOT "pslp" where IntType == "O - O"
  ##* What would be the best option to match "efficiency" and "pslp" in both datasets?
  ## I extracted the one we want to keep here but keep in mind when merging that efficiency will be under "REST" for AMI and under "SLEEP" for Actiware

  }

  else dat2 <- NULL

  #alldata <- rbind.fill(dat,dat2) # merging both datasets
  ll <- list(dat, dat2)
  alldata <- data.table::rbindlist(ll, use.names = TRUE, fill = TRUE, idcol=FALSE)
  alldata <- alldata %>% dplyr::mutate(interval_type = factor(interval_type, levels = c("REST", "SLEEP", "ACTIVE","EXCLUDED", "Down","O - O","Up", "24-Hr" )))
  #data.table::setattr(alldata$interval_type,"levels",c("REST", "SLEEP", "ACTIVE","EXCLUDED", "Down","O - O","Up", "24-Hr" ))

# if(missing(form)){ form = "%d/%m/%Y"}
#  alldata$start_date <- as.POSIXct( strptime( alldata$start_date, format = form), tz = tz)
#  alldata$end_date <- as.POSIXct( strptime( alldata$end_date, format = form), tz = tz)
#  alldata$datime_start <- alldata$start_date + hms(alldata$start_time) #as.POSIXct(paste( alldata$start_date, alldata$start_time), tz = tz)
#  alldata$datime_end   <- alldata$end_date + hms(alldata$end_time) #as.POSIXct(paste( alldata$end_date, alldata$end_time), tz = tz)

  alldata$datime_start <- paste( as.POSIXct( strptime( alldata$start_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$start_time)
  alldata$datime_end   <- paste( as.POSIXct( strptime( alldata$end_date, format = "%d/%m/%Y"), tz = "UTC"), alldata$end_time)

  alldata$datime_start[which(as.character(alldata$datime_start) == "NA NA")] <- NA
  alldata$datime_end[which(as.character(alldata$datime_end) == "NA NA")] <- NA


  #alldata$datime_start[which(alldata$datime_start == "NA NA")] <- NA
  #alldata$datime_end[which(alldata$datime_end == "NA NA")] <- NA

  # standardizing the interval_type
  alldata$interval_type[alldata$interval_type == "Down"] <- as.factor("REST")
  alldata$interval_type[alldata$interval_type == "O - O"] <- as.factor("SLEEP")
  alldata$interval_type[alldata$interval_type == "Up"] <- as.factor("ACTIVE")

alldata$sleep_time[alldata$interval_type == "REST"] <- NA
alldata$sleep_time[alldata$interval_type == "ACTIVE"] <- NA

alldata$efficiency[which(alldata$efficiency == ".") ] <- NA
alldata$efficiency <- as.numeric(as.character(alldata$efficiency))

alldata$efficiency[alldata$interval_type == "ACTIVE"] <- NA
  #alldata$sleep_time <- ifelse(alldata$interval_type == "ACTIVE", NA, alldata$sleep_time)
  #alldata$efficiency <- ifelse(alldata$interval_type == "ACTIVE", NA, alldata$efficiency)

  #alldata2 <- alldata %>%
  #  dplyr::group_by(interval_number, start_date, start_time, duration) %>%
  #  dplyr::filter_(all(interval_type != "24-Hr")) # removing intervals = "24-Hr"

  alldata2 <- alldata[alldata$interval_type != "24-Hr", ]

  #alldata2 <- alldata2[!is.na(alldata2$datime_start),]
  #alldata2 <- alldata2[!is.na(alldata2$datime_end),]
  #alldata2[alldata2$duration != ".", ]

  # In AMI, setting the interval_type == SLEEP periods to BAD when the BAD column != 0.

  alldata2$interval_type <- factor(alldata2$interval_type, levels = c(levels(alldata2$interval_type), "BAD"))
  alldata2[alldata2$interval_type == "SLEEP" & alldata2$bad > 0,]$interval_type <- factor("BAD")

  alldata2[alldata2$interval_type == "SLEEP" & alldata2$duration == 0,]$datime_start <- NA
  alldata2[alldata2$interval_type == "SLEEP" & alldata2$duration == 0,]$datime_end <- NA
                                       
  alldata2 <- droplevels(alldata2)

  alldata2
  }
