#' Cumulative sleep debt.
#'
#' @param x a report period
#' @param baseline_sleep a data frame containing the normal sleep duration or baseline by participant.
#' @param reset consecutive number of sleep periods that will set as zero the cumulative sleep debt when the total sleep in 24h is greater than the baseline_sleep.
#' It does not depend on the exceeding duration or credit.
#' @param plot a logical variable. By default is TRUE.
#' @param ... optional parameters
#'
#' @return a data frame and optionally a graph. def is the daily sleep deficiency.
#'
#'@details if no reset is desired just set a large value e.g. 1E3. Three cumsum values are computed: cumsum, cumsum_reset and cumsum_actual.
#'The last one allows the deficiency to be in credit (positive values).
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

#' rep1$with_excluded_bad[2] <- TRUE
#' baseline_sleep <-  data.frame(subject_ID = c("example01", "example02"), baseline_sleep = c(60 * 5, 60 * 7))  # normal sleep duration by participant
#' reset <- 2
#' csd(x = rep1, baseline_sleep = baseline_sleep, reset = 2)

#'# Example 2 with no reset by adding large reset value
#' csd(x = rep1, baseline_sleep = baseline_sleep, reset = 1e5)



#' @export
#' @importFrom grDevices dev.new dev.off x11
#' @importFrom dplyr left_join group_by
#' @importFrom magrittr %>%
#' @import ggplot2
#'
csd <- function(x, baseline_sleep, reset = 2, plot = TRUE,...){
  cumsum_reset <- def <- subject_ID <- period_number <- NULL

  cumsum_res <- function(xx){
    for (i in 1:length(xx)){ xx[i] <- ifelse( sum(xx[1:i]) < 0, xx[i], abs(sum(xx[1:i-1])))}
    sum(xx)
  }

  x <- x[x$with_excluded_bad == FALSE,]
  x <- x %>% left_join(baseline_sleep, by = "subject_ID")
  x$def <- x$total_sleep - x$baseline_sleep
  x$def2 <- x$def
  x$def2[x$def2 > 0] <- 0

  x2 <-  x %>%
    group_by(subject_ID) %>%
    mutate(cumsum = cumsum(def))

  particip <- unique(x2$subject_ID)
  x2$cumsum_reset <- 0

  colName <- c("subject_ID", "period_number", "total_sleep", "baseline_sleep", "def", "cumsum",	"cumsum_reset", "cumsum_actual")
  out <- data.frame(matrix(vector(), nrow = 1, length(colName), dimnames = list(c(), colName)), stringsAsFactors = F)
  out2 <- NULL
  for (i in 1 : length(particip)){
    tab <- filter(x2, subject_ID == particip[i])
    for (j in 1 : nrow(tab)){
      out$subject_ID <- tab$subject_ID[j]
      out$period_number <- tab$period_number[j]
      out$total_sleep <- tab$total_sleep[j]
      out$baseline_sleep <- tab$baseline_sleep[j]
      out$def <- tab$def[j]
      out$cumsum <- cumsum_res(x = tab$def[1:j]) #ifelse(sum(tab$def[1:j]) > 0 , 0 , sum(tab$def[1:j])) #tab$cumsum[j]
      out$cumsum_reset <- ifelse(j > reset, ifelse( all(tab$def[(j - reset + 1) : j] > 0) , 0, tab$cumsum_reset[j - 1] + tab$def[j]), out$cumsum)
      #out$cumsum_reset <- ifelse(j > reset, ifelse(sum(tab$def[(j - reset + 1) : j]) == 0, 0, tab$cumsum_reset[j - 1] + tab$def[j]), tab$cumsum[j])
      out2 <- rbind(out2, out)
    }

  }

  out2 <- out2 %>% group_by(subject_ID) %>%  mutate(cumsum_actual = cumsum(def))
  out3 <- NULL
  #out3 <- data.frame(subject_ID = particip, period_number = rep(0, length(particip)), total_sleep = rep(NA, length(particip))
  #                   , baseline_sleep = rep(NA, length(particip)), def = rep(0, length(particip)), cumsum = rep(0, length(particip))
  #                   , cumsum_reset = rep(0, length(particip)), cumsum_actual = rep(0, length(particip)))
  out3 <- rbind(out2, out3)

 p <- ggplot2::ggplot(data = out3, aes(colour = subject_ID, x = period_number, y = cumsum_reset,
                          group = subject_ID)) +
    geom_line() +
    xlab("Period number") + ylab("Cum sleep debt (in minutes)") +
    geom_point()    + scale_x_continuous(breaks =  seq(1,max(out3$period_number),1))

 if(plot == TRUE){
    #x11()
   return(list(p, out2))
  }
  else return(out2)
}



