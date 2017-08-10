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
#' csd(x = rep1, baseline_sleep = baseline_sleep, reset = 2)

#'# Example 2 with no reset by adding large reset value
#' csd(x = rep1, baseline_sleep = baseline_sleep, reset = 1e5)

#' @export
#' @importFrom grDevices dev.new dev.off x11
#' @importFrom dplyr left_join group_by
#' @importFrom magrittr %>%
#' @import ggplot2
#'
csd <- function(x, baseline_sleep, reset = 2, plot = TRUE, tab = TRUE, ylabel, ...){
  out2 <-  out <- cumsum_2 <- def <- subject_ID <- period_number <- NULL

  if(length(x$with_excluded_bad == TRUE) > 0) {x <- x[x$with_excluded_bad == FALSE,]}
  x <- x %>% left_join(baseline_sleep, by = c("subject_ID" = "subject_ID"))
  x$def <- x$total_sleep - x$baseline_sleep

  cumsum_2 <- function(x, reset = 2){ # x: a vector of the deficiency generally is total sleep - baseline. y is the csd
    y <- rep(NA, length(x))
    for (i in 1 : length(x)){
      y[i] <- ifelse(x[i] < 0 ,
                     ifelse(i == 1 ,
                            x[i],
                            ifelse(y[i - 1] < 0, x[i] + y[i - 1], x[i])),
                     ifelse(i == 1,
                            0,
                            ifelse( i > reset,
                                    ifelse(all(x[(i - reset + 1) : (i - 1)] >= 0) ,
                                           0,
                                           ifelse(x[i] < abs(y[i - 1]), y[i - 1] + x[i], 0)),
                                    ifelse(y[i - 1] < 0, min(x[i] + y[i - 1], 0, na.rm = TRUE), 0))
                     )
      )

    }
    data.frame(def = x, csd = y)
  }

  x <- x %>% dplyr::group_by(subject_ID) %>% dplyr::mutate(cumsum_actual = cumsum(def))

  particip <- unique(x$subject_ID)
  for (i in 1 : length(particip)){
    out <- x[x$subject_ID == particip[i],]
    out$cumsum <- cumsum_2(out$def, reset = 1E6)$csd
    out$cumsum_reset <- cumsum_2(out$def, reset = reset)$csd
    out2 <- rbind(out2, out)
  }

  if(missing(ylabel)){ylabel <- "Cum sleep debt (in minutes)"}
  p <- ggplot2::ggplot(data = out2, aes(colour = subject_ID, x = period_number, y = cumsum_reset,
                          group = subject_ID)) +
    geom_line() +
    xlab("Period number") + ylab(ylabel) +
    geom_point()    + scale_x_continuous(breaks =  seq(1,max(out2$period_number),1)) +
    theme(legend.position="bottom")

 if(plot == TRUE) return(list(p, out2))
 else return(out2)

 }





