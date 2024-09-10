#' Plots participants' SLEEP/WAKE intervals.
#'
#' @param x a dataframe.
#' @param datebreaks is the distance between breaks in the x-axis. "12 hour" by default.
#' @param acolor specifies the colors for the interval_type.
#' @param decal is a parameter for shifting the start date.
#' @param export if TRUE, it will export the data as CSV.
#' @param base matches the participants at the same start date.
#' @param si defines the size of the geom_segment, 8 by default.
#' @param tz is the times zone. tz = "UTC" by default.
#' @param tz2 an additional time zone used for a secondary x-axis.
#' @param show_plot logical to produce the plot or not.
#' @param shade if TRUE, it will draw in light green the home night period. FALSE by default.
#' @param local.shade (deprecated) if TRUE, it will draw in gray the local night period. FALSE by default.
#' @param homeTZ a data frame containing the all subject_IDs in x and the time zone. It is used for the
#' home night shading.
#' @param ... optional parameters.
#' @return a plot.
#'
#' @examples
#' library("dplyr")
#' library("lubridate")
#' data(act)
#' dat <- act %>% group_by(subject_ID) %>%
#'   mutate(start = min(datime_start)) %>%
#'   ungroup %>% filter(datime_start <= start + days(10), interval_type != "ACTIVE")
#' plot_Darwent(dat,  acolor = c("#D55E00", "black", "#56B4E9"),
#'                   tz2 = "Pacific/Auckland",
#'                   si = c(4, 3, 2.5))
#'
#' # Adding the 'home' night shade
#' # creating a data frame with the home time zone of the individuals
#' homeTZ = data.frame(subject_ID = unique(dat$subject_ID),
#'                     TZ = rep("Pacific/Auckland", 5),
#'                     stringsAsFactors = FALSE)
#' plot_Darwent(dat, acolor = c("#D55E00", "black", "#56B4E9"),
#'              si = c(4, 3, 2.5),
#'              tz = "UTC", tz2 = "Pacific/Auckland", shade = TRUE,
#'              homeTZ = homeTZ)
#'

#'
#' @export
#' @importFrom dplyr distinct left_join arrange summarize
#' @importFrom scales date_breaks date_format
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lubridate is.POSIXct floor_date tz ceiling_date
#' @importFrom tidyr unnest
#' @import ggplot2
#' @import RColorBrewer
#' @rdname plot.Darwent

plot_Darwent <- function(x, datebreaks = "12 hour",
                         base = "TRUE", acolor, decal, export = FALSE, show_plot = TRUE,
                          si, tz = "UTC", tz2, homeTZ, shade = FALSE, local.shade = FALSE, ...){

  subject_ID <- datime_start <- interval_type <- datime_end <- grayzone.start <-
  grayzone.end <- dec <- HNS <- LNS <- min_date <- NULL

  foo <- as.data.frame(x)
  foo <- foo[!is.na(foo$datime_start),]
  foo <- foo[!is.na(foo$datime_end),]

  if(is.POSIXct(foo$datime_start) == FALSE) {foo$datime_start <- as.POSIXct(foo$datime_start, tz = tz)}
  if(is.POSIXct(foo$datime_end) == FALSE) {foo$datime_end = as.POSIXct(foo$datime_end, tz = tz)}

  foo$interval_type <- as.factor(toupper(as.character(foo$interval_type)))
  foo <- arrange(foo, interval_type)


  dif_int <- setdiff(unique(foo$interval_type), c("ACTIVE" ,"EXCLUDED" ,"REST", "SLEEP" , "WORK", "SLEEP_B", "SLEEP_C", "DIARY", "BAD") )

  if(missing(acolor)) { # Defining the color
                    match_color <- c("ACTIVE" = "#009E73", "EXCLUDED" = "#D55E00", "REST" =  "black",
                   "SLEEP" = "#56B4E9", "WORK" = "#CC79A7", "SLEEP_B" = "#f03b20", "SLEEP_C" = "#F0E442", "DIARY" = "#999999", "BAD" = "#D55E00")

    if(length(dif_int) > 0){
      cols_ext <- brewer.pal(length(dif_int) + 2, "Paired")
      names(cols_ext) <- dif_int
      match_color <- c(match_color, cols_ext)
    }
  acolor = match_color
  }

  part <- length(unique(foo$subject_ID))
  participant <- as.factor(unique(foo$subject_ID))
  participant <- droplevels(participant)
  foo$subject_ID <- factor(as.character(foo$subject_ID))
  activ <- length(unique(foo$interval_type))

  if(missing(decal)){ decal = data.frame(subject_ID = participant, dec = rep(0, part))}


  if(missing(homeTZ)){homeTZ = data.frame(subject_ID = factor(unique(participant)), TZ = rep("UTC", length(unique(participant))), stringsAsFactors = F) }

  if(shade == TRUE){HNS <- home.night.shade(x = foo, homeTZ = homeTZ)
    HNS$datime_start <- with_tz(HNS$datime_start, tzone = tz)
    HNS$datime_end <- with_tz(HNS$datime_end, tzone = tz)
  }

  if(local.shade == TRUE){LNS <- local.night.shade(localTZ = localTZ)}

  match_start <- function(x){ # will match all the participants at the same start date when base = TRUE.
      x2 <- x %>% group_by(subject_ID) %>%
      summarize(min_date = floor_date(min(datime_start) , unit = "day")) %>%
      mutate(diff = difftime(min(min_date),  min_date, units = "secs")) #first

    x <- x %>% left_join(x2, by = c("subject_ID" = "subject_ID"))
    x$subject_ID <- factor(x$subject_ID)
    x <- x %>% left_join(decal, by = c("subject_ID" = "subject_ID"))
    x$dec <- x$dec * 60 * 60 * 24
    x$datime_start <- x$datime_start + x$diff + x$dec
    x$datime_end <- x$datime_end + x$diff + x$dec
    cbind(x)
  }

  if(base == "TRUE"){ # matching the participants at the same start date
    foo <- match_start(foo)
    if(shade == TRUE){HNS <- match_start(HNS)}
    if(local.shade == TRUE){LNS <- match_start(LNS)}

  }

  if (export == TRUE){
    usef <- c("subject_ID", "datime_start", "datime_end", "interval_type")
    Darw_data <- foo[, usef] %>% left_join(decal, by = "subject_ID")
    write.csv(Darw_data, "Darw_data.csv")
  }

  foo$interval_type <- as.factor(as.character(foo$interval_type))
  if(missing(si)){ si = rep(8, activ)}

  p <- ggplot2::ggplot() + {
    # home night shade
    if(shade == TRUE) {geom_segment(data = HNS,
                                    aes(x = as.numeric(datime_start),
                                        xend = as.numeric(datime_end),
                                        y = subject_ID, yend = subject_ID), size = 13,
                                    col = "green",
                                    alpha = 0.22)} } + {
    # local night shade
    if(local.shade == TRUE) {geom_segment(data = LNS,
             aes(colour = interval_type, x = as.numeric(as.POSIXct(datime_start)),
             xend = as.numeric(as.POSIXct(datime_end)),
             y = subject_ID, yend = subject_ID), size = 12, col = "black", alpha = 0.22)}
    else localTZ <- NULL} +
    geom_segment(data = foo, aes(colour = interval_type, x = as.numeric(datime_start), xend = as.numeric(datime_end),
                                 y = subject_ID, yend = subject_ID, size = interval_type)) +
    scale_size_manual(values = si)  +
    theme_bw() + xlab(paste("Time in", tz(foo$datime_start[1]))) +  ylab("Participant(s)") +
    theme_classic() +
    scale_color_manual(values = acolor) +
    scale_fill_manual(name = "Act")

  if(show_plot == TRUE){

  x_axis_lab <- seq.POSIXt(floor_date(min(foo$datime_start), unit = "day"),
             ceiling_date(max(foo$datime_start), unit = "day"), by = datebreaks, tz = tz)

  x_axis_lab_num <- as.integer(as.numeric(x_axis_lab))

  p <- p + { if(!missing(tz2) == TRUE) scale_x_continuous(paste("Time in", tz = tz),
                                                        breaks = x_axis_lab_num,
              labels = format(x_axis_lab, format = "%H:%M", tz = tz),
              sec.axis = sec_axis(~ . + 0, breaks = x_axis_lab_num,
              labels = format(with_tz(x_axis_lab, tzone = tz2),
              format = "%H:%M"), name = paste("Time in", tz2)) ) } + {

  if(missing(tz2) == TRUE) scale_x_continuous(paste("Time in", tz),
                                breaks = x_axis_lab_num,
                                labels = format(x_axis_lab, format = "%H:%M", tz = tz) )
                }

  p <- p +  theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
    theme(plot.title = element_text(color = "black", size = 18, hjust = 0.5)) +
    theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
    ggtitle("Darwent plot")
  p
  }

  else
    p + scale_x_continuous(paste("Time in", tz), breaks = x_axis_lab_num,
                           labels = format(x_axis_lab, format = "%H:%M", tz = tz) ) +
    theme(axis.text.x = element_text(size = 8, angle = 90 , vjust = 0.5)) +
    theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm") ) +
    theme(plot.title = element_text(color = "black", size = 18, hjust = 0.5)) +
    theme(panel.grid.major.x = element_line(colour = 'gray', size = 0.1 )) +
    ggtitle("Darwent plot")
}



#' Plots with-in participant activity intervals (SLEEP, REST, ACTIVE, etc) in 24 hour sections.
#'
#' @param dat a dataframe.
#' @param acolor the color of the lines.
#' @param si defines the size of the geom_segment, 1.25 by default.
#' @param tz the time zone.
#' @param tz2 an additional time zone.
#' @param sp the starting point in the x-axis. Set as 00:00:00 by default.
#' @param with_date allows adding the cutpoint date to the y-axis. with_date = FALSE by default.
#' @param alphas is the transparency.
#' @param hourbreaks is the number of labels on the x-axis.
#' @param ... Optional parameters.
#' @return a plot.
#'
#'
#'@details The date-time data in the data frame dat will keep time zone.
#' The argument tz and tz2 are used to specify the time in the x-axis.
#'
#' @examples
#' library("dplyr")
#' data(act)
#' act[act$subject_ID == 1 & act$interval_type != "ACTIVE",] %>%
#' plot_long(., si = c(3, 2.5, 2, 1.5),
#'              tz2 = "Pacific/Auckland",
#'              acolor = c("#D55E00", "black", "#56B4E9"))


#' @export
#' @importFrom dplyr distinct
#' @importFrom scales date_breaks date_format
#' @importFrom lubridate seconds is.POSIXct floor_date tz ceiling_date
#' @importFrom tidyr unnest
#' @import ggplot2
#' @rdname plot_long

plot_long <- function(dat, acolor, si, tz = "UTC", tz2, sp = "00:00:00", with_date = FALSE, alphas = 1, hourbreaks = 5, ...) {
  sinceMidnight_end <- sinceMidnight_start <- interval_type <- dat3 <- dat4 <- dat5 <- dat6 <- NULL

    dat <- dat[!is.na(dat$datime_start),]
  dat <- dat[!is.na(dat$datime_end),]
  dat <- arrange(dat, interval_type)

  dat$subject_ID <- gsub("_.*$", "", dat$subject_ID)

  if(is.POSIXct(dat$datime_start) == FALSE) {dat$datime_start <- as.POSIXct(dat$datime_start, tz = tz)}
  if(is.POSIXct(dat$datime_end) == FALSE) {dat$datime_end = as.POSIXct(dat$datime_end, tz = tz)}

  # make time into mins since midnight - make the sleeps that cross midnight plot across 2 lines
  dat$trun_start <- as.POSIXct(paste(trunc(dat$datime_start, "day"), sp),format = "%Y-%m-%d %H:%M:%S", tz = tz(dat$datime_start))
  dat$trun_end <- as.POSIXct(paste(trunc(dat$datime_end, "day"), sp),format = "%Y-%m-%d %H:%M:%S", tz = tz(dat$datime_start))

  # extract the date and make into factor counter: This is not working for cases where the REST/SLEEP occurs after midnight
  df_seq <- data.frame(seqDates = seq.POSIXt(min(dat$trun_start), max(dat$trun_end ),  by = "day"),
                       sequence = 1 : length(seq.POSIXt(min(dat$trun_start), max(dat$trun_end),  by = "day")))

  dat <- dat %>% left_join(df_seq, c("trun_start" = "seqDates"))

  dat$sinceMidnight_start <- as.numeric(difftime(dat$datime_start, dat$trun_start , units = "mins"))
  dat$sinceMidnight_end   <- as.numeric(difftime(dat$datime_end, dat$trun_end , units = "mins")) #trunc(dat$datime_end,   "day")

  dat[dat$sinceMidnight_start < 0,]$sequence <- dat[dat$sinceMidnight_start < 0,]$sequence - 1

  # adjusting the negative intervals.
  if(nrow(filter(dat, sinceMidnight_start < 0)) > 0){
    dat2 <- filter(dat, sinceMidnight_start < 0)

    if(nrow(filter(dat2, sinceMidnight_end <= 0)) > 0) {
      dat3 <- filter(dat2, sinceMidnight_end <= 0)
      dat3$sinceMidnight_start <- dat3$sinceMidnight_start + 1440
      dat3$sinceMidnight_end <- dat3$sinceMidnight_end + 1440   }

    if(nrow(filter(dat2, sinceMidnight_end > 0)) > 0) {
      dat4 <- filter(dat2, sinceMidnight_end > 0)
      dat5 <- dat4
      dat5$sinceMidnight_start = 1440 + dat5$sinceMidnight_start
      dat5$sinceMidnight_end = 1440

      dat4$sinceMidnight_start <- 0
      dat4$sequence <- dat4$sequence + 1  }
    dat6 <- rbind(dat3, dat4, dat5)
    dat <- dat[dat$sinceMidnight_start >= 0,]
    dat <- rbind(dat, dat6)
  }

  dat[dat$sinceMidnight_start > 0 & dat$sinceMidnight_end < 0,]$sinceMidnight_end <- 1440 +
    dat[dat$sinceMidnight_start > 0 & dat$sinceMidnight_end < 0,]$sinceMidnight_end

  dat <- arrange(dat, interval_type)

  activ <- length(unique(dat$interval_type))
  if(missing(si)){ si = rep(1.25, activ)}

  dif_int <- setdiff(unique(dat$interval_type), c("ACTIVE" ,"EXCLUDED" ,"REST", "SLEEP" , "WORK", "SLEEP_B", "SLEEP_C", "DIARY", "BAD") )

  if(missing(acolor)) { # Defining the color
    match_color <- c("ACTIVE" = "#009E73", "EXCLUDED" = "#D55E00", "REST" =  "black",
                     "SLEEP" = "#56B4E9", "WORK" = "#CC79A7", "SLEEP_B" = "#f03b20", "SLEEP_C" = "#F0E442", "DIARY" = "#999999", "BAD" = "#D55E00")

    if(length(dif_int) > 0){
      cols_ext <- brewer.pal(length(dif_int) + 2, "Paired")#[1 : length(dif_int)]
      names(cols_ext) <- dif_int
      match_color <- c(match_color, cols_ext)
    }
    acolor = match_color
  }


  p <- ggplot(data = dat)


  # plot the x-midnight segments in 2 goes: one for the bit between sleep start and midnight
  if(nrow(dat %>% filter(sinceMidnight_start >= 0 & sinceMidnight_end > 0 & sinceMidnight_end < sinceMidnight_start)) > 0){
    p <- p + geom_segment(aes(colour = interval_type, x = sinceMidnight_start, xend = 1440, y = sequence, yend = sequence, size = interval_type), alpha = alphas, data = dat %>% filter(sinceMidnight_start >= 0 & sinceMidnight_end > 0 & sinceMidnight_end < sinceMidnight_start))

    p <- p + geom_segment(aes(colour = interval_type, x = 0, xend = sinceMidnight_end, y = sequence + 1, yend = sequence + 1, size = interval_type), alpha = alphas, data = dat %>% filter(sinceMidnight_start >= 0 & sinceMidnight_end > 0 & sinceMidnight_end < sinceMidnight_start))}

  if(nrow(dat %>% filter(sinceMidnight_start >= 0 & sinceMidnight_end > 0 & sinceMidnight_end > sinceMidnight_start)) > 0){
    p <- p + geom_segment(aes(colour = interval_type, x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence, size = interval_type, alpha = alphas), alpha = alphas , data = dat %>% filter(sinceMidnight_start >= 0 & sinceMidnight_end > 0 & sinceMidnight_end > sinceMidnight_start))}


  p <- p + scale_size_manual(values = si)
  # flip it upside down so newest dates are on the top of the figure
  p <- p + theme_bw()
  p <- p + ggtitle(gsub("_.*$", "", dat$subject_ID))
  p <- p + labs(x = "Time", y = "Days")

  b <-  seq.POSIXt(dat$trun_start[1], dat$trun_start[1] + hours(24),  length.out = hourbreaks) #c("00:00", "06:00", "12:00", "18:00", "24:00")
  b <- format(b, format="%H:%M", usetz = TRUE, tz = tz)

  if(missing(tz2)){labs = b}
  else{ b2 <- strptime(paste("01-01-2000", b), format= "%d-%m-%Y %H:%M", tz = tz) #"EST"
  b2EST <- with_tz(b2, tzone = tz2)
  hmEST <- format(b2EST, format="%H:%M", usetz = TRUE)
  labs <- paste(b, hmEST, sep = "\n")

  }

  p <- p + scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 360),
                              expand = c(0, 0),labels = labs)

  # adding date info in y axis
  bys <- unique(df_seq$sequence)
  if(with_date==FALSE ){labs2  = bys}
  else{labs2 <- paste(bys, unique(df_seq$seqDates), sep = " ") }


  p <- p + scale_y_reverse(breaks = unique(df_seq$sequence) , labels = labs2)
  p <- p + scale_color_manual(values = acolor)

  p <- p + theme(
    strip.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(5.5, 12.5, 5.5, 5.5), "points"),
    axis.title.y = element_text(size = 10, colour = "black", face = "bold", margin = margin(0, 0, 0, 0, unit = "points")),
    axis.title.x = element_text(size = 10, colour = "black", face = "bold", margin = margin(10, 0, 0, 0, unit = "points")),
    axis.text  = element_text(size = 10, colour = "black", face = "bold"))

  p

}


