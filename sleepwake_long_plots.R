###############################################
# Filename: sleepwake_long_plots.R
# Purpose:  Plot sleep and wake data (long) 
# Created:  2016-12-12 LJW
# Updated:  2016-12-12
###############################################

## load packages
library(ggplot2)
library(dplyr)

# import sleep/wake data
dat <- read.table(file = "C:\\Users\\lwu1\\Dropbox\\ActisoftR\\EXAMPLE_DATA\\example01_AMI.txt", sep = "\t", header = TRUE, skip = 0)
head(dat)
str(dat)
dat$subject_ID <- gsub("_.*$", "", dat$ID)
dat <- dat[dat$IntName == "O - O", ]
dat <- dat[!is.na(dat$IntNum), ] # get rid of the 2x summary lines

# make a datetime variable
dat$datime_start <- as.POSIXct(paste(as.POSIXct(strptime(dat$sdate, format = "%d/%m/%Y"), tz = "UTC"), dat$stime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dat$datime_end   <- as.POSIXct(paste(as.POSIXct(strptime(dat$edate, format = "%d/%m/%Y"), tz = "UTC"), dat$etime), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# extract the date and make into factor counter
dat$sequence <- as.numeric(factor(as.Date(dat$sdate)))

# make time into mins since midnight - make the sleeps that cross midnight plot across 2 lines
dat$sinceMidnight_start <- as.numeric(difftime(dat$datime_start, trunc(dat$datime_start, "day"), units = "mins"))
dat$sinceMidnight_end   <- as.numeric(difftime(dat$datime_end,   trunc(dat$datime_end,   "day"), units = "mins"))

head(dat)
str(dat)

# how many go across midnight? 17/34
library(dplyr)
dat %>% filter(sinceMidnight_end < sinceMidnight_start)

dat[1:14, c(4, 40:44)]

p <- ggplot(data = dat)
# plot only the easy ones
p <- p + geom_segment(aes(x = sinceMidnight_start, xend = sinceMidnight_end, y = sequence, yend = sequence), size = 1.25, data = dat %>% filter(sinceMidnight_end > sinceMidnight_start))
# plot the x-midnight segments in 2 goes: one for the bit between sleep start and midnight
p <- p + geom_segment(aes(x = sinceMidnight_start, xend = 1440, y = sequence, yend = sequence), size = 1.25, data = dat %>% filter(sinceMidnight_end < sinceMidnight_start))
# second bit for midnight until sleep offset
p <- p + geom_segment(aes(x = 0, xend = sinceMidnight_end, y = sequence + 1, yend = sequence + 1), size = 1.25, data = dat %>% filter(sinceMidnight_end < sinceMidnight_start))
# flip it upside down so newest dates are on the top of the figure
p <- p + scale_y_reverse()
p <- p + theme_bw()

p <- p + ggtitle("ID example01")
p <- p +labs(x = "Sleep", y = "Days")
p <- p + scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 360),
                            expand = c(0, 0), labels = c("00:00", "06:00", "12:00", "18:00", "00:00"))
p <- p + theme(
  strip.background = element_blank(),
  panel.grid.major.x = element_blank(), # no grids
  panel.grid.minor.x = element_blank(), # no grids
  plot.margin = unit(c(5.5, 12.5, 5.5, 5.5), "points"),
  axis.title.y = element_text(size = 10, colour = "black", face = "bold", margin = margin(0, 0, 0, 0, unit = "points")), # make x axis title pretty
  axis.title.x = element_text(size = 10, colour = "black", face = "bold", margin = margin(10, 0, 0, 0, unit = "points")), # make x axis title pretty
  axis.text  = element_text(size = 10, colour = "black", face = "bold")) # make y axis text pretty
p

# save an output
ggsave(filename = "C:\\Users\\lwu1\\Dropbox\\ActisoftR\\plots\\sleepwake_long.png", plot = p, width = 25, height = 20, units = c("cm") )


## add work periods

work_dat <- read.table(header = TRUE, sep = "\t",
                       text = "subject_ID	start_datime	end_datime
example01	4/07/2016 10:00	4/07/2016 18:00
example01	5/07/2016 10:00	5/07/2016 18:00
example01	6/07/2016 10:00	6/07/2016 18:00
example01	7/07/2016 10:00	7/07/2016 18:00
example01	8/07/2016 10:00	8/07/2016 18:00
example01	11/07/2016 10:00	11/07/2016 18:00
example01	12/07/2016 10:00	12/07/2016 18:00
example01	13/07/2016 10:00	13/07/2016 18:00
example01	14/07/2016 10:00	14/07/2016 18:00
example01	15/07/2016 10:00	15/07/2016 18:00
example01	18/07/2016 10:00	18/07/2016 18:00
example01	19/07/2016 10:00	19/07/2016 18:00
example01	20/07/2016 10:00	20/07/2016 18:00
example01	21/07/2016 10:00	21/07/2016 18:00
example01	22/07/2016 10:00	22/07/2016 18:00
example01	25/07/2016 10:00	25/07/2016 18:00
example01	26/07/2016 10:00	26/07/2016 18:00
example01	27/07/2016 10:00	27/07/2016 18:00
example01	28/07/2016 10:00	28/07/2016 18:00
example01	29/07/2016 10:00	29/07/2016 18:00")


str(work_dat)


# make a datetime variable
work_dat$start_datime <- as.POSIXct(work_dat$start_datime, format = "%d/%m/%Y %H:%M", tz = "UTC")
work_dat$end_datime   <- as.POSIXct(work_dat$end_datime, format = "%d/%m/%Y %H:%M", tz = "UTC")
work_dat$type <- as.factor("work")

head(work_dat)


# EOF