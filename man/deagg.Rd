% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deagg.R
\name{deagg}
\alias{deagg}
\title{Deaggregation of reports for summary_type sequential or daily.}
\usage{
deagg(period)
}
\arguments{
\item{period}{a dataframe containing participants, start and end date/time period}
}
\value{
a dataframe
}
\description{
Deaggregation of reports for summary_type sequential or daily.
}
\examples{
library("lubridate")
data("act")
par <- data.frame(subject_ID = 1,
                  summary_duration_h = 24,
                  summary_type = "sequential",
                  summary_start_datime = ymd_hms("2017-12-05 00:00:00 UTC"),
                  summary_end_datime = ymd_hms("2017-12-15 00:00:00 UTC"))
deagg(period = par)

}
