% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{woy}
\alias{woy}
\title{Converts Date values to week-of-the-year 
 
Values may range from 1-53 unless coerce is TRUE (the default)
in which case will coerce the oddball end-of-year days to belong
to the 52 week}
\usage{
woy(x = Sys.Date(), coerce = TRUE)
}
\arguments{
\item{x}{The date values as POSIXct or Date class}

\item{coerce}{logical, if TRUE for weeks into 1-52}
}
\value{
numeric week of year (either 1-53 or 1-52)
}
\description{
Converts Date values to week-of-the-year 
 
Values may range from 1-53 unless coerce is TRUE (the default)
in which case will coerce the oddball end-of-year days to belong
to the 52 week
}
