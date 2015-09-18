# stsaav-package.R

#' Standard Time Series Analysis and Visualization.
#'
#' Simplified analysis and display of standardizedtime series string modeled
#' after examples found here 
#' \url{http://www.st.nmfs.noaa.gov/plankton/time-series-methods/index.html}
#'
#' @name stsaav-package
#' @docType package
#' @import methods
NULL

#' Airport weather data at Narsarsuaq, Greenland
#'
#' A 24 variable 6498 record data frame of daily weather observations.
#'
#' Source?????
#'
#' \itemize{
#'    \item{Date POSIXct} 
#'    \item{Max.TemperatureF integer} 
#'    \item{Mean.TemperatureF integer} 
#'    \item{Min.TemperatureF integer} 
#'    \item{Max.Dew.PointF integer} 
#'    \item{MeanDew.PointF integer} 
#'    \item{Min.DewpointF integer} 
#'    \item{Max.Humidity integer} 
#'    \item{Mean.Humidity integer} 
#'    \item{Min.Humidity integer} 
#'    \item{Max.Sea.Level.PressureIn numeric} 
#'    \item{Mean.Sea.Level.PressureIn numeric} 
#'    \item{Min.Sea.Level.PressureIn numeric} 
#'    \item{Max.VisibilityMiles integer} 
#'    \item{Mean.VisibilityMiles integer} 
#'    \item{Min.VisibilityMiles integer} 
#'    \item{Max.Wind.SpeedMPH integer} 
#'    \item{Mean.Wind.SpeedMPH integer} 
#'    \item{Max.Gust.SpeedMPH integer} 
#'    \item{PrecipitationIn numeric} 
#'    \item{CloudCover integer} 
#'    \item{Events factor} 
#'    \item{WindDirDegrees integer} 
#'    \item{Fog logical} 
#' }
"airport_wx"
