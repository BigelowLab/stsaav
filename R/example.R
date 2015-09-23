# example.R


#' Show an example output using the built-in dataset
#'
#' @export
#' @param x a data frame
#' @param t_step the time step to analyze
#' @param vcol the name of the variable column to analyze
#' @return a stsaav object
show_example <- function(x = airport_wx, t_step = 'Month', vcol = 'Fog'){
   r <- stsaav(x, tcol = 'Date', vcol = vcol, t_step = t_step)
   plot(r,main = sprintf('%s at Narsarsuaq, Greenland', vcol))
   invisible(r)
}