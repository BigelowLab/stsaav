# analyze.R

#' Converts Date values to week-of-the-year 
#'  
#'  Values may range from 1-53 unless coerce is TRUE (the default)
#'  in which case will coerce the oddball end-of-year days to belong
#'  to the 52 week
#'  NOTE: does not use %U or %W format intentionally
#'
#' @export
#' @param x The date values as POSIXct or Date class 
#' @param coerce logical, if TRUE for weeks into 1-52
#' @return numeric week of year (either 1-53 or 1-52)
woy <- function(x=Sys.Date(), coerce = TRUE) {
   stopifnot(inherits(x, 'POSIXt') || inherits(x, 'Date'))
  woy = (as.numeric(format.Date(x, "%j")) %/% 7) + 1
  if (coerce == TRUE)  woy[ woy > 52] <- 52
  return(woy)
}

#' Construct a stsaav class object from a data frame.
#'
#' @export
#' @param x a data.frame
#' @param what the name of the time interval for analysis
#' @param tcol character, the name of the time column
#' @param vcol character the name of the value column
#' @return a stsaav class object which contains
#' \itemize{
#'    \item{data the original data frame with Year, Month, Week and Day columns added}
#'    \item{xycol charcacter vector of tcol and vcol names}
#'    \item{ann_sum a vector of annual sums}
#'    \item{ann_mean a vector of annual means}
#'    \item{ann_dep a vector of annual deparature from the long term annual mean}
#'    \item{ann_rdep a vector of annual standardized departures as sd intervals}
#'    \item{t_step character, the name of the time step (Month, Week, Day)}
#'    \item{t_x vector of years}
#'    \item{t_y vector of time steps}
#'    \item{t_sum matrix of the sum of the 'value' by t_step and year}
#'    \item{t_n matrix of the observation counts}
#'    \item{t_mean matrix of the mean observations of 'value'}
#'    \item{t_sd matrix of the standard deviation of observations of 'value'}
#'    \item{t_dep matrix of the departure from mean for each time step each year}
#'    \item{t_rdep matrix of standardized departure from mean for each time step each year}
#'  }
stsaav <- function(x, what = c('Month', 'Week', 'Day')[1],
   tcol = 'Date', vcol = 'value'){
   
   stopifnot(tcol %in% colnames(x))
   stopifnot(vcol %in% colnames(x))
   what <- match.arg(what, c('Month', 'Week', 'Day'))
   
   Yjm <- do.call(rbind, strsplit(format(x[,tcol], "%Y-%j-%m"), "-"))
   colnames(Yjm) <- c("Year", "Day", "Month")
   x$Year <- sprintf("%4.4i", as.numeric(Yjm[,1]))
   x$Day <- sprintf("%3.3i", as.numeric(Yjm[,2]))
   x$Month <- sprintf("%2.2i", as.numeric(Yjm[,3]))
   x$Week <- sprintf("%2.2i", woy(as.POSIXct(x[,tcol])))
   Yjmw <- cbind(Yjm,'Week' = x[,'Week']) 
   Yjmwn <- matrix(as.numeric(Yjmw), ncol = ncol(Yjmw))
   colnames(Yjmwn) <- colnames(Yjmw)
   
   r <- apply(Yjmwn, 2, range)
   rn <- apply(r, 2, function(x) diff(x)+1 )
   
   
   t_sum <- switch(what,
      'Day' = matrix(0, ncol = rn['Year'], nrow = rn['Day'],
         dimnames = list(
            'Day' = sprintf("%3.3i", seq(from = r[1,'Day'], to = r[2,'Day'])), 
            'Year' = sprintf("%4.4i", seq(from = r[1,'Year'], to = r[2,'Year'])))
         ),
      'Month' = matrix(0, ncol = rn['Year'], nrow = rn['Month'],
         dimnames = list(
            'Month' = sprintf("%2.2i", seq(from = r[1,'Month'], to = r[2,'Month'])), 
            'Year' = sprintf("%4.4i", seq(from = r[1,'Year'], to = r[2,'Year'])))
         ),
      'Week' = matrix(0, ncol = rn['Year'], nrow = rn['Week'],
         dimnames = list(
            'Week' = sprintf("%2.2i", seq(from = r[1,'Week'], to = r[2,'Week'])), 
            'Year' = sprintf("%4.4i", seq(from = r[1,'Year'], to = r[2,'Year'])))
         )
      )

   # make a counter
   t_n <- t_sum
   
   # loop through summing and counting
   # not that we make sure that we only sum non-NA value
   ix <- which(!is.na(x[,vcol]))
   for (i in seq_along(ix)){
      t_sum[Yjmw[i,what], Yjmw[i,'Year']] <- t_sum[Yjmw[i,what], Yjmw[i,'Year']] + x[i, vcol]
      t_n[Yjmw[i,what], Yjmw[i,'Year']] <- t_n[Yjmw[i,what], Yjmw[i,'Year']] + 1
   }
   
   # now find the zero-counts and change them to NA
   ix <- t_n <= 0
   t_sum[ix] <- NA
   t_n[ix] <- NA
   
   # and here is the t_mean
   t_mean <- t_sum/t_n   # matrix
   t_vmean <- rowMeans(t_mean, na.rm = TRUE)  # vector
   t_vsd <- apply(t_mean, 1, sd, na.rm = TRUE)  # vector
   t_dep <- t_mean - t_vmean  # matrix
   t_rdep <- t_dep/t_vsd # matrix
   
   ann_sum <- apply(t_sum, 2, sum, na.rm = TRUE) 
   ann_n <- apply(t_n, 2, sum ,na.rm = TRUE)
   ann_mean <- ann_sum/ann_n
   ann_sd <- sd(ann_mean, na.rm = TRUE)
   ann_dep <- ann_mean - mean(ann_mean, na.rm = TRUE)
   ann_rdep <- ann_dep/ann_sd
   
   r <- list(
      # the data
      data = x,
      xycol = c(tcol = tcol, vcol = vcol), 
      
      # annual stuff
      ann_sum = ann_sum,   # vector
      ann_mean = ann_mean, # vector
      ann_sd = ann_sd,     # single number
      ann_dep = ann_dep,   # vector
      ann_rdep = ann_rdep, # vector
      
      # time step stuff
      t_step = what,                #character Month, Week, Day
      t_x = as.numeric(colnames(t_sum)),#time steps along years
      t_y = as.numeric(rownames(t_sum)),#time steps along t
      t_sum = t_sum,                # matrix - sum of 'value'
      t_n = t_n,                    # matrix - counts
      t_mean = t_mean,              # matrix - means of 'value'
      t_mean = t_vmean,             # vector - mean values by t
      t_sd = t_vsd,                 # vector - sd values by t
      t_dep = t_dep,                # matrix - departures
      t_rdep = t_rdep               # matrix - depatures in sd
      )
   
   class(r) <- 'stsaav'
   invisible(r)
}
