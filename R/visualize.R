# visualize.R

#' Plot the seasonal anomaly
#' 
#' @export
#' @param x a stsaav object
#' @param label_fmt character the formating specification for the color bar
plot_t_anom = function(x,
   label_format = "%0.2f",
   main = pretty_main(x$t_step, what = 'Anomalies')){
   
   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))
   
   NC <- 10
   bar <- c(
       "royalblue4",
       "lightskyblue4",  "lightskyblue3",  "lightskyblue2", "lightskyblue1",
       "white",
       "palevioletred1", "palevioletred2", "palevioletred3", "palevioletred4",
       "red4")

   #cbar <- colorRampPalette(c( "#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
   cbar <- colorRampPalette(bar)
   image(x$t_x, x$t_y, t(x$t_dep), col = cbar(NC),
      xlab = labs[2], ylab = labs[1], main = main)
   grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
   box()
   bar <- matrix(cbar(NC), ncol = NC, nrow = 2, byrow = TRUE)
   rx <- range(x$t_x)
   ry <- range(x$t_y)
   rz <- range(x$t_dep, na.rm = TRUE)
   w <- diff(rx)
   h <- diff(ry)
   par(xpd = NA)
   rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
      interpolate = FALSE)
   text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
      labels = sprintf("%0.2f", rz), cex = 0.9, adj = c(0.5, 0.5))
   par(xpd = opar[['xpd']])
   
} # plot_t_anom



#' Plot the seasonal anomaly
#' 
#' @export
#' @param x a stsaav object
#' @param n_sd numeric vector, number of standard deviation steps above/below
#'    by default we use -3 to +3 
#' @param label_fmt character the formating specification for the color bar
plot_t_ranked_anom = function(x,
   n_sd = c(-3, 3),
   label_format = "%0.0f",
   main = pretty_main(x$t_step, what = 'Ranked Anomalies')){
    
   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))
   
   
   bar <- c(
       "lightskyblue4",  "lightskyblue3",  "lightskyblue2", "lightskyblue1",
       "palevioletred1", "palevioletred2", "palevioletred3", "palevioletred4")
   sd_steps <- seq(from = n_sd[1], to = n_sd[2], by = 1)
   
   if (length(bar) != (length(sd_steps) + 1)){
      bar <- colorRampPalette(bar)(length(sd_steps) + 1)
   }
   NC <- length(bar)
   
   z <- matrix(findInterval(x$t_rdep, sd_steps) + 1,
      ncol = ncol(x$t_rdep), nrow = nrow(x$t_rdep))
   image(x$t_x, x$t_y, t(z), col = bar,
      xlab = labs[2], ylab = labs[1], 
      main = main)
   grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
   box()
   par(xpd = NA)
   bar <- matrix(bar, ncol = NC, nrow = 2, byrow = TRUE)
   rx <- range(x$t_x)
   ry <- range(x$t_y)
   rz <- range(sd_steps)
   w <- diff(rx)
   h <- diff(ry)
   par(xpd = NA)
   rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
      interpolate = FALSE)
   text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
      labels = sprintf("%0.0f sd", rz), cex = 0.9, adj = c(0.5, 0.5))
   par(xpd = opar[['xpd']])

   
} # plot_t_anom


#' Plot the seasonal cycle.  Continuous variables are plotted as boxplots while
#' binary variables are plotted as sums.
#' @export
#' @param x a stsaav object
#' @param continuous logical TRUE unless dealing with logical inputs. 
#' @param main character, by default 'Season Cycle'
plot_t_cycle <- function(x, 
   continuous = is_continuous(x),
   main = pretty_main(x$t_step, what = 'Cycle')){

   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))
      
   if (continuous){
   
      boxplot( as.numeric(x$data[,vcol]) ~ as.numeric(x$data[,x$t_step]),
         main = main, col = "green", 
         horizontal = TRUE,
         xlab = x$t_step, ylab = vcol)
   
   } else {
   
      par(las = 1)
      z <- as.data.frame(t(x$t_mean))
      boxplot(z,
         horizontal = TRUE,
         #names.arg = names(x$t_avg),
         #xlim = c(0,1),
         col = 'green',
         ylab = x$t_step, xlab = vcol,
         main = main)
   }
   
   par(las = opar[['las']])

} # plot_t_cycle


#' Plot the seasonal cycle.  Continuous variables are plotted as boxplots while
#' binary variables are plotted as sums.
#' @export
#' @param x a stsaav object
#' @param main the title
plot_a_anom <- function(x,
   main = pretty_main(x$t_step, what = 'Anomalies')){

   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))
   
      # bottom left annual anomalies
   aa <- x$ann_dep
   plot(as.numeric(names(aa)), aa, 
      typ = 'h', lend = 'butt', lwd = 5,
      col =  c('blue', 'red')[(aa > 0)+1],
      xlab = 'Year', ylab = vcol,
      main = main)  

}

#' Plot the sample density. 
#' @export
#' @param x a stsaav object
#' @param main the title
plot_sample_density <- function(x,
   main = pretty_main(x$t_step, what='Sample Density') ){

   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))
      
      # bottom right - sample density
   NC <- 9
   bar <- RColorBrewer::brewer.pal(9, "YlOrRd")
   image(x$t_x, x$t_y, t(x$t_n), col = bar,
      xlab = labs[2], ylab = labs[1], 
      main = main)
   grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
   box()
   par(xpd = NA)
   bar <- matrix(bar, ncol = NC, nrow = 2, byrow = TRUE)
   rx <- range(x$t_x)
   ry <- range(x$t_y)
   rz <- range(x$t_n, na.rm = TRUE)
   w <- diff(rx)
   h <- diff(ry)
   par(xpd = NA)
   rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
      interpolate = FALSE)
   text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
      labels = sprintf("%0.0f", rz), cex = 0.9, adj = c(0.5, 0.5))
   par(xpd = opar[['xpd']])
}



#' Plot the timeseries.  Continuous variables are plotted as is while
#' binary variables are plotted as t_step means.
#' @export
#' @param x a stsaav object
#' @param continuous logical TRUE unless dealing with logical inputs. 
#' @param main character, the title
plot_t_series <- function(x,
   continuous = is_continuous(x),
   main = 'Time Series'){
 
   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))  
   
   
   if (continuous){
   
      plot(x$data[,tcol], x$data[,vcol],  
         xlab = 'Date', ylab = vcol,
         main = main,
         cex = 0.6)
      abline(lm(x$data[,vcol] ~ x$data[,tcol]),col="red",lty="dashed",lwd=2)  
       
   } else {
      
      
      yy <- as.vector(x$t_mean)
      years <- colnames(x$t_mean)
      tval <- rownames(x$t_mean)
      xx <- switch(tolower(x$t_step),
      
         'day' = sapply(years, 
            function(y, tval = '001'){ 
               as.POSIXct(paste(y, tval, sep = "-"), format = '%Y-%j')
               },
            tval = tval),
            
         'month' = sapply(years,
            function(y, tval = '01') { 
               as.POSIXct(paste(y, tval,'15', sep = "-"), format = '%Y-%m-%d')
               },
            tval = tval),
         
         'week' = sapply(years,
            function(y, tval = '01'){
               as.POSIXct(paste(y, (as.numeric(tval) - 1) * 7 + 3.5, sep = "-"), format = '%Y-%j')
               },
               tval = tval)
         )
      xx <- as.POSIXct(as.vector(xx), origin = '1970-01-01')
      
      plot(xx, yy,  
         xlab = 'Date', ylab = vcol,
         main = main,
         cex = 0.6)
      abline(lm(yy ~ xx),col="red",lty="dashed",lwd=2)   

   }

   
} # plot_t_series



#' Generate a pretty title.
#'
#' @export
#' @param x either character (Day, Month or Year) or an stsaav class object
#' @param character, by default 'Anomalies' but could be anything
#' @return character such as 'Daily Anomalies'
pretty_main <- function(x, 
   what = c("Anomalies", 'Ranked Anomalies', 'Cycle')[1]){
   
   if (inherits(x, "stsaav")) x <- x$t_step
   
   pre <- switch(tolower(x),
      'day' = 'Daily',
      'month' = 'Monthly',
      'year' = 'Yearly',
      'Seasonal')
      
   paste(pre, what)
}



#' Plot a stsaav object
#' @export
#' @param x a stsaav object
#' @param main character title, if null then not displayed
plot.stsaav <- function(x, 
   main = NULL){
   
   stopifnot(inherits(x, 'stsaav'))
   opar <- par(no.readonly = TRUE)
   on.exit(par(opar))
   par(mfrow=c(3,2), mgp = c(2,1,0))
   if (!is.null(main)) par(oma = c(0,0,3,0))
   tcol <- x$xycol[['tcol']]
   vcol <- x$xycol[['vcol']]
   labs  = names(dimnames(x$t_sum))

   plot_t_anom(x)
   plot_t_cycle(x)
   plot_t_ranked_anom(x)
   plot_t_series(x)
   plot_a_anom(x)
   plot_sample_density(x)

   if (!is.null(main)) {
      par(xpd = NA)
      xt <- grconvertX(0.5, from = 'ndc', to = 'user')
      yt <- grconvertY(0.98, from = 'ndc', to = 'user')
      text(xt, yt, main, cex = 1.5, adj = c(0.5, 0.5), font = 2)
   }
   invisible(NULL)
}

#   #' Plot a stsaav object
#   #' @export
#   #' @param x a stsaav object
#   #' @param main character title, if null then not displayed
#   plot_stsaav_old <- function(x, 
#      main = NULL){
#      
#      stopifnot(inherits(x, 'stsaav'))
#      opar <- par(no.readonly = TRUE)
#      on.exit(par(opar))
#   
#      par(mfrow=c(3,2), mgp = c(2,1,0))
#      if (!is.null(main)) par(oma = c(0,0,3,0))
#      tcol <- x$xycol[['tcol']]
#      vcol <- x$xycol[['vcol']]
#      labs  = names(dimnames(x$t_sum))
#   
#      # Top left Anomalies
#      NC <- 10
#      cbar <- colorRampPalette(c( "#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
#      image(x$t_x, x$t_y, t(x$t_dep), col = cbar(NC),
#         xlab = labs[2], ylab = labs[1], main = paste(paste0(x$t_step, "ly"),'Anomalies'))
#      grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
#      box()
#      bar <- matrix(cbar(NC), ncol = NC, nrow = 2, byrow = TRUE)
#      rx <- range(x$t_x)
#      ry <- range(x$t_y)
#      rz <- range(x$t_dep, na.rm = TRUE)
#      w <- diff(rx)
#      h <- diff(ry)
#      par(xpd = NA)
#      rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
#         interpolate = FALSE)
#      text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
#         labels = sprintf("%0.2f", rz), cex = 0.9, adj = c(0.5, 0.5))
#      par(xpd = opar[['xpd']])
#      
#      # Top Right Seasonal plot 
#      boxplot( as.numeric(x$data[,vcol]) ~ as.numeric(x$data[,x$t_step]),
#         main = "Seasonal cycle", col = "green",
#         xlab = x$t_step, ylab = vcol)
#   
#      # middle left  departures from means
#      NC <- 6
#      bar <- c("blue","lightskyblue", 'lightskyblue1', "palevioletred1", "palevioletred", "red")
#      z <- matrix(findInterval(x$t_rdep, c(-3,-2, -1, 0, 1, 2), all.inside = TRUE),
#         ncol = ncol(x$t_rdep), nrow = nrow(x$t_rdep))
#      image(x$t_x, x$t_y, t(z), col = bar,
#         xlab = labs[2], ylab = labs[1], 
#         main = paste(paste0(x$t_step, "ly"),'Ranked Anomalies'))
#      grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
#      box()
#      par(xpd = NA)
#      bar <- matrix(bar, ncol = NC, nrow = 2, byrow = TRUE)
#      rx <- range(x$t_x)
#      ry <- range(x$t_y)
#      rz <- c(-3, 3)
#      w <- diff(rx)
#      h <- diff(ry)
#      par(xpd = NA)
#      rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
#         interpolate = FALSE)
#      text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
#         labels = sprintf("%0.0f sd", rz), cex = 0.9, adj = c(0.5, 0.5))
#      par(xpd = opar[['xpd']])
#      
#      # middle right time series
#      plot(x$data[,tcol], x$data[,vcol],  
#         xlab = 'Date', ylab = vcol,
#         main = 'Time Series',
#         cex = 0.6)
#      abline(lm(x$data[,vcol] ~ x$data[,tcol]),col="red",lty="dashed",lwd=2)
#   
#      # bottom left annual anomalies
#      aa <- x$ann_dep
#      plot(as.numeric(names(aa)), aa, 
#         typ = 'h', lend = 'butt',, lwd = 4,
#         col =  c('blue', 'red')[(aa > 0)+1],
#         xlab = 'Year', ylab = vcol,
#         main = 'Annual Anomalies')  
#         
#      # bottom right - sample density
#      NC <- 9
#      bar <- RColorBrewer::brewer.pal(9, "YlOrRd")
#      image(x$t_x, x$t_y, t(x$t_n), col = bar,
#         xlab = labs[2], ylab = labs[1], 
#         main = paste(paste0(x$t_step, "ly"),'Sampling Density'))
#      grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
#      box()
#      par(xpd = NA)
#      bar <- matrix(bar, ncol = NC, nrow = 2, byrow = TRUE)
#      rx <- range(x$t_x)
#      ry <- range(x$t_y)
#      rz <- range(x$t_n, na.rm = TRUE)
#      w <- diff(rx)
#      h <- diff(ry)
#      par(xpd = NA)
#      rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
#         interpolate = FALSE)
#      text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
#         labels = sprintf("%0.0f", rz), cex = 0.9, adj = c(0.5, 0.5))
#      par(xpd = opar[['xpd']])
#   
#      if (!is.null(main)) {
#         par(xpd = NA)
#         xt <- grconvertX(0.5, from = 'ndc', to = 'user')
#         yt <- grconvertY(0.98, from = 'ndc', to = 'user')
#         text(xt, yt, main, cex = 1.5, adj = c(0.5, 0.5), font = 2)
#      }
#      invisible(NULL)
#   }
