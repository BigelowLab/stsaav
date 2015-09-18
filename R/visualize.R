# visualize.R

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

   # Top left Anomalies
   NC <- 10
   cbar <- colorRampPalette(c( "#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
   image(x$t_x, x$t_y, t(x$t_dep), col = cbar(NC),
      xlab = labs[2], ylab = labs[1], main = paste(paste0(x$t_step, "ly"),'Anomalies'))
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
   
   # Top Right Seasonal plot 
   boxplot( as.numeric(x$data[,vcol]) ~ as.numeric(x$data[,x$t_step]),
      main = "Seasonal cycle", col = "green",
      xlab = x$t_step, ylab = vcol)

   # middle left  departures from means
   NC <- 6
   bar <- c("blue","lightskyblue", 'lightskyblue1', "palevioletred1", "palevioletred", "red")
   z <- matrix(findInterval(x$t_rdep, c(-3,-2, -1, 0, 1, 2), all.inside = TRUE),
      ncol = ncol(x$t_rdep), nrow = nrow(x$t_rdep))
   image(x$t_x, x$t_y, t(z), col = bar,
      xlab = labs[2], ylab = labs[1], 
      main = paste(paste0(x$t_step, "ly"),'Ranked Anomalies'))
   grid(nx = length(x$t_x), ny = length(x$t_y), col = 'white', lwd = 2, lty = 'solid')
   box()
   par(xpd = NA)
   bar <- matrix(bar, ncol = NC, nrow = 2, byrow = TRUE)
   rx <- range(x$t_x)
   ry <- range(x$t_y)
   rz <- c(-3, 3)
   w <- diff(rx)
   h <- diff(ry)
   par(xpd = NA)
   rasterImage(bar, rx[1] + 0.1*w, ry[2] + 0.07*h, rx[2] - 0.1*w, ry[2] + 0.12*h,
      interpolate = FALSE)
   text(c(rx[1] + 0.05*w, rx[2] - 0.05*w), c(ry[2] + 0.1*h,ry[2] + 0.1*h), 
      labels = sprintf("%0.0f sd", rz), cex = 0.9, adj = c(0.5, 0.5))
   par(xpd = opar[['xpd']])
   
   # middle right time series
   plot(x$data[,tcol], x$data[,vcol],  
      xlab = 'Date', ylab = vcol,
      main = 'Time Series',
      cex = 0.6)
   abline(lm(x$data[,vcol] ~ x$data[,tcol]),col="red",lty="dashed",lwd=2)

   # bottom left annual anomalies
   aa <- x$ann_dep
   plot(as.numeric(names(aa)), aa, 
      typ = 'h', lend = 'butt',, lwd = 4,
      col =  c('blue', 'red')[(aa > 0)+1],
      xlab = 'Year', ylab = vcol,
      main = 'Annual Anomalies')  
      
   # bottom right - sample density
   NC <- 9
   bar <- RColorBrewer::brewer.pal(9, "YlOrRd")
   image(x$t_x, x$t_y, t(x$t_n), col = bar,
      xlab = labs[2], ylab = labs[1], 
      main = paste(paste0(x$t_step, "ly"),'Sampling Density'))
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

   if (!is.null(main)) {
      par(xpd = NA)
      xt <- grconvertX(0.5, from = 'ndc', to = 'user')
      yt <- grconvertY(0.98, from = 'ndc', to = 'user')
      text(xt, yt, main, cex = 1.5, adj = c(0.5, 0.5), font = 2)
   }
   invisible(NULL)
}
