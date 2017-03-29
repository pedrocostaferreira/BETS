#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom zoo zooreg
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.selic = function(start = NULL, ylim = NULL, xlim = NULL){
  
  selic = BETS.get(4189)
  
  if(!is.null(start)){
    selic = window(selic, start = start)
  }
  else{
    start = start(selic)
  }
  
  target = get.series.bacen(432)[[1]]
  target[,1] = as.Date(target[,1], format = "%d/%m/%Y")
  inx = grep("-15$",target[,1])
  first = target[1,1]
  target = ts(target[inx,2], start = as.numeric(c(format(first,"%Y"),format(first,"%m"))), frequency = 12)
  target = window(target, start = start, frequency = 12)
  
  lims = chart.add_basic(ts = selic, ylim = ylim, xlim = xlim, title = "Base Interest Rate (SELIC)", subtitle = "Accumulated in the Month, in Annual Terms", col = "darkolivegreen", arr.pos = "h", leg.pos = "none")
  chart.add_extra(target, ylim = lims[3:4], xlim = lims[1:2], arr.pos = "none", leg.pos = "none", col = "darkgray")
  legend("bottomleft", c("SELIC", "Target"), lty=c(1,2), lwd=c(2.5,2.5),col=c("darkolivegreen", "darkgray"), bty = "n", cex = 0.9)
  
  chart.add_notes(selic, ylim = lims[3:4], xlim = lims[1:2])
 
}