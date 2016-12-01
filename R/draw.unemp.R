#' @title Create a chart of the Open Unemployment Rate time series
#' 
#' @description  Creates a plot of series 10777
#' @param file A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory. 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param open A \code{boolean}. Indicates whether the plot must be opened after being created.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' 
#' @importFrom mFilter hpfilter

draw.unemp = function(file, start = NULL, ylim = NULL, open = TRUE){
  
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
  
  if(grepl("\\.png", file)){
    png(file,width=728,height=478, pointsize = 15) 
  }
  else {
    pdf(file, width = 8.0, height = 5.3)
  }
  
  unemp = BETS.get(10777)
  trend = fitted(hpfilter(unemp))
  
  if(!is.null(start)){
    unemp = window(unemp, start = start)
    trend = window(trend, start = start)
  }
  
  if(is.null(ylim)){
    ylim = c(min(unemp)-5,max(unemp)+5)
  }
  
  dt = as.Date(unemp)[length(unemp)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(unemp, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Open Unemployment Rate", col = "royalblue", ylim = ylim)
  lines(trend, lty = 6, col = "darkgray", lwd = 2)
  mtext("Metropolitan Regions")
  
  end.x = last[1]
  d.x = last[2]/12 
  val = round(unemp[length(unemp)],2)
  
  points(end.x + d.x, val, pch = 21, cex = 1.25, lwd = 2, bg = "royalblue", col = "darkgray")
  legend("topleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
  text(start[1] + 0.14*x.spam, ylim[2] - 0.1*y.spam, aval, cex = 0.9)
  
  x1 = end.x + d.x 
  y0 = ylim[1] + 0.2*y.spam
  y1 = val - 0.028*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.005*x.spam, y0 - 0.067*y.spam, as.character(val), cex = 1.1, font = 2)
  
  add.notes(unemp, ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}