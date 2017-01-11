#' @title Create a chart of the Real Percentage Change of GDP in the Year time series
#' 
#' @description  Creates a plot of series 7326
#' 
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

draw.vargdp = function(file, start = NULL, ylim = NULL, open = TRUE){
  
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
  
  vargdp = BETS.get(7326)
  trend = fitted(hpfilter(vargdp))
  
  if(!is.null(start)){
    vargdp = window(vargdp, start = start)
    trend = window(trend, start = start)
  }
  
  if(is.null(ylim)){
    ylim = c(min(vargdp)-1,max(vargdp)+1)
  }
  
  years = as.Date(vargdp)
  dt = years[length(vargdp)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  barplot(as.vector(vargdp), names.arg = as.character(1994:2015), xlab = "", ylab = "", main = "Real GDP", col = "chocolate1", ylim = ylim)
  par(new = TRUE)
  plot(trend, lty = 6, col = "darkgray", lwd = 2, xaxt="n",yaxt = "n",xlab = "",ylab = "", ylim = ylim)
  mtext("Percentage Change in the Year")
  
  val = round(vargdp[length(vargdp)],2)
  y0 = ylim[1] + 0.2*y.spam
  
  text(2014.5, y0 - 0.2*y.spam, as.character(val), cex = 1.1, font = 2)
  legend("topleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}