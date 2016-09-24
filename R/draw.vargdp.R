#' @title  xxx
#' 
#' @description  xxxx
#' 
#' @param ts xxx
#' @param lag.max xxx
#' @param mode xxx
#' @param ci xxx 
#' 
#' @return xxx
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
  
  if(is.null(ylim)){
    ylim = c(min(vargdp)-1,max(vargdp)+1)
  }
  
  if(!is.null(start)){
    vargdp = window(vargdp, start = start)
    trend = window(trend, start = start)
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
  
  text(2014.5, y0 - 0.18*y.spam, as.character(val), cex = 1.1, font = 2)
  legend("topleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}