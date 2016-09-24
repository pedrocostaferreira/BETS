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
#' @importFrom zoo zooreg

draw.selic = function(file, start = NULL, ylim = NULL, open = TRUE){
  
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
  
  selic = BETS.get(4189)
  
  #target = read.csv2("inst//target_selic.csv", stringsAsFactors = F)
  #d = as.Date("2000-09-25")
  #z <- zooreg(target[,2], start = d, frequency = 1)
  #window(z, start = as.Date("2006-01-01"))
  
  if(is.null(ylim)){
    ylim = c(min(selic)-5,max(selic)+5)
  }
  
  if(!is.null(start)){
    selic = window(selic, start = start)
  }
  
  dt = as.Date(selic)[length(selic)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(selic, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Base Interest Rate (SELIC)", col = "darkolivegreen", ylim = ylim)
  #lines(trend, lty = 6, col = "darkgray", lwd = 2)
  mtext("Accumulated in the Month in Annual Terms")
  
  end.x = last[1]
  d.x = last[2]/12 
  val = round(selic[length(selic)],2)
  
  points(end.x + d.x, val, pch = 21, cex = 1.25, lwd = 2, bg = "royalblue", col = "darkgray")
  legend("topleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
  text(start[1] + 0.14*x.spam, ylim[2] - 0.1*y.spam, aval, cex = 0.9)
  
  x1 = end.x + d.x 
  y0 = ylim[1] + 0.2*y.spam
  y1 = val - 0.028*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.005*x.spam, y0 - 0.067*y.spam, as.character(val), cex = 1.1, font = 2)
  
  add.notes(selic, ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}