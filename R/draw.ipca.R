#' @title Create a chart of the National Consumer Price Index time series
#' 
#' @description  Creates a plot of series 13522 (NCPI), along with series 4466 (NCPI core)
#' 
#' @param file A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory. 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param open A \code{boolean}. Indicates whether the plot must be opened after being created.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' 
#' @importFrom grDevices dev.new dev.off pdf png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text points mtext arrows
#' 
#' @importFrom rootSolve multiroot

draw.ipca = function(file, start = NULL, ylim = NULL, open = TRUE){
  
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
  
  if(is.null(ylim)){
    ylim = c(2,11)
  }
  
  if(grepl("\\.png", file)){
    png(file,width=728,height=478, pointsize = 15) 
  }
  else {
    pdf(file, width = 8.0, height = 5.3)
  }
 
  ipca = BETS.get(13522)
  core = BETS.get(4466)
  
  if(is.null(start)){
    
    start = vector(mode = "logical")
    
    if(start(ipca) > start(core)){
      start = start(ipca)
    }
    else if(start(ipca) == start(core)) {
      start[1] = start(ipca)[1]
      start[2] = max(start(ipca)[2],start(core)[2])
    }
    else {
      start = start(core)
    }
  }
  
  ipca = window(ipca, start = start)
  
  core_acc = vector(mode = "numeric")
  
  for(i in 12:length(core)){
    sum = 0 
    for(j in 1:11){
      sum = sum + core[i-j]
    }
    core_acc[i-11] = sum 
  }
  
  core_acc = ts(core_acc, start = c(1996,12), frequency = 12)
  core = window(core_acc, start = start)
  
  dt = as.Date(ipca)[length(ipca)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1, mar=c(7.1,4.1,4.1,2.1))
  plot(ipca, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "National Consumer Price Index (IPCA)", col = "firebrick4", ylim = ylim, xlim = c(start[1],last[1]))
  mtext("Cumulative 12-Month Percentage")
  
  par(new = TRUE)
  plot(core, lwd = 2.5, lty = 2, xlab = "", ylab = "", col = "firebrick3", ylim = ylim, xlim = c(start[1],last[1]))
  
  last.x = last[1]
  start.x = start(ipca)[1]
  d.x = last[2]/12 
  
  last.core = core[length(core)]
  last.ipca = ipca[length(ipca)]
  
  par(new = TRUE)
  points(last.x + d.x, last.core, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick3", col = "darkgray")
  points(last.x + d.x, last.ipca, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick4", col = "darkgray")
  
  x0 = last.x - 1
  x1 = last.x + d.x 
  y0 = ylim[2]
  y1 = last.ipca
  
  tan = (y1 - y0)/(x1 - x0)
  mod = sqrt((y1 - y0)^2 + (x1 - x0)^2)
  func = function(x) (1 + tan^2)*(x - x0)^2 - (mod^2)*0.75
  
  x1.ipca = uniroot(func, c(2014,x1))$root
  y1.ipca = tan*(x1.ipca - x0) + y0
  
  arrows(x0 = x0, x1 = x1.ipca, y0 = y0, y1 = y1.ipca, length = c(0.01*x.spam, 0.011*y.spam), lwd = 2)
  
  x1 = last.x + d.x 
  y0 = min(core)
  y1 = last.core
  
  tan = (y1 - y0)/(x1 - x0)
  mod = sqrt((y1 - y0)^2 + (x1 - x0)^2)
  func = function(x) (1 + tan^2)*(x - x0)^2 - (mod^2)*0.9
  
  x1.core = multiroot(func, start = 2015)$root
  y1.core = tan*(x1.core - x0) + y0
  
  arrows(x0 = x0, x1 = x1.core, y0 = y0, y1 = y1.core, length = c(0.01*x.spam, 0.011*y.spam), lwd = 2)
  
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  text(x0 - 0.05*x.spam, ylim[2], as.character(last.ipca), cex = 1.1, font = 2)
  text(x0 - 0.05*x.spam, min(core), as.character(last.core), cex = 1.1, font = 2)
  
  legend("topleft", c("IPCA", "Core"), lty=c(1,1), lwd=c(2.5,2.5),col=c("firebrick4", "firebrick3"), bty = "n", cex = 0.9)
  text(start[1] + 0.14*x.spam, ylim[2] - 0.165*y.spam, aval, cex = 0.9)
  
  abline(a = 4.5, b = 0, lty = 3, lwd = 3, col = "darkgray")
  text(last.x - 0.02*x.spam, 0.455*y.spam, "Target", cex = 0.9)
  
  add.notes(list(ipca = ipca, core = core), names = c("IPCA","Core"), ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}