#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' @param file A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory. 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param open A \code{boolean}. Indicates whether the plot must be opened after being created.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off pdf png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text points mtext arrows
#' 
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
  
  if(!is.null(start)){
    selic = window(selic, start = start)
  }
  else{
    start = start(selic)
  }
  
  dt = as.Date(selic)[length(selic)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  
  # --- QUE COMPLICACAO
  target = read.csv2("target_selic.csv", stringsAsFactors = F)
  
  dates = vector(mode = "character")
  curr = start
  j = 1
  
  while(curr[1] != (last[1]+1)){
    
    for(i in curr[2]:12){
      
      month = i
      
      if(i < 10){
        month = paste0("0",i)
      }
      
      dates[j] = paste0(curr[1],"-",month,"-15")
      j = j + 1
    }
    
    curr[1] = curr[1] + 1
    curr[2] = 1
  }
  
  dates = as.Date(dates)
  
  zr = zooreg(target[,2], start = as.Date("2000-09-25"))
  zr.sub = subset(zr, time %in% dates)
  target = ts(zr.sub, start = start, frequency = 12)
  target = window(target, end = last)
  
  if(is.null(ylim)){
    ylim = c(min(selic)-2,max(selic)+2)
  }
  
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(selic, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Base Interest Rate (SELIC)", col = "darkolivegreen", ylim = ylim)
  lines(target, lty = 6, col = "darkgray", lwd = 1)
  mtext("Accumulated in the Month, in Annual Terms")
  
  end.x = last[1]
  d.x = last[2]/12 
  val = round(selic[length(selic)],2)
  
  points(end.x + d.x, val, pch = 21, cex = 1.25, lwd = 2, bg = "darkolivegreen", col = "darkgray")
  text(start[1] + 0.14*x.spam, ylim[2] - 0.07*y.spam, aval, cex = 0.85)
  
  x1 = end.x + d.x 
  y0 = ylim[1] + 0.2*y.spam
  y1 = val - 0.028*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.005*x.spam, y0 - 0.067*y.spam, as.character(val), cex = 1.1, font = 2)
  legend("topleft", "Target (15th of the Month)", lty = 6, lwd = 1, col="darkgrey", bty = "n", cex = 0.85)
  
  add.notes(selic, ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}