#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @param file A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory. 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param open A \code{boolean}. Indicates whether the plot must be opened after being created.
#' 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 

draw.ulc = function(file, start = NULL, ylim = NULL, open = TRUE){

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
  
  if(is.null(ylim)){
    ylim = c(70,180)
  }
  
  cut = BETS.get(11777)
  
  if(!is.null(start)){
    cut = window(cut, start = start)
  }
  
  dt = as.Date(cut)[length(cut)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(cut, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Unitary Labor Cost", col = "firebrick4", ylim = c(70,180), xlim = c(2006,2016))
  mtext("US$ - June 1994 = 100")
  
  end.x = last[1]
  d.x = last[2]/12 
  val = cut[length(cut)]
  
  points(end.x + d.x, val, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick4", col = "darkgray")
  text(start[1] + 0.14*x.spam, ylim[2] - 0.0165*y.spam, aval, cex = 0.9)
  
  x1 = end.x + d.x 
  y0 = 0.75*ylim[2]
  y1 = val + 0.028*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.03*x.spam, y0 + 0.067*y.spam, as.character(val), cex = 1.1, font = 2)
  
  add.notes(cut, ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}