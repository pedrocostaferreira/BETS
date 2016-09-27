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
#' @import seasonal 


draw.indprod = function(file, start = NULL, ylim = NULL, open = TRUE){
  
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
  
  indprod = final(seas(BETS.get(21859)))
  
  if(!is.null(start)){
    indprod = window(indprod, start = start)
  }
  
  if(is.null(ylim)){
    ylim = c(min(indprod)-5,max(indprod)+5)
  }
 
  dt = as.Date(indprod)[length(indprod)]
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  
  x.spam = last[1] - start[1]
  y.spam = ylim[2] - ylim[1]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(indprod, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Industrial Production", col = "chocolate1", ylim = ylim)
  mtext("Seasonally Adjusted. Index (2012 = 100)")
  
  end.x = last[1]
  d.x = last[2]/12 
  val = round(indprod[length(indprod)],2)
  
  points(end.x + d.x, val, pch = 21, cex = 1.25, lwd = 2, bg = "chocolate1", col = "darkgray")
  text(start[1] + 0.14*x.spam, ylim[2] - 0.05*y.spam, aval, cex = 0.9)
  
  x1 = end.x + d.x 
  y0 = ylim[1] + 0.06*y.spam
  y1 = val - 0.03*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.005*x.spam, y0 - 0.05*y.spam, as.character(val), cex = 1.1, font = 2)
  
  add.notes(indprod, ylim = ylim, xlim = c(start[1],last[1]))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}