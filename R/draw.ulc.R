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
#' @importFrom rootSolve multiroot

draw.ulc = function(file){

  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
  
  png(file,width=628,height=378, pointsize = 15)
  
  cut = BETSget(11777)
  cut = window(cut, start = c(2005,1))
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(cut, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Unitary Labor Cost", col = "firebrick4", ylim = c(70,180), xlim = c(2006,2016))
  mtext("US$ - June 1994 = 100")
  
  end.x = end(cut)[1]
  d.x = end(cut)[2]/12 
  last = cut[length(cut)]
  
  points(end.x + d.x, last, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick4", col = "darkgray")
  
  x1 = end.x + d.x 
  y0 = 135
  y1 = last + 5
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.1, 0.1), lwd = 2)
  text(x1 - 0.3, y0 + 6, as.character(last), cex = 1.1, font = 1)
  
  add.notes(cut, ylim = c(70,180), xlim = c(2006,2016))
  
  dev.off()
  
  file.show(file)
}