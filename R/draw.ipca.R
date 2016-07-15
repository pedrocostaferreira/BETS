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

draw.ipca = function(file){
  
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
  
  png(file,width=628,height=378, pointsize = 15) 
  
  ipca = BETSget(13522)
  ipca = window(ipca, start = c(2006,1))
  
  core = BETSget(4466)
  core_acc = vector(mode = "numeric")
  
  for(i in 12:length(core)){
    sum = 0 
    for(j in 1:11){
      sum = sum + core[i-j]
    }
    core_acc[i-11] = sum 
  }
  
  core_acc = ts(core_acc, start = c(1996,12), frequency = 12)
  core = window(core_acc, start = c(2006,1))
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot(ipca, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "National Consumer Price Index (IPCA)", col = "firebrick4", ylim = c(2,11), xlim = c(2006,2016))
  mtext("Cumulative 12-Month Percentage")
  
  par(new = TRUE)
  plot(core, lwd = 2.5, lty = 2, xlab = "", ylab = "", col = "firebrick3", ylim = c(2,11), xlim = c(2006,2016))
  
  end.x = end(ipca)[1]
  start.x = start(ipca)[1]
  d.x = end(ipca)[2]/12 
  
  last.core = core[length(core)]
  last.ipca = ipca[length(ipca)]
  
  par(new = TRUE)
  points(end.x + d.x, last.core, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick3", col = "darkgray")
  points(end.x + d.x, last.ipca, pch = 21, cex = 1.25, lwd = 2, bg = "firebrick4", col = "darkgray")
  
  x0 = end.x - 1
  x1 = end.x + d.x 
  y0 = 11
  y1 = last.ipca
  
  tan = (y1 - y0)/(x1 - x0)
  mod = sqrt((y1 - y0)^2 + (x1 - x0)^2)
  func = function(x) (1 + tan^2)*(x - x0)^2 - (mod^2)*0.75
  
  x1.ipca = uniroot(func, c(2014,x1))$root
  y1.ipca = tan*(x1.ipca - x0) + y0
  
  arrows(x0 = x0, x1 = x1.ipca, y0 = y0, y1 = y1.ipca, length = c(0.1, 0.1), lwd = 2)
  
  x1 = end.x + d.x 
  y0 = min(core)
  y1 = last.core
  
  tan = (y1 - y0)/(x1 - x0)
  mod = sqrt((y1 - y0)^2 + (x1 - x0)^2)
  func = function(x) (1 + tan^2)*(x - x0)^2 - (mod^2)*0.9
  
  x1.core = multiroot(func, start = 2015)$root
  y1.core = tan*(x1.core - x0) + y0
  
  arrows(x0 = x0, x1 = x1.core, y0 = y0, y1 = y1.core, length = c(0.1, 0.1), lwd = 2)
  
  text(x0 - 0.6, 11, as.character(last.ipca), cex = 1.1, font = 1)
  text(x0 - 0.6, min(core), as.character(last.core), cex = 1.1, font = 1)
  legend("topleft", c("Full", "Core"), lty=c(1,1), lwd=c(2.5,2.5),col=c("firebrick4", "firebrick3"), bty = "n", cex = 0.9)
  
  abline(a = 4.5, b = 0, lty = 3, lwd = 2.5, col = "darkgray")
  text(end.x - 0.2, 4.1, "Target", cex = 0.9)
  
  par(xpd=NA)
  points(x = 2008, y = -1, pch = 17, col = "blue", bg = "blue", cex = 1.5)
  
  dev.off()
  
  file.show(file)
}