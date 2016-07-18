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
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


add.notes = function(series.list, xlim, ylim){
  
  par(xpd=NA)
  
  x.coords = vector(mode = "numeric")
  x.spam = xlim[2] - xlim[1]
  y.spam = ylim[2] - ylim[1]
  divs = length(series.list)*2 + 2
  dist = x.spam/divs
  
  for(i in 1:(divs-2)){
    x.coords[i] = xlim[1] + i*dist
  }
  
  y.coord = ylim[1] - (1/3)*y.spam
  j = 1
  
  for(i in 1:length(series.list)){
    
    series = series.list[[i]]
    len = length(series)
    freq = frequency(series)
    
    
    last.period = (series[i]/series[i-1] - 1)*100
    last.year = (series[i]/series[i-freq] - 1)*100
    
    points(x = x.coords[j], y = y.coord, pch = 23, col = "blue", bg = "blue", cex = 1.5)
    points(x = x.coords[j+1], y = y.coord, pch = 24, col = "red", bg = "red", cex = 1.5)
    
    j = j + 2
  }
}