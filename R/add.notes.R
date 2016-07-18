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


add.notes = function(series.list, xlim, ylim, names = NULL){
  
  par(xpd=NA)
  
  if(class(series.list) == "ts"){
    series.list = list(series.list)
  }
  
  if(length(series.list) == 1){
    mg = 0
    divs = 3
  }
  else{
    if(is.null(names)){
      names = names(series.list)
    }
    mg = 1
    divs = length(series.list)*2 + 2 
  }
  
  x.coords = vector(mode = "numeric")
  x.spam = xlim[2] - xlim[1]
  y.spam = ylim[2] - ylim[1]
  
  dist = x.spam/divs
  
  for(i in 1:(divs-1)){
    x.coords[i] = xlim[1] + i*dist 
  }

  y.coord = ylim[1] - (1/3)*y.spam - mg 
  j = 1
  
  for(i in 1:length(series.list)){
    
    series = series.list[[i]]
    len = length(series)
    freq = frequency(series)
    
    last.period.val = paste0(round((series[len]/series[len-1] - 1)*100,2),"%")
    last.year.val = paste0(round((series[len]/series[len-freq] - 1)*100,2),"%")
    
    dt = as.Date(series)[len-1]
    last.period.comp = paste0(format(dt,"%b"),"/", format(dt,"%Y"),": ", series[len-1])
    last.year.comp = paste0("last ", format(as.Date(series)[len],"%b"), ": ", series[len-freq])
    
    d.lp = 0
    d.ly = 0
    
    if(nchar(last.period.val) == 4){
      d.lp = 0.15
    }

    if(nchar(last.year.val) == 4){
      d.ly = 0.15
    }
    
    if(last.period.val > 0){
      points(x = x.coords[j] + d.lp, y = y.coord - 0.2, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.period.val < 0){
     points(x = x.coords[j] + d.lp, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j] + d.lp, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.period.val, x = x.coords[j] + .65, y = y.coord, cex = 0.8)
    text(last.period.comp, x = x.coords[j] + .4, y = y.coord - 0.9, cex = 0.6)
    
    if(last.year.val > 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord - 0.2, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.year.val < 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j+1]+ d.ly, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.year.val, x = x.coords[j+1] + .65, y = y.coord, cex = 0.8)
    text(last.year.comp, x = x.coords[j+1] + .4, y = y.coord - 0.9, cex = 0.6)
    
    if(!is.null(names)){
      title.x = 0.5*x.coords[j] + 0.5*x.coords[j+1] + .4
      title.y = y.coord + mg
      text(names[i], x = title.x, y = title.y, cex = 0.8, font = 2)
    }
    
    j = j + 3
  }
}