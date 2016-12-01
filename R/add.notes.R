#' @title  Add notes
#' 
#' @description  Add notes
#' 
#' @param series.list A \code{ts object}
#' @param xlim A \code{vector}
#' @param ylim A \code{vector}
#' @param names A \code{character}
#' 
#' 
#' @importFrom graphics par points  text
#' @importFrom stats frequency
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


add.notes = function(series.list, xlim, ylim, names = NULL){
  
  par(xpd=NA)
  
  if(class(series.list) == "ts"){
    series.list = list(series.list)
  }
  
  x.spam = xlim[2] - xlim[1]
  y.spam = ylim[2] - ylim[1]
  
  if(length(series.list) == 1){
    mg = -0.025*y.spam
    divs = 3
  }
  else{
    if(is.null(names)){
      names = names(series.list)
    }
    mg = 0.11*y.spam
    divs = length(series.list)*2 + 2 
  }
  
  x.coords = vector(mode = "numeric")

  dist = x.spam/divs
  
  for(i in 1:(divs-1)){
    x.coords[i] = xlim[1] + i*dist 
  }

  y.coord = ylim[1] - (1/4)*y.spam - mg 
  j = 1
  
  for(i in 1:length(series.list)){
    
    series = series.list[[i]]
    len = length(series)
    freq = frequency(series)
    
    last.period.val = paste0(round((series[len]/series[len-1] - 1)*100,2),"%")
    last.year.val = paste0(round((series[len]/series[len-freq] - 1)*100,2),"%")
    
    if(freq != 365){
      dt.lp = as.Date(series)[len-1]
      dt.ly = as.Date(series)[len-freq]
      
      last.period.comp = paste0(format(dt.lp,"%b"),"/", format(dt.lp,"%Y"),": ", round(series[len-1],2))
      last.year.comp = paste0(format(dt.ly,"%b"),"/", format(dt.ly,"%Y"), ": ", round(series[len-freq],2))
    }
    else {
      last.period.comp = paste0("A day before: ", round(series[len-1],2))
      last.year.comp = paste0("A month before: ", round(series[len-30],2))
    }

    d.lp = 0
    d.ly = 0
    
    if(nchar(last.period.val) == 4){
      d.lp = 0.05*x.spam
    }

    if(nchar(last.year.val) == 4){
      d.ly = 0.05*x.spam
    }
    
    if(nchar(last.period.val) == 5){
      d.lp = 0.015*x.spam
    }
    
    if(nchar(last.year.val) == 5){
      d.ly = 0.015*x.spam
    }
    
    x.coords[j] = x.coords[j] - 0.01*x.spam
    x.coords[j + 1] = x.coords[j + 1] + 0.01*x.spam
    
    if(last.period.val > 0){
      points(x = x.coords[j] + d.lp, y = y.coord - 0.022*y.spam, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.period.val < 0){
     points(x = x.coords[j] + d.lp, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j] + d.lp, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.period.val, x = x.coords[j] + 0.075*x.spam, y = y.coord, cex = 1.1, font = 2)
    text(last.period.comp, x = x.coords[j] +  0.06*x.spam, y = y.coord - 0.1*y.spam, cex = 0.9)
    
    if(last.year.val > 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord - 0.022*y.spam, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.year.val < 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j+1]+ d.ly, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.year.val, x = x.coords[j+1] + 0.067*x.spam, y = y.coord, cex = 1.1, font = 2)
    text(last.year.comp, x = x.coords[j+1] + 0.06*x.spam, y = y.coord - 0.1*y.spam, cex = 0.9)
    
    if(!is.null(names)){
      title.x = 0.5*x.coords[j] + 0.5*x.coords[j+1] + 0.04*x.spam
      title.y = y.coord + mg
      text(names[i], x = title.x, y = title.y, cex = 0.9, font = 2)
    }
    
    j = j + 3
  }
}