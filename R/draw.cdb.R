#' @title Create a chart of the Time Deposits time series 
#' 
#' @description  Creates a plot of series 14
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

draw.cdb= function(file, start = NULL, ylim = NULL, open = TRUE){
  
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
  
  #cdb= BETS.get(14)
  #cdb = cdb[-1]
  #days = seq(as.Date("1992-03-04"), Sys.Date(), by = "1 day")
  #weekdays = days[!weekdays(days) %in% c('Saturday','Sunday')]
  #weekdays = weekdays[1:length(cdb)]
  #15-04-2016
  
  cdb = read.csv2("cdb.csv",stringsAsFactors = F)
  cdb = data.frame(as.Date(cdb[,1],format = "%d/%m/%Y"),as.numeric(cdb[,2]))
  
  if(!is.null(start)){
    
    if(start[2] > 9){
      start[2] = paste0("0",start[2])
    }
    if(start[3] > 9){
      start[3] = paste0("0",start[3])
    }
    
    init = as.Date(paste0(start[1],"-",start[2],"-",start[3]))
    
    if(weekdays(init) == "Sunday"){
      init = init + 1
    }
    else if(weekdays(init) == "Saturday"){
      init = init - 1
    }
    
    i = which(cdb[,1] == init)
    
    if(i != F){
      cdb = cdb[i:nrow(cdb),]
    }
  }
  
  
  if(is.null(ylim)){
    ylim = c(min(cdb[,2]),max(cdb[,2])+0.02)
  }
  
  first = as.numeric(cdb[1,1])
  last = as.numeric(cdb[nrow(cdb),1])
  aval = paste0("Last available data: ",cdb[nrow(cdb),1])
  
  x.spam = last - first
  y.spam = ylim[2] - ylim[1]
  
  s = seq(1,nrow(cdb),by = floor(nrow(cdb)/5))
  labs = cdb[,1][s]
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
  plot.ts(x = cdb[,1], y = cdb[,2], type = "l", xaxt = "n", ylim = ylim, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = "Time Deposits (CDB/RDB-Preset)", col = "darkolivegreen")
  axis(1, at = labs, labels = labs, las=1, cex.axis = 0.75)
  mtext("Daily Returns (%)")
  
  val = round(cdb[nrow(cdb),2],2)
  
  points(last, val, pch = 21, cex = 1.25, lwd = 2, bg = "darkolivegreen", col = "darkgray")
  text(first + 0.22*x.spam, ylim[2] - 0.05*y.spam, aval, cex = 0.9)
  
  x1 = last
  y0 = ylim[1] + 0.4*y.spam
  y1 = val - 0.03*y.spam
  
  arrows(x0 = x1, x1 = x1, y0 = y0, y1 = y1, length = c(0.00003*x.spam, 0.00006*y.spam), lwd = 2)
  text(x1 - 0.005*x.spam, y0 - 0.05*y.spam, as.character(val), cex = 1.1, font = 2)
  
  add.notes(ts(cdb[,2], frequency = 365), ylim = ylim, xlim = c(first,last))
  
  dev.off()
  
  if(open){
    file.show(file)
  }
}