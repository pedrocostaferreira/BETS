#' @title Create a chart of the Time Deposits time series 
#' 
#' @description  Creates a plot of series 14
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 

draw.cdb= function(start = NULL, ylim = NULL, xlim = NULL){

  cdb = BETS.get(code = 14, data.frame = TRUE)
  
  if(!is.null(start)){
    
    if(start[2] < 9){
      start[2] = paste0("0",start[2])
    }
    if(start[3] < 9){
      start[3] = paste0("0",start[3])
    }
    
    init = as.Date(paste0(start[1],"-",start[2],"-",start[3]))
    
    i = which(cdb[,1] >= init)
    
    if(length(i) != 0){
      cdb = cdb[i,]
    }
  }
  
  lims = chart.add_basic(ts = cdb, ylim = ylim, xlim = xlim, title = "Time Deposits (CDB/RDB-Preset)", subtitle = "Daily Returns (%)", col = "darkolivegreen", leg.pos = "bottom")
  chart.add_notes(ts(cdb[,2], frequency = 365), ylim = lims[3:4], xlim = lims[1:2],dec = 4)
  
  
}