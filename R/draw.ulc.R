#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom zoo as.Date
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.ulc = function(start = NULL, ylim = NULL, xlim = NULL){
  
  cut = BETS.get(11777)
  
  if(!is.null(start)){
    cut = window(cut, start = start)
  }
  
  lims = chart.add_basic(ts = cut, xlim = xlim, ylim = ylim, title = "Unitary Labor Cost", subtitle = "US$ - June 1994 = 100", col = "firebrick4", arr.size = 25)
  chart.add_notes(cut, ylim = lims[3:4], xlim = lims[1:2])

}