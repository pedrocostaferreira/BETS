#' @title Create a chart of the Open Unemployment Rate time series
#' 
#' @description  Creates a plot of series 10777
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom zoo as.Date
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.unemp = function(start = NULL, ylim = NULL, xlim = NULL){
  
  unemp = BETS.get(10777)
  
  if(!is.null(start)){
    unemp = window(unemp, start = start)
  }
  
  lims = chart.add_basic(ts = unemp, ylim = c(4,14), title = "Open Unemployment Rate", subtitle = "Metropolitan Regions", col = "royalblue", trend = TRUE)
  chart.add_notes(unemp, ylim = lims[3:4], xlim = lims[1:2])
}