#' @title Create a chart of the Real Percentage Change of GDP in the Year time series
#' 
#' @description  Creates a plot of series 7326
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.vargdp = function(start = NULL, ylim = NULL, xlim = NULL){
  
  vargdp = BETS.get(7326)
  
  if(!is.null(start)){
    vargdp = window(vargdp, start = start)
  }
  
  chart.add_basic(ts = vargdp, type = "bar", ylim = c(-5.5,10), xlim = xlim, title = "Real GDP", subtitle = "Percentage Change in the Year", col = "chocolate1", trend = T)
  
}