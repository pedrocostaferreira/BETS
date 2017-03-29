#' @title Create a chart of the Economically Active Population time series
#' 
#' @description  Creates a plot of series 10810
#' 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' 
#' @importFrom grDevices dev.new dev.off pdf png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text points mtext arrows
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 

draw.eap = function(start = NULL, ylim = NULL, xlim = NULL){
  
  eap = (BETS.get(10810)/BETS.get(10800))*100
  
  if(!is.null(start)){
    eap = window(eap, start = start)
  }
  
  lims = chart.add_basic(ts = eap, ylim = ylim, xlim = xlim, title = "Economically Active Population", subtitle = "Percentage of Population in Active Age", col = "royalblue", arr.pos = "h", leg.pos = "bottom")
  chart.add_notes(eap, ylim = lims[3:4], xlim = lims[1:2])

}