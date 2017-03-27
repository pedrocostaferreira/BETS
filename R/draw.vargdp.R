#' @title Create a chart of the Real Percentage Change of GDP in the Year time series
#' 
#' @description  Creates a plot of series 7326
#' 
#' @param file A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory. 
#' @param start A \code{character}. The stating period of the series.
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param open A \code{boolean}. Indicates whether the plot must be opened after being created.
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