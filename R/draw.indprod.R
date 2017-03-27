#' @title Create a chart of the Production Indicators time series
#' 
#' @description  Creates a plot of series 21859
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
#' @importFrom seasonal seas

draw.indprod = function(start = NULL, ylim = NULL, xlim = NULL){
  
  indprod = seasonal::final(seas(BETS.get(21859)))
  
  if(!is.null(start)){
    indprod = window(indprod, start = start)
  }
  
  lims = chart.add_basic(ts = indprod, ylim = ylim, xlim = xlim, title = "Industrial Production", subtitle = "Seasonally Adjusted. Index (2012 = 100)", col = "chocolate1")
  chart.add_notes(indprod, ylim = lims[3:4], xlim = lims[1:2])

}