#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @import plotly 

draw.animal_spirits = function(){
  
  sond = paste0(system.file(package="BETS"), "/sondagem_fgv.csv")
  fec = paste0(system.file(package="BETS"), "/fecomercio.csv")
  
  data <- read.csv2(sond, stringsAsFactors = F)
  data2 <- read.csv2(fec, stringsAsFactors = F)
  
  ISA = (data[,3] + as.numeric(data2[,2]))/2
  IE = (data[,4] + as.numeric(data2[,3]))/2 
  P = IE - ISA
  
  as = ts(round(P[-length(P)],2), start = c(2001,1), frequency = 12)
  
  a <- list(
    x = as.Date(as)[length(as)],
    y = as[length(as)],
    text = paste0("<b>",as[length(as)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 30)
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(as), y = as, name = "Animal Spirits") %>% 
    layout(title = "<b>Animal Spirits</b>", 
           yaxis = list(title = "Index", tickfont = list(size = 30), titlefont = list(size = 30)),
           xaxis = list(title = "", titlefont = list(size = 30)),
           titlefont = list(size = 30),
           annotations = a,
           shapes = list(
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2001-01-01", x1 = "2001-12-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2003-01-01", x1 = "2003-06-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2008-10-01", x1 = "2009-03-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(as)[length(as)], xref = "x",
                  y0 = -20, y1 = 45, yref = "y")))
  
  return(p)
}