#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom grDevices rgb
#' @import plotly 

draw.misery_rate = function(){
  
  ipca = window(BETS.get(13522), start = c(2006,1))
  unemp = BETS.get(24369)
  
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
    font = list(size = 22)
  )
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(as), y = as, name = "Animal Spirits", width = 700, height = 450) %>% 
    layout(title = "<b>ANIMAL SPIRITS</b>", 
           yaxis = list(title = "Index", tickfont = list(size = 22), titlefont = list(size = 22)),
           xaxis = list(title = "", tickfont = list(size = 22)),
           margin = m,
           titlefont = list(size = 19),
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