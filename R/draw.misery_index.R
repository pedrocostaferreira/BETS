#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom grDevices rgb
#' @import plotly 

draw.misery_index = function(){
  
  ipca = suppressWarnings(BETS.get(13522)) 
  ipca = ts(ipca[,"value"], start = c(1980,12), frequency = 12)
  ipca = window(ipca, start = c(2012,3))
  unemp = BETS.get(24369)
  misery = ipca + unemp 
  
  
  a <- list(
    x = as.Date(misery)[length(misery)],
    y = misery[length(misery)],
    text = paste0("<b>",misery[length(misery)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 22)
  )
  
  
  t <- list(
    x = 0.5,
    y = 1.18,
    text = "<b>MISERY INDEX</b><br><span style = 'font-size:16'>Unemployment Rate Plus Inflation Rate</span>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  m <- list(
    t = 60,
    pad = 1
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(width = 700, height = 450) %>%
    add_lines(x = as.Date(misery), y = misery, name = "Misery Index") %>%
    layout(title = "", 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(title = "", tickfont = list(size = 22)),
           margin = m,
           annotations = list(t,a),
           shapes = list(
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(misery)[length(misery)], xref = "x",
                  y0 = 10, y1 = 22, yref = "y")))
  
  return(p)
}