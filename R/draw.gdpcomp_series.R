#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom forecast ma
#' @import plotly 
#' @importFrom seasonal seas

draw.gdpcomp_series = function(){
  
  gdp_comp = paste0(system.file(package="BETS"), "/pib_componentes.csv")
  data <- read.csv2(gdp_comp, stringsAsFactors = F)
  
  gdp = window(ts(as.numeric(data[,2]), start = c(1996,1), frequency = 4),start = c(2013,1))
  gdp_cons = window(ts(as.numeric(data[,3]), start = c(1996,1), frequency = 4),start = c(2013,1))
  gdp_gov = window(ts(as.numeric(data[,4]), start = c(1996,1), frequency = 4),start = c(2013,1))
  gdp_bffk = window(ts(as.numeric(data[,5]), start = c(1996,1), frequency = 4),start = c(2013,1))
  
  m <- list(
    t = 50,
    l = 75,
    pad = 1
  )
  
  ay <- list(
    title = "% do PIB (Dívida)",
    overlaying = "y",
    side = "right",
    zeroline = FALSE,
    showgrid = FALSE
  )
  
  if(gdp[length(gdp)] < 0){
    y0 = 0 
  } else {
    y0 = gdp[length(gdp)]
  }
  
  a <- list(
    x = as.Date(gdp)[length(gdp)],
    y = y0,
    text = paste0("<b>",gdp[length(gdp)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -40,
    ax = 0,
    font = list(size = 22)
  )
  
  p = plot_ly(type = "bar", x = as.Date(gdp), y = gdp, name = "GDP", width = 700, height = 450) %>% 
    add_trace(y = gdp_gov, x = as.Date(gdp_gov), name = "Gov. Exp.", type = "scatter", mode = "lines") %>%
    add_trace(y = gdp_cons, x = as.Date(gdp_cons), name = "Hous. Exp.", type = "scatter", mode = "lines") %>%
    add_trace(y = gdp_bffk, x = as.Date(gdp_bffk), name = "BFFK", type = "scatter", mode = "lines") %>%
    layout(title = "<b>GDP COMPONENTS</b>", 
           yaxis = list(title = "% variation", tickfont = list(size = 20), titlefont = list(size = 22)),
           xaxis = list(title = "", tickfont = list(size = 17)),
           #yaxis2 = ay,
           margin = m,
           titlefont = list(size = 19),
           annotations = a,
           legend = list(orientation = 'h', x = 0.15)
    )
  
  return(p)
}