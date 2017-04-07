#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @import plotly 

draw.ei_comps = function(){
  
  file.cei = paste0(system.file(package="BETS"), "/cei.csv")
  cei <- read.csv2(file.cei, stringsAsFactors = F)
  
  file.lei = paste0(system.file(package="BETS"), "/lei.csv")
  lei <- read.csv2(file.lei, stringsAsFactors = F)
  
  lei.labs = c("Swap<br>Rate","Manufacturing<br>Expec.", "Services<br>Expec.", "Consumers<br>Expec.", "Stock<br>Prices", "Terms of<br>Trade", "Consumer<br>Durable Goods<br>Production Exp.", "Exports<br>Volume")
  cei.labs = c("Industrial<br>Prod.","Ind. Electric<br>Energy Cons.","Shipments of<br>Corrugated<br>Paper", "Volume of<br>Sales (Ret.)", "Employement", 'Avg. Real<br>Income (Workers)')

  cei$Value = as.numeric(cei$Value)
  cei = cbind(cei, cei.labs)[,2:3]
  cei = cei[order(cei$Value, decreasing = T),]
  
  lei$Value = as.numeric(lei$Value)
  lei = cbind(lei, lei.labs)[,2:3]
  lei = lei[order(lei$Value, decreasing = T),]
  
  lei.x <- t(lei[,1])
  lei.y <- "LEI"
  data.lei = data.frame(lei.y, lei.x)
  
  cei.x <- t(cei[,1])
  cei.y <- "CEI"
  data.cei = data.frame(cei.y, cei.x)
  
  m <- list(
    t = 80,
    pad = 1
  )
  
  t <- list(
    x = 0.5,
    y = 1.175,
    text = "<b>LEI AND CEI COMPONENTS VARIATION</b>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  cols = c('rgba(38, 24, 74, 1)', 'rgba(38, 24, 74, 0.9)', 'rgba(38, 24, 74, 0.8)', 'rgba(71, 58, 131, 0.8)', 'rgba(71, 58, 134, 0.7)', 'rgba(164, 163, 204, 0.85)', 'rgba(190, 192, 213, 1)','rgba(122, 120, 168, 0.8)')
  
  p1 <- plot_ly(data.lei, type = 'bar', orientation = 'h',  width = 700, height = 450) %>%
    add_trace(x = ~X1, name = lei.labs[1], marker = list(color = cols[1])) %>%
    add_trace(x = ~X2, name = lei.labs[2], marker = list(color = cols[2])) %>%
    add_trace(x = ~X3, name = lei.labs[3], marker = list(color = cols[3])) %>%
    add_trace(x = ~X4, name = lei.labs[4], marker = list(color = cols[4])) %>%
    add_trace(x = ~X5, name = lei.labs[5], marker = list(color = cols[5])) %>%
    add_trace(x = ~X6, name = lei.labs[6], marker = list(color = cols[6])) %>%
    add_trace(x = ~X7, name = lei.labs[7], marker = list(color = cols[7])) %>%
    add_trace(x = ~X8, name = lei.labs[8], marker = list(color = cols[8])) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = T,
                        zerolinecolor = '#969696',
                        zerolinewidth = 3),
           yaxis = list(showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0, 0.75)),
           barmode = 'relative',
           annotations = t,
           #paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           showlegend = FALSE) %>%
           add_annotations(xref = 'paper', yref = 'paper', 
                            xanchor = 'right', x = 0, y = 0.375, 
                            text = paste0("<b>",data.lei[1,1],"</b>"), showarrow = F,font = list(size = 16))
  
  p2 <- plot_ly(data.cei, type = 'bar', orientation = 'h',  width = 700, height = 450) %>%
    add_trace(x = ~X1, name = cei.labs[1], marker = list(color = cols[1])) %>%
    add_trace(x = ~X2, name = cei.labs[2], marker = list(color = cols[2])) %>%
    add_trace(x = ~X3, name = cei.labs[3], marker = list(color = cols[3])) %>%
    add_trace(x = ~X4, name = cei.labs[4], marker = list(color = cols[4])) %>%
    add_trace(x = ~X5, name = cei.labs[5], marker = list(color = cols[5])) %>%
    add_trace(x = ~X6, name = cei.labs[6], marker = list(color = cols[6])) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = T,
                        zerolinecolor = '#969696',
                        zerolinewidth = 3),
           yaxis = list(showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0,0.75)),
           barmode = 'relative',
           #paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           showlegend = FALSE) %>%
    add_annotations(xref = 'paper', yref = 'paper', 
                    xanchor = 'right', x = 0, y = 0.375, 
                    text = paste0("<b>",data.cei[1,1],"</b>"), showarrow = F,font = list(size = 16))
  
  subplot(p1,p2, nrows = 2)                                                                  
  
  return(p)
}