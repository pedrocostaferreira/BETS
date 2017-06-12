#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @param ts aaaa
#' @param style aaa
#' @param params aaa
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.generic <- function(ts, style, params){
    
    no.extra = F
    no.legend = F
    
    if(is.null(params$extra)){
        no.extra = T
    }
    
    if(is.null(params$legend)){
        no.legend = T
        leg = paste0("Series ", 1:length(series))
    } else {
        leg = params$legend
    }
    
    if(is.null(params$trend)){
        params$trend = F
    }

    if(is.null(params$type)){
        params$type = 'lines'   
    } 
    
    if(is.null(params$title)){
        params$title = ''
    }
    
    if(is.null(params$subtitle)){
        params$subtitle = ''
    }
    
    if(is.null(params$arr.ort)){
        params$arr.ort = 'v'
    }
    
    if(is.null(params$extra.arr.ort)){
        params$extra.arr.ort = 'h'
    }
    
    if(is.null(params$legend.pos)){
        params$legend.pos = 'topleft'
    }
    
    
    if(is.null(params$xlim) && !no.extra){
        rgs = c(range(time(ts)),range(time(params$extra)))
        params$xlim = c(min(rgs), max(rgs))
    }
    
    if(is.null(params$ylim) && !no.extra){
        rgs = c(range(ts),range(params$extra))
        params$ylim = c(min(rgs), max(rgs))
    }
    

    if(style == "normal"){
        
        series = ts
        leg.pos = params$legend.pos
        
        if(is.null(params$colors)){
            params$colors = c("firebrick4", "firebrick3")
        }
        
        if(!no.extra){
            leg.pos = 'none'
        }
        
        lims = chart.add_basic(ts = series, type = params$type, title = params$title, subtitle = params$subtitle, xlim = params$xlim, ylim = params$ylim, col = params$colors[1], leg.pos = leg.pos, arr.pos = params$arr.ort, arr.size = params$arr.len, trend = params$trend)
        
        if(is.null(params$xlim)){
            params$xlim = lims[1:2]  
        } 
        
        if(is.null(params$ylim)){
            params$ylim = lims[3:4]  
        } 
            
        if(!no.extra){
            series = list(series, params$extra)
            chart.add_extra(params$extra, ylim = params$ylim, xlim = params$xlim, col = params$colors[2], leg.pos = leg.pos, arr.pos = params$extra.arr.ort, arr.size = params$extra.arr.len, main.type = params$type)
        }
        
        if(!no.legend){
            
            t2 = 2
            
            if(params$type == "bar"){
                t2 = 1
            }
            
            legend(params$legend.pos, leg, lty=c(1,t2), lwd=c(2.5,2.5), col= params$colors, bty = "n", cex = 0.9)
        } 
        
        if(frequency(ts) != 1){
            chart.add_notes(series, names = leg, ylim = lims[3:4], xlim = lims[1:2]) 
        }
        
    } else {
        
        if(is.null(params$colors)){
            params$colors = c("#8B1A1A", "#CD2626")
        }
        
        m <- list(
            t = 50,
            pad = 1
        )
        
        
        last_val = ts[length(ts)]
        last_date = as.Date(ts)[length(ts)]
        
        a1 <- list(
            x = last_date,
            y = last_val,
            text = paste0("<b>",last_val,"</b>"),
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 6,
            ay = 40,
            ax = 0,
            font = list(size = 22)
        )
        
        a2 <- NULL
        
        p = plot_ly(width = 700, height = 450) 
        
        if(params$type == "lines"){
            p <- p %>% add_lines(x = as.Date(ts), y = ts, name = leg[1], line = list(color = params$colors[1]))
        } else {
            p <- p %>% add_trace(type = "bar", x = as.Date(ts), y = ts, name = leg[1], marker = list(color = params$colors[1]))
        }
        
        if(params$trend){
            require(mFilter)
            tr <- fitted(mFilter::hpfilter(ts))
            p <- p %>% add_trace(y = tr, x = as.Date(tr), name = "Trend", line = list(color = "#bd081c", dash = "dash")) 
        }
        
        if(!no.extra){
            
            if(is.null(params$extra.y2)){
                y2 <- "y"
                ay <- NULL
            } else {
                y2 <- "y2"
                ay <- list(
                    overlaying = "y",
                    side = "right",
                    zeroline = FALSE,
                    showgrid = FALSE,
                    tickfont = list(size = 22)
                )
            }
            
            a2 <- list(
                x = last_date,
                y = last_val,
                text = paste0("<b>",last_val,"</b>"),
                xref = "x",
                yref = y2,
                showarrow = TRUE,
                arrowhead = 6,
                ay = 40,
                ax = 0,
                font = list(size = 22)
            )
            
            p <- p %>% add_lines(yaxis = y2, x = as.Date(params$extra), y = params$extra, name = leg[2])
        }
        
        if(!no.legend){
            legend.list = list(orientation = params$legend.pos)
        } else {
            legend.list = NULL
        }
        
        p <- p %>% layout(title = paste0("<b>",title,"</b>"), 
                       yaxis = list(tickfont = list(size = 22)),
                       xaxis = list(tickfont = list(size = 15)),
                       yaxis2 = ay,
                       margin = m,
                       titlefont = list(size = 19),
                       annotations = list(a1,a2),
                       legend = legend.list)
        
    }
    
    return(NULL)
}