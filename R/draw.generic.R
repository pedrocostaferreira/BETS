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
    
    no.extra <- F
    
    if(is.null(params$type)){
        params$type = 'lines'   
    } 
    
    if(is.null(params$title)){
        params$title = ''
    }
    
    if(is.null(params$subtitle)){
        params$subtitle = ''
    }
    
    if(is.null(params$arrow)){
        params$arrow = 'v'
    }
    
    if(is.null(params$extra)){
        no.extra = T
    }
    
    if(is.null(params$extra.arrow)){
        params$extra.arrow = 'h'
    }
    
    if(is.null(params$legend)){
        params$legend = 'none'
    }
    
    if(is.null(params$legend.pos)){
        params$legend.pos = 'topleft'
    }
    
    if(is.null(params$colors)){
        params$colors = c("firebrick4", "firebrick3")
    }
    

    if(style == "normal"){
        
        series = ts
        leg.pos = params$legend.pos
        ylim = params$ylim
        xlim = params$xlim
        
        if(!is.null(params$extra)){
            leg.pos = 'none'
        }
        
        lims = chart.add_basic(ts = series, title = params$title, subtitle = params$subtitle, xlim = params$xlim, ylim = params$ylim, col = params$colors[1], leg.pos = leg.pos)
        
        if(is.null(params$xlim)){
          params$xlim = lims[1:2]  
        } 
        
        if(is.null(params$ylim)){
            params$ylim = lims[3:4]  
        } 
            
        if(!is.null(params$extra)){
            series = list(series, params$extra)
            chart.add_extra(params$extra, ylim = params$ylim, xlim = params$xlim, col = params$colors[2], leg.pos = leg.pos)
        }
        
        if(!is.null(params$legend)){
            leg = params$legend
            legend("topleft", leg, lty=c(1,2), lwd=c(2.5,2.5), col= params$colors, bty = "n", cex = 0.9)
        } else {
            leg = paste0("Chart ", 1:length(series))
        }
        
        chart.add_notes(series, names = params$legend, ylim = lims[3:4], xlim = lims[1:2])
        
    } else {
        
        
        
    }
    
    return(NULL)
}