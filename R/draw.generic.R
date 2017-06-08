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
    
    if(style == "plotly"){
        
        series = ts
        
        if(is.null(params$legend) || !is.null(params$extra)){
            leg.pos = "none"
        } else {
            leg.pos = "top"
        }
        
        if(is.null(params$colors)){
            cols = c("firebrick4", "firebrick3")
        } else {
            cols = params$colors
        }
        
        lims = chart.add_basic(ts = series, title = params$title, subtitle = params$subtitle, col = cols[1], leg.pos = leg.pos)
        
        if(!is.null(params$extra)){
            series = list(series, params$extra)
            chart.add_extra(params$extra, ylim = lims[3:4], xlim = lims[1:2], col = cols[2], leg.pos = leg.pos)
        }
        
        if(!is.null(params$legend)){
            leg = params$legend
            legend("topleft", leg, lty=c(1,2), lwd=c(2.5,2.5),col=cols, bty = "n", cex = 0.9)
        } else {
            leg = paste0("Chart ", 1:length(series))
        }
        
        chart.add_notes(series, names = params$legend, ylim = lims[3:4], xlim = lims[1:2])
        
    } else {
        
        
        
    }
    
    return(NULL)
}