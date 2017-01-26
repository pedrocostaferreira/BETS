#' @title Create a dynamic report with a full analysis of a time series
#' 
#' @description Generate an automatic report with a complete analysis of a time series. For now, only a SARIMA analysis (Box & Jenkins approach) is possible. In a near future, a GRNN (General Regression Neural Network) analysis will be released. Soon after, Holt-Winters, Multilayer Perceptron, Fuzzy Logic and Box-Cox analysis will become available.
#' 
#' @param mode A \code{character}. The type of the analysis. So far, only 'SARIMA' is available.
#' @param ts A \code{integer} or a \code{ts} object. Either the ID of the series in the BETS database or a time series object (any series, not just BETS's)
#' @param parameters A \code{list}. The parameters of the report. See the 'details' section for more information.
#' @param report.file A \code{character}. A path and a name for the report file (an .html file). If this parameter is not provided, the report will be saved inside the 'reports' folder, under the BETS installation directory.
#' @param series.saveas \code{character} The format of the file in which the series and the previsions should be saved. Possible values are 'none' (default), 'sas', 'dta' and 'spss' . Is is going to be saved under the same directory as the report file.
#' 
#' @details 
#' 
#' \bold{SARIMA Report Parameters}
#' 
#' \itemize{
#' \item{\code{lag.max}: An \code{integer} Maximum number of lags to show on the ACFs e PACFs}
#' \item{\code{n.ahead}: An \code{integer} Prevision horizon (number of steps ahead)}
#' }
#' 
#' @return An .html file (the report)
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import rmarkdown


BETS.report <- function(mode = "SARIMA", ts = 21864, parameters = NULL, report.file= NA){
  
  name = paste0("analysis_",mode,".Rmd")
  file = system.file(package="BETS", name)
  
  if(is.null(parameters)){
    rmarkdown::render(file)
  }
  else {
    parameters$ts = ts
    rmarkdown::render(file, params = parameters)
  }
  
  file = gsub(".Rmd", ".html", file)
  
  if(is.na(report.file)){
    if(class(ts) == 'ts'){
      ts = "custom"
    }
    
    dir = paste0(system.file(package="BETS"),"//reports")
    dir.create(dir)
    report.file = paste0(dir,"//analysis_", mode, "_", ts, ".html")
  }
  
  file.copy(file, report.file, overwrite = T)
  file.remove(file)
  system2("open", report.file)
}