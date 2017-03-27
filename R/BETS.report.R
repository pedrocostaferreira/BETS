#' @title Create dynamic reports with a full analysis of a set of time series
#' 
#' @description Generate automatic reports with a complete analysis of a set of time series. For now, only a SARIMA analysis (Box & Jenkins approach) is possible. In a near future, a GRNN (General Regression Neural Network) analysis will be released. Soon after, Holt-Winters, GRNN, Multilayer Perceptron, Fuzzy Logic and Box-Cox analysis will become available.
#' 
#' @param mode A \code{character}.The type of the analysis. So far, only 'SARIMA' is available.
#' @param ts A \code{integer}, a \code{ts} object or a \code{list} of \code{integer}s and \code{ts} objects. Either the ID of the series in the BETS database or a time series object (any series, not just BETS's). If a \code{list} is provided, a report is generated for each series in this list, which can be mixed with IDs and time series objects.
#' @param parameters A \code{list}. The parameters of the report. See the 'details' section for more information.
#' @param report.file A \code{character}. A path and a name for the report file (an .html file). If there is more than one series, this name will be used as a prefix. If this parameter is not provided, the report will be saved inside the 'reports' folder, under the BETS installation directory. 
#' @param series.saveas A \code{character}. The format of the file on which the series and the predictions should be written. Possible values are 'none' (default), 'sas', 'dta', 'spss', 'csv', 'csv2' . Is is saved under the same directory as the report file.
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
#' @return One or more .html files (the reports) and, optionally, data files (series plus predictions).
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import rmarkdown


BETS.report <- function(mode = "SARIMA", ts = 21864, parameters = NULL, report.file= NA, series.saveas = "none"){
  
  if(class(ts) == "list" || class(ts) == "numeric" || class(ts) == "integer"){
    vec = ts
  } else if(class(ts) == "ts"){
    vec = list(ts)
  } else {
    return(msg("ts - ", .MSG_PARAMETER_NOT_VALID))
  }
  
  if(is.na(report.file)){
    
    dir = paste0(system.file(package="BETS"),"//reports")
    dir.create(dir)
    report.file = paste0(dir,"//analysis")
  }
  
  i = 1

  for(ts in vec){ 
    
    
    name = paste0("analysis_",mode,".Rmd")
    file = system.file(package="BETS", name)
    
    if(class(ts) == 'ts'){
      id = paste0("custom_", i)
      i = i + 1
    }
    else{
      id = ts
    }
    
    rep.file = paste0(report.file, "_", mode, "_", id)
    
    if(series.saveas != "none"){
      series.file = paste0(rep.file,".",series.saveas) 
    }
    else{
      series.file = NA
    }
    
    rep.file = paste0(rep.file,".html")
    
    if(!(ts == 21864 && is.null(parameters))){
      parameters$ts = ts
    }

    parameters$series.file = series.file
    rmarkdown::render(file, params = parameters)
    
    file = gsub(".Rmd", ".html", file)
  
    file.copy(file, rep.file, overwrite = T)
    file.remove(file)
    system2("open", rep.file)
  }
}