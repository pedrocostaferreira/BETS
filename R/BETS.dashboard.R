#' @title  Create a BETS custom dashboard
#' 
#' @description  Generate thematic dashboards using a selection of BETS time series and charts. For now, themes and charts are pre-defined.
#' 
#' @param type A \code{character}. The theme of the dashboard. The only option, for the time being, is 'inflation'.
#' @param country A \code{character}. The country from which data must drawn. For now, BETS only accepts 'BR' (Brazil) 
#' @param parameters A \code{list}. A list of parameters. See the 'Details' section for a description of these parameters for each type of dashboard.
#' @param saveas A \code{character}. 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import rmarkdown


BETS.dashboard = function(type = "inflation", country = "BR", parameters = NULL, saveas = NA){
  
  rmd = paste0(type, "_dashboard.Rmd")
  file = system.file(package="BETS", rmd)
  
  if(!is.null(parameters$text)){
    if(is.null(parameters$author)){
      msg("You've provided an analysis to be printed together with the dashboard, but the argument 'author' is missing. Dashboard will not be printed.")
    }
  }
  
  if(is.null(parameters)){
    rmarkdown::render(file)
  }
  else {
    rmarkdown::render(file, params = parameters)
  }
  
  if(is.na(saveas)){
    dir = paste0(system.file(package="BETS"),"//dashboards")
    dir.create(dir) 
    saveas = paste0(dir, "//", type, "_dashboard.pdf")
  }
  
  file = gsub(".Rmd", ".pdf", file)
  
  file.copy(file, saveas, overwrite = T)
  file.remove(file)
  system2("open", saveas)
}