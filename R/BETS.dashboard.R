#' @title  Create a BETS custom dashboard
#' 
#' @description  Generate thematic dashboards using a selection of BETS time series and charts. For now, themes and charts are pre-defined.
#' 
#' @param type A \code{character}. The theme of the dashboard. The only option, for the time being, is 'sentiment'.
#' @param saveas A \code{character}. A path and a name for the dashboard file (a .pdf file). If this parameter is not provided, the dashboard will be saved inside the 'dashboards' folder, under the BETS installation directory.
#' 
#' @return A .pdf file (the dashboard)
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @examples 
#' 
#' # BETS.dashboard()
#' # BETS.dashboard(saveas = "survey.pdf")
#' 
#' @export
#' @import rmarkdown


BETS.dashboard = function(type = "sentiment", saveas = NA){
  
  rmd = paste0(type, "_dashboard.Rmd")
  file = system.file(package="BETS", rmd)
  
  rmarkdown::render(file)
  
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