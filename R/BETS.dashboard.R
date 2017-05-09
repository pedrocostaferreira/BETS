#' @title  Create a BETS custom dashboard
#' 
#' @description  Generate thematic dashboards using a selection of BETS time series and charts. For now, themes and charts are pre-defined.
#' 
#' @param type A \code{character}. The theme of the dashboard. The only two options, for the time being, is 'business_cycle' and 'macro_situation'.
#' @param saveas A \code{character}. A path and a name for the dashboard file (a .pdf file). If this parameter is not provided, the dashboard will be saved inside the 'dashboards' folder, under the BETS installation directory.
#' @param parameters A \code{list}. A list of parameters. See the 'Details' section for a description of these parameters for each type of dashboard.
#' 
#' @details
#' 
#' \bold{Macro Situation Dashboard Parameters} 
#' 
#' \itemize{
#' \item{ \code{text} - The text to be printed in the dashboard. Separate paragraphs with two backslashes 'n' and pages with '##'. There are no other syntax rules.}
#' \item{ \code{author} - The author's name.}
#' \item{ \code{email} - The author's email.}
#' \item{ \code{url} - The author's webpage.}
#' \item{ \code{logo} - The author's business logo.}
#' }
#' 
#' @return A .pdf file (the dashboard)
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @examples 
#' 
#' # BETS.dashboard()
#' # BETS.dashboard(saveas = "survey.pdf")
#' # BETS.dashboard(type = "macro_situation")
#' 
#' @export
#' @import rmarkdown


BETS.dashboard = function(type = "business_cycle", saveas = NA, parameters = NULL){
  
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