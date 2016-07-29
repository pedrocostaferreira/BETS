#' @title  xxx
#' 
#' @description  xxxx
#' 
#' @param model xxx
#' @param params xxx
#' 
#' @return xxx
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import rmarkdown


BETS.report <- function(code = 21864, model = "SARIMA", parameters = NULL){
  
  file = system.file(package="BETS", "analysis.Rmd")
  
  if(is.null(parameters)){
    rmarkdown::render(file)
  }
  else {
    parameters$code = code 
    rmarkdown::render(file, params = parameters)
  }
  
  file = gsub(".Rmd", ".html", file)
  dir.create("reports")
  saveas = gsub("analysis.html", paste0("analysis_", code, ".html"), file)
  file.copy(file, saveas)
  
  system2("open", saveas)
}