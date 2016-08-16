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


BETS.report <- function(ts = 21864, mode = "SARIMA", parameters = NULL){
  
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
  dir.create("reports")
  saveas = paste0("reports//analysis_", mode, "_", ts, ".html")
  file.copy(file, saveas, overwrite = T)
  
  system2("open", saveas)
}