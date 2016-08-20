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


BETS.report <- function(mode = "SARIMA", ts = 21864, parameters = NULL, saveas= NA){
  
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
  
  if(is.na(saveas)){
    dir = paste0(system.file(package="BETS"),"//reports")
    dir.create(dir)
    saveas = paste0(dir,"//analysis_", mode, "_", ts, ".html")
  }
  
  file.copy(file, saveas, overwrite = T)
  file.remove(file)
  system2("open", saveas)
}