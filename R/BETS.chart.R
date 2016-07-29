#' @title  xxx
#' 
#' @description  xxxx
#' 
#' @param ts xxx
#' @param lag.max xxx
#' @param mode xxx
#' @param ci xxx 
#' 
#' @return xxx
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export


BETS.chart = function(alias, lang = "en", out = "png", file = NULL, start = c(2006,1), ylim = NULL){
  
  if(lang == "en"){
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }
  else if(lang == "pt"){
    Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Brazil.1252")
  }
  else {
    return(invisible(msg(.MSG_LANG_NOT_AVAILABLE)))
  }
  
  if(is.null(file)){
    dir.create("graphs", showWarnings = F)
    file = paste0("graphs","\\",alias)
  }
  
  if(out != "png"){
    if(out != "pdf"){
      return(invisible(msg(.MSG_OUT_NOT_AVAILABLE)))
    }
    
    if(!grepl("\\.pdf$", file)) {
      file <- paste(file,".pdf",sep="")
    }
  }
  else {
    if(!grepl("\\.png$", file)) {
      file <- paste(file,".png",sep="")
    }
  }

  if(alias == "ipca"){
    
    draw.ipca(file , start = start, ylim = ylim)
  }
  else if(alias == "ulc"){
    
    draw.ulc(file, start = start, ylim = ylim)
  }
  else {
    msg(paste("Plot was not created.",.MSG_PARAMETER_NOT_VALID))
  }
  
}