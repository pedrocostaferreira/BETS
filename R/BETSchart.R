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


BETSchart = function(alias, file = NULL){

  if(alias == "ipca"){
    
    if(is.null(file)){
      file = "inst//ipca.png"
    }
    
    draw.ipca(file)
  }
  else if(alias == "ulc"){
    
    if(is.null(file)){
      file = "inst//ulc.png"
    }
    
    draw.ulc(file)
  }
  else {
    msg(paste("Plot was not created.",.MSG_PARAMETER_NOT_VALID))
  }
  
}