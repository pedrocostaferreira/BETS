#' @title Exportar series em extensao .dta (STATA)
#' 
#' @description
#' 
#' @param x  obejto
#' @param codigo que referencia a serie temporal
#' @param file.name nome com o qual o arquivo sera exportado
#' 
#' @return 
#' 
#' @note 
#' 
#' @examples 
#' 
#' @references 
#' 
#' @import foreign
#' @export BETSsave.stata


BETSsave.stata=function(x,codigo,file.name="teste"){
  k=invisible(BETSsearch(code=codigo,view=FALSE))
  local=getwd()
  require(foreign)
  datas=as.Date(x)
  y=as.data.frame(x)
  colnames(y)=paste(abbreviate(as.character(BETSsearch(code=codigo,view=F)[1,2])))
  rownames(y)=datas
  options(warn=0)
  write.dta(y, paste0(local,"/",file.name,".dta"))
}


