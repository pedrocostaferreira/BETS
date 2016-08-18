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

BETS.deflate = function(ts, deflator, type = "index"){
  
  freq_ts = frequency(ts)
  freq_def = frequency(deflator)
  
  if(freq_ts != freq_def){
    return("ERROR")
  }
  
  start_ts = start(ts)
  start_def = start(deflator)
  end_ts = end(ts)
  end_def = end(deflator)
  
  if(start_ts[1] > start_def[1]){
    deflator = window(deflator, start = start_ts, frequency = freq_ts)
  }
  else if(start_ts[1] < start_def[1]){
    ts = window(ts, start = start_def, frequency = freq_ts)
  }
  else{
    
    if(start_ts[2] > start_def[2]){
      deflator = window(deflator, start = start_ts, frequency = freq_ts)
    }
    else if(start_ts[2] < start_def[2]){
      ts = window(ts, start = start_def, frequency = freq_ts)
    }
  }
  
  if(end_ts[1] > end_def[1]){
    ts = window(ts, end = end_def, frequency = freq_ts)
  }
  else if(end_ts[1] < end_def[1]){
    deflator = window(deflator, end = end_ts, frequency = freq_ts)
  }
  else{
    
    if(end_ts[2] > end_def[2]){
      ts = window(ts, end = end_def, frequency = freq_ts)
    }
    else if(end_ts[2] < end_def[2]){
      deflator = window(deflator, end = end_ts, frequency = freq_ts)
    }
  }
  
  if(type == "index"){
    deflator = 100/deflator
  }
  else if(type == "point.perc"){
    deflator= 1/deflator
  }
  else {
    deflator = 1/(deflator/100 + 1)
  }
  
  return(ts*deflator)
}