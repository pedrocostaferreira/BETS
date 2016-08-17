gdp = window(BETS.get(4382), start = c(1996,1))
ipca = window(BETS.get(13522),start = c(1996,1))

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

BETS.normalize = function(series, mode){
  
  if(mode == "maxmin"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x-min(x))/(max(x)-min(x))}))
    }
    else{
      return((series - min(series))/max(series) - min(series))
    }
    
  }
  
  else if(mode == "scale"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x - mean(x))/sd(x)}))
    }
    else {
      return((series - mean(series))/sd(series))
    }
  }
  
  return("ERROR")
}

gdp_real = BETS.deflate(gdp, ipca, type = "point.perc")
gdp_real_norm = BETS.normalize(gdp_real, mode = "scale")
ipca_norm = BETS.normalize(ipca, mode = "scale")

lag.max = 2

series = vector(mode = "list")
series[[1]] = ipca_norm
series[[2]] = gdp_real_norm

train = vector(mode = "list")
train[[1]] = ipca_norm

lag.max = 2
pres = 1

nvars = length(series)
for(i in 1:nvars){
  s = (nvars - pres) + (i-1)*lag.max
  for(j in 1:lag.max){
    train[[s + j]] = lag(train[[i]],-j)
  }
}

sigma = 0.5
data = cbind(train[[1]],train[[3]],train[[4]])
nn = smooth(learn(data),sigma)

