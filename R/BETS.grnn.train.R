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
#' @import grnn forecast


BETS.grnn.train = function(train.set, sigma, step = 0.1, select = TRUE, names = NA){
  
  if(length(train.set) < 2 || !check.series(train.set, "Series list: train.")){
    return(NULL)
  }
  
  if(!is.na(names) && length(train.set) != length(names)){
    msg("ERROR")
    return(NULL)
  }
  
  train.n_elem = length(train.set[[1]])
  train.n_series = length(train.set)
  series = train.set[[1]]
  
  if(is.vector(sigma)){
    sigma = seq(sigma[1],sigma[2],step)
  }
  
  train_mt = matrix(nrow = train.n_elem, ncol = train.n_series)

  for(i in 1:train.n_series){
    train_mt[,i] = train.set[[i]]
  }
  
  results.list = vector(mode = "list")
  id = 1
  
  if(select){
    
    for(i in 1:(train.n_series-1)){
      
      trial = combn(2:train.n_series,i) 
      
      for(j in 1:ncol(trial)){
        
        sub_train = matrix(nrow = train.n_elem, ncol = nrow(trial)+1)
        sub_train[,1] = train_mt[,1]
        
        for(k in 1:nrow(trial)){
          
          ind = trial[k,j]
          sub_train[,k+1] = train_mt[,ind]
        }
        
        result = vector(mode = "list")
        result$mape = 1.797693e+308
        vec.sigmas = vector(mode = "numeric")
        vec.mapes = vector(mode = "numeric")
        
        for(s in sigma){
          
          nn = smooth(learn(sub_train),s)
          
          fitted = vector(mode = "numeric")
          sub_train_fit = as.matrix(sub_train[,-1])
          
          for(r in 1:nrow(sub_train)){
            fitted[r] = guess(nn, t(as.matrix(sub_train_fit[r,])))
          }
          
          acc = accuracy(fitted,sub_train[,1])[5]
          
          vec.sigmas = c(vec.sigmas,s)
          vec.mapes = c(vec.mapes,acc)

          if(acc < result$mape){
            
            result$mape = acc
            result$fitted = fitted
            result$net = nn
            result$sigma = s
            
            regs = trial[,j]
            
            if(!is.na(names)){
              result$regressors = names[regs] 
            }
            else {
              result$regressors = regs 
            }
          }
        }
        
        result$sigma.mape = cbind(sigma = vec.sigmas, mape = vec.mapes)
        result$series = series
        result$residuals = result$series - result$fitted
        result$id = id
        
        results.list[[id]] = result
        id = id + 1
      }
    }
  }
  
  len = length(results.list)
  rankm = data.frame(matrix(nrow = len, ncol = 4))
  names(rankm) = c("id","mape","regs","sigma")
  
  for(i in 1:len){
    rankm[i,"id"] = i
    rankm[i,"mape"] = results.list[[i]]$mape
    rankm[i,"regs"] = paste(results.list[[i]]$regressors, collapse = ",")
    rankm[i,"sigma"] = results.list[[i]]$sigma
  }
  
  rankm = head(rankm[order(rankm[,2]),],20)
  
  print("General Regression Neural Network")
  print(rankm)
  
  results = vector(mode = "list")
  for(i in 1:20){
    results[[i]] = results.list[[rankm[i,"id"]]]
  }
  
  return(results)
}