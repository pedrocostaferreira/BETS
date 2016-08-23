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


BETS.grnn.train = function(train, sigma, step = 0.1, select = TRUE, names = NA){
  
  if(length(train) < 2 || !check.series(train, "Series list: train.")){
    return(NULL)
  }
  
  #if(length(test) < 2 || !check.series(test, "Series list: test.")){
    #return(NULL)
  #}
  
  if(!is.na(names) && length(train) != length(names)){
    msg("ERROR")
    return(NULL)
  }
  
  train.n_elem = length(train[[1]])
  train.n_series = length(train)
  #test.n_elem = length(test[[1]])
  #test.n_series = length(test) - 1
  #actual = test[[1]]
  
  if(is.vector(sigma)){
    sigma = seq(sigma[1],sigma[2],step)
  }
  
  vec.sigmas = vector(mode = "numeric")
  vec.mapes = vector(mode = "numeric")
  
  train_mt = matrix(nrow = train.n_elem, ncol = train.n_series)
  #test_mt = matrix(nrow = test.n_elem, ncol = test.n_series)

  for(i in 1:train.n_series){
    train_mt[,i] = train[[i]]
  }
  
  #for(i in 1:test.n_series){
   # test_mt[,i] = test[[i+1]]
  #}
  
  result = vector(mode = "list")
  result$mape = 1.797693e+308
  
  if(select){
    
    for(i in 1:(train.n_series-1)){
      
      trial = combn(2:train.n_series,i) 
      
      for(j in 1:ncol(trial)){
        
        sub_train = matrix(nrow = train.n_elem, ncol = nrow(trial)+1)
        #sub_test = matrix(nrow = test.n_elem, ncol = nrow(trial))
        sub_train[,1] = train_mt[,1]
        
        for(k in 1:nrow(trial)){
          
          ind = trial[k,j]
          sub_train[,k+1] = train_mt[,ind]
          #sub_test[,k] = test_mt[,ind-1]
        }
        
        for(s in sigma){
          
          nn = smooth(learn(sub_train),s)
          #prevs = vector(mode = "numeric")
          
          #for(r in 1:nrow(sub_test)){
           # prevs[r] = guess(nn, t(as.matrix(sub_test[r,])))
          #}
          
          #acc = accuracy(prevs,actual)[5]
          
          fitted = vector(mode = "numeric")
          sub_train_fit = as.matrix(sub_train[,-1])
          
          for(r in 1:nrow(sub_train)){
            fitted[r] = guess(nn, t(as.matrix(sub_train_fit[r,])))
          }
          
          acc = accuracy(fitted,sub_train[,1])[5]
          
          #print(c(acc,floor(trial[,j]),s))

          if(acc < result$mape){
            
            vec.sigmas = c(vec.sigmas,s)
            vec.mapes = c(vec.mapes,acc)
            
            result$mape = acc
            #result$predicted = prevs
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
        
        vec.sigmas = c(vec.sigmas,result$sigma)
        vec.mapes = c(vec.mapes,result$mape)
      }
    }
  }
  
  result$sigma.mape = data.frame(sigma = vec.sigmas, mape = vec.mapes)

  return(result)
}