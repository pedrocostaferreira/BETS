#' @title Create dummy to monthly ts object
#'
#' @description Given a informations about the time and dates, return a monthly 
#' or quarterly ts with dummies.
#' 
#'
#' 
#' 
#' 
#' @param start  the time of the first observation. Either a vector of 
#' two integers, which specify a natural time unit and a (1-based) 
#' number of samples into the time unit. 
#' @param end   the time of the last observation, specified in the same way as start
#' @param frequency the number of observations per unit of time (12 in defaut).
#' @param year a \code{integer},or a \code{seq} or a \code{vector} that 
#'  contains the years. Which year or years will be created the dummys
#' @param month  a \code{integer} or a \code{vector} of size 12 that 
#'  contains the months. For which month or months will be created the dummys
#' @param quarter a \code{integer} or a \code{vector} of size 4 that contains
#' the quarters. For which quarter or quarters will be created the dummys
#' @param date a \code{list} with the date objects. 
#' @param from the starting  value of the sequence. Of 
#' length 1 unless just from is supplied as an unnamed argument.
#' @param to the maximal value of the sequence. 
#' @return A monthly or a quarterly \code{ts}.
#'
#'
#' @details 
#'   the BETS.dummy works for the monthly or a quarterly \code{ts}. 
#'
#' @examples 
#'
#' #1 from a specific date to another specific date
#' BETS.dummy(start = c(2000,1),end = c(2012,5),frequency = 12,from = c(2005,1),to = c(2006,12))
#'
#'
#' #Other options that may be helpful:
#' 
#' #over a month equal to 1
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = c(5,12))
#' 
#' #Months equal to 1 only for some year
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = 5, year = 2010)
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = 8, year = 2002)
#' 
#' #Months equal to 1 only for some years
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = 5, year = 2005:2007)
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = 3, year = c(2005,2007))
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, month = 5:6, year = c(2005,2007))
#'
#' #specific dates
#' BETS.dummy(start = c(2000,1), end = c(2012,5), frequency = 12, date = list(c(2010,1)))
#' BETS.dummy(start = c(2000,1), end = c(2012,5), 
#'     freq = 12, date = list(c(2010,9), c(2011,1), c(2000,1)) )
#'
#'
#' @seealso \code{\link[stats]{ts}}, \code{\link[BETS]{BETS.dummy}}
#'
#'
#' @import zoo
#' @export


BETS.dummy <- function(start = NULL, end = NULL, frequency = 12,
                  year = NULL, month = NULL, quarter = NULL,  
                  date = NULL, from = NULL, to = NULL){
  
 
  
  if(is.null(frequency) | !(frequency %in% c(4,12))){
    stop("Set freq as 12 for monthly series or 4 for quarterly series")
  }
  if(is.null(start)){ stop("Set start")}
  if(is.null(end)){ stop("Set end")}
  
  
  ts <- ts(0, start = start, end = end, freq = frequency)
  years <- as.numeric(substr(as.Date(ts),1,4))
  
  if(frequency == 12){  
    months <- as.numeric(substr(as.Date(ts),6,7))  
    
    if(is.null(date) & is.null(from)){
      
      if(is.null(year)){
        ts[months %in% month] <- 1
      }else{
        ts[months %in% month & years %in% year] <- 1
      }
      
    }else if(!is.null(date)){
      n <- length(date)
      
      months0 <- unlist(date)[seq(2, 2*n, by = 2)]
      years0 <- unlist(date)[seq(1, 2*n, by = 2)]
      
      for(i in 1:n){
        ts[months0[i] == months & years0[i] == years] <- 1
      }
      
    }else if(!is.null(from)){
      
      months0 <- from[2]
      years0 <- from[1]
      
      if(!is.null(to)){
        months1 <- to[2]
        years1 <- to[1]
      }else{
        months1 <- months[length(months)]
        years1 <- years[length(years)]
      }
      
      ts[which(months0 == months & years0 == years):
           which(months1 == months & years1 == years)] <- 1
      
    }
    
  }else if(frequency == 4){  
    quarters <- as.numeric(substr(as.Date(ts),6,7))
    quarters[quarters == 4] <- 2
    quarters[quarters == 7] <- 3
    quarters[quarters == 10] <- 4  
    
    if(is.null(date) & is.null(from)){
      
      if(is.null(year)){
        ts[quarters %in% quarter] <- 1
      }else{
        ts[quarters %in% quarter & years %in% year] <- 1
      }
      
    }else if(!is.null(date)){
      n <- length(date) 
      
      quarters0 <- unlist(date)[seq(2, 2*n, by = 2)]
      years0 <- unlist(date)[seq(1, 2*n, by = 2)]
      
      for(i in 1:n){
        ts[quarters0[i] == quarters & years0[i] == years] <- 1
      }
      
    }else if(!is.null(from)){
      quarters0 <- from[2]
      years0 <- from[1]
      
      if(!is.null(to)){
        quarters1 <- to[2]
        years1 <- to[1]
      }else{
        quarters1 <- quarters[length(quarters)]
        years1 <- years[length(years)]
      }
      
      ts[which(quarters0 == quarters & years0 == years):
           which(quarters1 == quarters & years1 == years)] <- 1
      
    }
  }
  ts
}

