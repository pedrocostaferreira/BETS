get.period <- function(start,frequency){
  
  if(class(start) != 'Date'){
    return(msg("Error: Argument 'start' must be a Date object"))
  }
  
  starting_year <- as.numeric(substr(start,1,4))
  starting_month <- as.numeric(substr(start,6,7))
  
  if(frequency == 12 || frequency == 1){
    return(c(starting_year,starting_month))
  }
  
  if(frequency == 4){
    starting_quarter = ceiling(starting_month/3)
    return(c(starting_year,starting_quarter))
  }
  
  if(frequency == 52 || frequency == 365){
    return(1)    
  }
  
}