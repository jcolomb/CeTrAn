##get time/date
load.time <- function(params) {
  
 
  
  experiment_time = as.numeric(params$TIMESTAMP)	
  fly_id <- params$FLY
  
  #create time
  timeatstart <- ISOdatetime(1970,1,1,0,0,0) + experiment_time
  
  
  
  res=c(fly_id,timeatstart)
  
  return(time)
  
  
}
x=load.time(params)
##get other group management