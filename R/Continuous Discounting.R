##Step 3: Make functions for what happens when each event occurs
#'@param time1_ is the time of the first event
#'@param time2_ is the time of the second event
#'@param DR_ is the discount rate
#'@return disc_le is the discounted time between time 1 and time 2
#'
Disc_LE <- function(time1_,
                    time2_,
                    DR_){
  #Calculate the variables that we will need
  temp1 <- log(1+DR_) # Log is the natural logarithm in R
  temp2 <- (1+DR_)^-time1_
  temp3 <- (1+DR_)^-time2_
  
  if(time1_ < time2_){
    #In case of mistakes do one set of code when time1_ > time2_
    #If time1_ is smaller than time2_ do temp 3 (bigger time) first
    disc_le <- 
      -((temp3)/temp1)-
      -((temp2)/temp1)
    
  }else{
    #If time2_ is bigger than time3_ do temp 2 (bigger time) first
    disc_le <- 
      -((temp2)/temp1)-
      -((temp3)/temp1)
  }
  #Drop calculated variables that I am not returning out of the function
  rm(temp1, temp2, temp3)
  
  return(disc_le)
}