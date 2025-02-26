##Step 3: Make functions for what happens when each event occurs
#'@param time1_ is the time of the first event
#'@param DR_ is the discount rate
#'@return disc_factor is the 1/((1+Discount Rate)^Time)

Disc_factor <- function(time1_,
                    DR_){
  
  temp2 <- (1+DR_)^-time1_ ##(1+r)^-t = 1 /((1+r)^t)
  
  return(temp2)
}