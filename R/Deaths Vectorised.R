####Death Code
#'@param pat_chars_ is the patient charactersitics matrix
#'@param GlobalOptions_, is the matrix with the Global Options
#'@param parameter_ is the vector of model parameters for this model run
#'@param Treatment_ is a text varaible indicating which arm to run
#'@param last_event_ is a number that indicates the time of the last event
#'@param next_event is a number that indicates the time of the next event
#'@param mask_ is a TRUE/FALSE vector indicating which patients to apply these calculations to
#'@return pat_chars_ is the patient charactersitics matrix after we have done our calculations

DeathVec <- function (pat_chars_, GlobalOptions_, parameter_,Treatment_,last_event_, this_event_, mask_){
  
  #Get the discount rates as numbers
  DRC <- as.numeric(GlobalOptions_["Discount_rate_cost","Value"])
  DRQ <- as.numeric(GlobalOptions_["Discount_rate_QALY","Value"])
  
  #Calculate the time of this event and the last event
  last_event <- last_event_[mask_]
  this_event <- this_event_[mask_]
  
  #Undiscounted QALYs
  pat_chars_[mask_,"QALYs"] <- pat_chars_[mask_,"QALYs"] + (this_event-last_event)*pat_chars_[mask_,"Utility"]
  #Discounted QALYs
  pat_chars_[mask_,"Discounted QALYs"] <- pat_chars_[mask_,"Discounted QALYs"] + 
    Disc_LE(this_event,last_event, DRQ)*pat_chars_[mask_,"Utility"]
  
  if(Treatment_ == "Intervention"){
    #Apply intervention costs, this applies from model entry to death
    pat_chars_[mask_,"Costs"] <- pat_chars_[mask_,"Costs"] +  this_event*parameter_[,"InterventionCost"]
    pat_chars_[mask_,"Discounted Costs"] <- pat_chars_[mask_,"Discounted Costs"] + 
      parameter_[,"InterventionCost"]*Disc_LE(this_event,0, DRC)
  }
  ##Remove the calculated variables here
  rm(this_event, last_event, DRC, DRQ)
  ###After this is done return the patient characteristics matrix
  return(pat_chars_[mask_,])
}