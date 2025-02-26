####Death Code
#'@param pat_chars_ is the patient charactersitics matrix
#'@param i_ is the current patient we are testing in the loop
#'@param events_ is the ordered vector of events, with events after death set to missing
#'@param y_ is the number of the current event
#'@param GlobalOptions_, is the matrix with the Global Options
#'@param parameter_ is the vector of model parameters for this model run
#'@param Treatment_ is a text varaible indicating which arm to run
#'@return pat_chars_ is the patient charactersitics matrix after we have done our calculations

Death <- function (pat_chars_, i_, events_, y_, GlobalOptions_, parameter_,Treatment_){
  #Extract the time of the vert fracture
  time_death <- events_[y_]
  #Set the time of the last event, which is y_-1 event if we are not assessing the first event, 0 otherwise
  time_last_event <- ifelse(y_>1,events_[(y_-1)],0)
  #Get the discount rates as numbers
  DRC <- as.numeric(GlobalOptions_["Discount_rate_cost","Value"])
  DRQ <- as.numeric(GlobalOptions_["Discount_rate_QALY","Value"])
  
  #Undiscounted QALYs
  pat_chars_[i_,"QALYs"] <- pat_chars_[i_,"QALYs"] + (time_death-time_last_event)*pat_chars_[i_,"Utility"]
  #Discounted QALYs
  pat_chars_[i_,"Discounted QALYs"] <- pat_chars_[i_,"Discounted QALYs"] + 
    Disc_LE(time_death,time_last_event, DRQ)*pat_chars_[i_,"Utility"]
  ##Only change the patient's utility if this is their first vert fracture
  if(Treatment_ == "Intervention"){
    #Apply intervention costs if they die instantly, this applies from model entry
    pat_chars_[i_,"Costs"] <- pat_chars_[i_,"Costs"] +  time_death*parameter_[,"InterventionCost"]
    pat_chars_[i_,"Discounted Costs"] <- pat_chars_[i_,"Discounted Costs"] + 
      parameter_[,"InterventionCost"]*Disc_LE(time_death,0, DRC)
  }
  ##Remove the calculated variables here
  rm(time_death, time_last_event, DRC, DRQ)
  ###After this is done return the patient characteristics matrix
  return(pat_chars_)
}