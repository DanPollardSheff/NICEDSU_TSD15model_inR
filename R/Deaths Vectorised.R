####Death Code
#'@description this function calculates costs and QALYs between death and a patient's previous event. This function is vectorised so it applies to 
#'to multiple patients at once. QALYs are calculated from patient's last event, as utility changes over time. Intervention costs are calculated from time of
#'model entry as other events in the model do not effect the intervention costs
#'@units Times within this function are in years 
#'@param pat_chars_ is the patient characteristics matrix from the simulation. The pat_chars_ matrix must contain columns: QALYs, Discounted QALYs, Costs, Discounted Costs, and Utility.
#'@param GlobalOptions_, is the matrix with the Global Options from the data folder
#'@param parameter_ is the single row of model parameters matrix (still a matrix object, not a vector) for this model run [single row of parameters.csv in the data folder]
#'@param Treatment_ is a text variable indicating which arm to run (either "Intervention" for intervention or anything else for control)
#'@param last_event_ is a numeric vector that indicates the time of the last event for each patient passed through to this function
#'@param this_event_ is a numeric vector that indicates the time of the death event for each patient passed through to this function
#'@param mask_ is a TRUE/FALSE vector. TRUE for for patients who died in this event cycle. This must be the same length as the pat_chars matrix.
#'@return pat_chars_[mask_] is the subset of the patient's for whom we have applied these calculations upon their deaths
#'@dependecies \code {Disc_LE} user defined function in this repository. This must be loaded within the global environment for this function to work.

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