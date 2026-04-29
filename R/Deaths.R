####Death Code
#'@description this function calculates costs and QALYs between death and a patient's previous event. This function is vectorised so it applies to 
#'to multiple patients at once. QALYs are calculated from patient's last event, as utility changes over time. Intervention costs are calculated from time of
#' model entry as other events in the model do not effect the intervention costs
#'@param pat_chars_ is the patient characteristics matrix from the simulation. The pat_chars_ matrix must contain columns: QALYs, Discounted QALYs, Costs, Discounted Costs, and Utility.
#'@param GlobalOptions_, is the matrix with the Global Options from the data folder
#'@param i_ is the current patient we are testing in the loop
#'@param events_ is the ordered vector of events, with events after death set to missing
#'@param y_ is the number of the current event
#'@param parameter_ is the single row of model parameters matrix for this model run [single row of parameters.csv in the data folder]
#'@param Treatment_ is a text variable indicating which arm to run (either "Intervention" for intervention or anything else for control)
#'@return pat_chars_ is the full patient characteristics matrix after we have done our calculations
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