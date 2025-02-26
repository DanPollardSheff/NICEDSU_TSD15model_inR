#####Vert Fracture code
#'@param pat_chars_ is the patient characteristics matrix
#'@param i_ is the current patient we are testing in the loop
#'@param events_ is the ordered vector of events, with events after death set to missing
#'@param y_ is the number of the current event
#'@param GlobalOptions_, is the matrix with the Global Options
#'@param parameter_ is the vector of model parameters for this model run
#'@return pat_chars_ is the patient characteristics matrix after we have done our calculations

VertFracture <- function (pat_chars_, i_, events_, y_, GlobalOptions_, parameter_){
  #Extract the time of the vert fracture
  time_vert <- events_[y_]
  #Set the time of the last event, which is y_-1 event if we are not assessing the first event, 0 otherwise
  time_last_event <- as.numeric(ifelse(y_>1,events_[(y_-1)],0))
  #Get the discount rates as numbers
  DRC <- as.numeric(GlobalOptions_["Discount_rate_cost","Value"])
  DRQ <- as.numeric(GlobalOptions_["Discount_rate_QALY","Value"])
  #Undiscounted Cost
  pat_chars_[i_,"Costs"] <- pat_chars_[i_,"Costs"] + parameter_[,"Vert_Fracture_Cost"]
  #Discounted Cost
  pat_chars_[i_,"Discounted Costs"] <- pat_chars_[i_,"Discounted Costs"] + (as.numeric(parameter_[,"Vert_Fracture_Cost"])*Disc_factor(time_vert,DRC))
  #Undiscounted QALYs
  pat_chars_[i_,"QALYs"] <- pat_chars_[i_,"QALYs"] + (time_vert-time_last_event)*pat_chars_[i_,"Utility"]
  #Discounted QALYs
  pat_chars_[i_,"Discounted QALYs"] <- pat_chars_[i_,"Discounted QALYs"] + 
    Disc_LE(time_vert,time_last_event, DRQ)*pat_chars_[i_,"Utility"]
  ##Only change the patient's utility if this is their first vert fracture
  if(names(events_[y_])=="Time_1st_Vertfracture"){
    pat_chars_[i_,"Utility"]<- pat_chars_[i_,"Utility"]*parameter_[,"Vert_Utility_Multiplier"]
  }
  ##Remove the calculated varaibles here
  rm(time_vert, time_last_event, DRC, DRQ)
  ###After this is done return the patient characteristics matrix
  return(pat_chars_)
}
