#####Vert Fracture code
#'@param pat_chars_ is the patient characteristics matrix
#'@param GlobalOptions_, is the matrix with the Global Options
#'@param parameter_ is the vector of model parameters for this model run
#'@param last_event_ is a number that indicates the time of the last event
#'@param next_event is a number that indicates the time of the next event
#'@param mask_ is a TRUE/FALSE vector indicating which patients to apply these calculations to
#'@return pat_chars_ is the patient charactersitics matrix after we have done our calculations

VertFractureVec <- function (pat_chars_, GlobalOptions_, parameter_,last_event_, this_event_, mask_){
  
  #Get the discount rates as numbers
  DRC <- as.numeric(GlobalOptions_["Discount_rate_cost","Value"])
  DRQ <- as.numeric(GlobalOptions_["Discount_rate_QALY","Value"])
  
  #Exclude dead peopl from last_event and this_event
  last_event <- last_event_[mask_]
  this_event <- this_event_[mask_]
  
  #Undiscounted Cost
  pat_chars_[mask_,"Costs"] <- pat_chars_[mask_,"Costs"] + parameter_[,"Vert_Fracture_Cost"]
  #Discounted Cost
  pat_chars_[mask_,"Discounted Costs"] <- pat_chars_[mask_,"Discounted Costs"] + (as.numeric(parameter_[,"Vert_Fracture_Cost"])*Disc_factor(this_event,DRC))
  #Undiscounted QALYs
  pat_chars_[mask_,"QALYs"] <- pat_chars_[mask_,"QALYs"] + (this_event-last_event)*pat_chars_[mask_,"Utility"]
  #Discounted QALYs
  pat_chars_[mask_,"Discounted QALYs"] <- pat_chars_[mask_,"Discounted QALYs"] + 
    Disc_LE(this_event,last_event, DRQ)*pat_chars_[mask_,"Utility"]
  ##Only change the patient's utility if this is their first vert fracture
  #If it is a 1st vert fracture, 1st vert fracture will be missing
  #as this is logical, do this for every patient, we'll subset later
  vert1 <- pat_chars_[mask_,"Number of Vert Fractures"]==0
  
  #Change the multiplier for people with a 1st Vert Fracture
  pat_chars_[vert1,"Utility"]<- pat_chars_[vert1,"Utility"]*parameter_[,"Vert_Utility_Multiplier"]
  #Change the Number of Vert Fractures to 1 (N.B this can be anything that isn't missing)
  pat_chars_[vert1,"Number of Vert Fractures"] <- 1
  
  
  ##Remove the calculated varaibles here
  rm(this_event, last_event, DRC, DRQ)
  ###After this is done return the patient characteristics matrix
  return(pat_chars_[mask_,])
}
