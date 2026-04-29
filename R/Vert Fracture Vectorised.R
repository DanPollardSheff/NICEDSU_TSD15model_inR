#####Vert Fracture code
#'@description this function calculates costs and QALYs between vert fracture (either 1st or second) and a patient's previous event. This function is vectorised so it applies to 
#'to multiple patients at once. QALYs are calculated from patient's last event, as utility changes over time with other events. Utility only changes upon 1st vert fracture, and does not change with the incidence of a second vert fracture. 
#'Costs are one off costs associated with the occurrence of the vert fracture.
#'@units Times within this function are in years 
#'@param pat_chars_ is the patient characteristics matrix from the simulation. The pat_chars_ matrix must contain columns: QALYs, Discounted QALYs, Costs, Discounted Costs, and Utility.
#'@param GlobalOptions_, is the matrix with the Global Options from the data folder
#'@param parameter_ is the single row of model parameters matrix (still a matrix object, not a vector) for this model run [single row of parameters.csv in the data folder]
#'@param last_event_ is a numeric vector that indicates the time of the last event for each patient passed through to this function
#'@param this_event_ is a numeric vector that indicates the time of the death event for each patient passed through to this function
#'@param mask_ is a TRUE/FALSE vector. TRUE for for patients who died in this event cycle. This must be the same length as the pat_chars matrix.
#'@return pat_chars_[mask_] is the subset of the patient's for whom we have applied these calculations upon their deaths
#'@dependecies \code {Disc_LE}{Disc_factor} user defined function in this repository to apply one-off and continuous time discounting. This must be loaded within the global environment for this function to work.


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
