#####Hip Fracture code
#'@description this function calculates costs and QALYs between hip fracture and a patient's previous event. This function is vectorised so it applies to 
#'to multiple patients at once. QALYs are calculated from patient's last event, as utility changes over time. Costs are one off costs associated with the occurance of 
#'the hip fracture
#'@units Times within this function are in years 
#'@param pat_chars_ is the patient characteristics matrix from the simulation. The pat_chars_ matrix must contain columns: QALYs, Discounted QALYs, Costs, Discounted Costs, and Utility.
#'@param GlobalOptions_, is the matrix with the Global Options from the data folder
#'@param parameter_ is the single row of model parameters matrix (still a matrix object, not a vector) for this model run [single row of parameters.csv in the data folder]
#'@param Treatment_ is a text variable indicating which arm to run (either "Intervention" for intervention or anything else for control)
#'@param last_event_ is a numeric vector that indicates the time of the last event for each patient passed through to this function
#'@param this_event_ is a numeric vector that indicates the time of the death event for each patient passed through to this function
#'@param mask_ is a TRUE/FALSE vector. TRUE for for patients who died in this event cycle. This must be the same length as the pat_chars matrix.
#'@return pat_chars_ is the full patient's characteristics matrix after doing calculations
#'@dependecies \code {Disc_LE}{Disc_factor} user defined function in this repository to apply one-off and continuous time discounting. This must be loaded within the global environment for this function to work.

HipFracture <- function (pat_chars_, i_, events_, y_, GlobalOptions_, parameter_, Treatment_){
  #Extract the time of the hip fracture
  time_hip <- events_[y_]
  #Set the time of the last event, which is y_-1 event if we are not assessing the first event, 0 otherwise
  time_last_event <- ifelse(y_>1,events_[(y_-1)],0)
  #Get the discount rates as numbers
  DRC <- as.numeric(GlobalOptions_["Discount_rate_cost","Value"])
  DRQ <- as.numeric(GlobalOptions_["Discount_rate_QALY","Value"])
  #Undiscounted Cost
  pat_chars_[i_,"Costs"] <- pat_chars_[i_,"Costs"] + parameter_[,"Hip_Fracture_Cost"]
  #Discounted Cost
  pat_chars_[i_,"Discounted Costs"] <- pat_chars_[i_,"Discounted Costs"] + (as.numeric(parameter_[,"Hip_Fracture_Cost"])*Disc_factor(time_hip,DRC))  
  #Undiscounted QALYs
  pat_chars_[i_,"QALYs"] <- pat_chars_[i_,"QALYs"] + (time_hip-time_last_event)*pat_chars_[i_,"Utility"]
  #Discounted QALYs
  pat_chars_[i_,"Discounted QALYs"] <- pat_chars_[i_,"Discounted QALYs"] + 
    Disc_LE(time_hip,time_last_event, DRQ)*pat_chars_[i_,"Utility"]
  ##Only change the patient's utility if this is their first vert fracture
  ###Apply treatment costs if they die and we are running an intervention arm
  if(runif(1) < parameter_[,"Mort_Prop_Hip"]){
    ###Put a number in for this patient if they died immediately
    pat_chars_[i_,"instanaeous_death_hip"] <- 1
    ###For safety change & easier bug checking set their time of death to the time of the hip fracture
    pat_chars_[i_,"Time_Death"] <- pat_chars_[i_,"Time_Hipfracture"]
    if(Treatment_ == "Intervention"){
      #Apply intervention costs if they die instantly, this applies from model entry
      pat_chars_[i_,"Costs"] <- pat_chars_[i_,"Costs"] +  time_hip*parameter_[,"InterventionCost"]
      pat_chars_[i_,"Discounted Costs"] <- pat_chars_[i_,"Discounted Costs"] + 
        parameter_[,"InterventionCost"]*Disc_LE(time_hip,0, DRC)
    }
  }
  ##Amend the Utility of patients with hip fracture
  pat_chars_[i_,"Utility"] <- pat_chars_[i_,"Utility"]*parameter_[,"Hip_Utility_Multiplier"]
  ##Remove the calculated variables here
  rm(time_hip, time_last_event, DRC, DRQ)
  ###After this is done return the patient characteristics matrix
  return(pat_chars_)
}