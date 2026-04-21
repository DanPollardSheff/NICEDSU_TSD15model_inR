#'@param j_ is a counting variable, that indicates which row to use from the 
#'parameters matrix. This controls which set of random variables to use in the analysis
#'@param parameter_, is the full parameters matrix
#'@param pat_chars_, is the patient characteristics matrix
#'@param Treatment_, is the variable that controls whether treatment effects are applied or not
#'Intervention means the model is running an intervention arm. Anything else and intervention
#'effects are not applied
#'@param GlobalOptions_ is the data object containing all global options for the model
#'e.g. discount rates, variables that control the number of patients, control variables 
#'to determine if a scenario analysis is being conducted or not
#'@param ResultsVariables_, is a list of all variables that I am collecting results for
#'
#'@return Results

Single_model_run_vectorised <- function(
    j_,
    parameter_,
    pat_chars_,
    Treatment_,
    GlobalOptions_,
    ResultsVariables_){
  
  #Step 1: Make a matrix to save summary results into
  #Set up the results matrix (single row), which will store the model averaged results
  Results <- matrix(data = NA, nrow = 1, ncol = length(ResultsVariables_))
  ###Assign names to the results matrix
  colnames(Results) <- ResultsVariables_  
  
  #Step 2: Select the Correct parameters for this model run
  parameter_ <- parameter_[j_,] # save a vector called parameter that is a single row of the parameters matrix

  ###Step 2a: Sample the time of events for each individual
  pat_chars_[,"Time_1st_Vertfracture"] <- qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                                  shape = parameter_[,"DistVertWeibShape"],
                                                  scale = parameter_[,"DistVertWeibScale"])
  
  pat_chars_[,"Time_Hipfracture"] <- qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                             shape = parameter_[,"DistHipWeibShape"],
                                             scale = parameter_[,"DistHipWeibScale"])
  
  pat_chars_[,"Time_Death"] <- qnorm(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                    parameter_[,"DeathDist_Norm_Mean"],
                                    parameter_[,"DeathDist_Norm_SE"])
  ##Applying intervention effects
  if(Treatment_ == "Intervention"){
    #Double the vert and hip fracture times
    pat_chars_[,"Time_1st_Vertfracture"] <- pat_chars_[,"Time_1st_Vertfracture"]*parameter_[,"Multiplier_of_treatment"]
    pat_chars_[,"Time_Hipfracture"] <- pat_chars_[,"Time_Hipfracture"]*parameter_[,"Multiplier_of_treatment"]
    #Add on a sample of the time until 2nd vert fracture (with treatment effect applied)
    #To the time of the first vert fractuce
    pat_chars_[,"Time_2nd_Vertfracture"] <- pat_chars_[,"Time_1st_Vertfracture"] + # add on a new time to event to the original vert fracture
      qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
               parameter_[,"DistVertWeibShape"],
               parameter_[,"DistVertWeibScale"])*parameter_[,"Multiplier_of_treatment"]
  }else{
    ##If none of that is true just take another random sample of hte time to vert fracture
    ##and add it onto the time of the first vert fracture
    pat_chars_[,"Time_2nd_Vertfracture"] <- pat_chars_[,"Time_1st_Vertfracture"] + # add on a new time to event to the original vert fracture
      qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
               parameter_[,"DistVertWeibShape"],
               parameter_[,"DistVertWeibScale"])
  }
  ###Step 2b: Set the other baseline characteristics 
  pat_chars_[,"Costs"] <- 0
  pat_chars_[,"Discounted Costs"] <- 0
  pat_chars_[,"QALYs"] <- 0
  pat_chars_[,"Discounted QALYs"] <- 0
  pat_chars_[," Discounted Life Years"] <- 0
  pat_chars_[,"Utility"]<- parameter_[,"BaselineUtility"]
  pat_chars_[,"Number of Vert Fractures"] <- 0
  
  ##stash the event times (model code sets these to missing)
  HipFractureTime <- pat_chars_[,"Time_Hipfracture"]
  Vert1FractureTime <- pat_chars_[,"Time_1st_Vertfracture"]
  Vert2FractureTime <- pat_chars_[,"Time_2nd_Vertfracture"]
  DeathTime <- pat_chars_[,"Time_Death"]
  
  ###Step 3: Run the model
  ###start counting events
  current_time <- rep(0, length(pat_chars_[,"Utility"]))
  
  
  ##Apply instantaenous death from hip fracture
  #Give 0 prob to anyone who dies before their hip fracture
  p_instantdeath <- ifelse(pat_chars_[,"Time_Hipfracture"] < pat_chars_[,"Time_Death"],
                           parameter_[,"Mort_Prop_Hip"],
                           0)
  #Record deaths by comapring a uniform random number to the prob of instant death 
  instantdeath <- runif(length(pat_chars_[,"Time_Hipfracture"] )) < p_instantdeath
  
  #Record the instant death
  pat_chars_[instantdeath,"instanaeous_death_hip"] <- 1
  #Change the time of death to be time of hip +  a very small number (I've approximated with 1/ 10 million)
  pat_chars_[instantdeath, "Time_Death"] <- pat_chars_[instantdeath,"Time_Hipfracture"] + (1/10000000)
  
  ##Set all events post-death to be missing
  ##For hip the inequality is less than or equals, as they can die instantly
  pat_chars_[,"Time_Hipfracture"] <- ifelse(pat_chars_[,"Time_Hipfracture"] <= pat_chars_[,"Time_Death"], 
                                            pat_chars_[,"Time_Hipfracture"],
                                            NA)
  
  pat_chars_[,"Time_1st_Vertfracture"] <- ifelse(pat_chars_[,"Time_1st_Vertfracture"] < pat_chars_[,"Time_Death"], 
                                                pat_chars_[,"Time_1st_Vertfracture"],
                                                NA)
  
  pat_chars_[,"Time_2nd_Vertfracture"] <- ifelse(pat_chars_[,"Time_2nd_Vertfracture"] < pat_chars_[,"Time_Death"], 
                                                pat_chars_[,"Time_2nd_Vertfracture"],
                                                NA)
  
  
  
  #Extract the event times only
  event_times <- pat_chars_[,c("Time_Hipfracture","Time_1st_Vertfracture", "Time_2nd_Vertfracture", "Time_Death")]
  #Calculate the maximum number of events (for flexibility)
  max_events <- ncol(event_times)
  #create an alive vector, TRUE for everyone
  alive <- pat_chars_[,"Time_Death"] >-1
  
 
  
  for(event_step in 1:max_events) {
    
    # Identify who has the earliest event of each type
    next_event_indices <- apply(event_times, 1, function(x) {
      if (all(is.na(x))) {
        return(4) # Assume death/no further events
      } else {
        return(which.min(x))
      }
    })
    
    next_event_times <- event_times[cbind((1:nrow(event_times)), next_event_indices)]
    
    #create logical vectors for each type of event
    Hip   <- next_event_indices==1 & alive
    Vert  <- (next_event_indices==2|next_event_indices==3) & alive 
    Death <- next_event_indices==4 & alive 
    
    
    #Apply code for based on the next event
    pat_chars_[Hip,] <- HipFractureVec(pat_chars_,GlobalOptions_,parameter_,
                                      Treatment_,current_time,next_event_times,
                                      Hip)
    
    pat_chars_[Vert,] <- VertFractureVec(pat_chars_,GlobalOptions_,parameter_,
                                         current_time,next_event_times,
                                         Vert)
    
    pat_chars_[Death,] <- DeathVec(pat_chars_,GlobalOptions_,parameter_,
                                   Treatment_,current_time,next_event_times,
                                   Death)
    
    
    #Reset current time to be current event
    current_time[alive] <- next_event_times[alive]
    
    #Set current event to 100000000
    event_times[cbind((1:nrow(event_times)), next_event_indices)][alive] <- NA
    
    #If alive and dead this event change vector to FALSE, otherwise leave it as it is
    alive <- ifelse(Death,FALSE, alive)
  }
  
  ##Reset time data - for results
  pat_chars_[,"Time_Hipfracture"] <- HipFractureTime
  pat_chars_[,"Time_1st_Vertfracture"] <- Vert1FractureTime
  pat_chars_[,"Time_2nd_Vertfracture"] <- Vert2FractureTime
  pat_chars_[,"Time_Death"] <- DeathTime
  
  
  ##Step 4 : Collect the results
    Results[1,"Life Expectancy"]             <- mean(pat_chars_[,"Time_Death"])
    Results[1,"QALYs"]                       <- mean(pat_chars_[,"QALYs"])
    Results[1,"Discounted QALYs"]            <- mean(pat_chars_[,"Discounted QALYs"])
    Results[1,"Costs"]                       <- mean(pat_chars_[,"Costs"])
    Results[1,"Discounted Costs"]            <- mean(pat_chars_[,"Discounted Costs"])
    Results[1,"Mean Time Vert"]              <- mean(pat_chars_[,"Time_1st_Vertfracture"],na.rm=T)
    Results[1,"Mean Time Hip"]               <- mean(pat_chars_[,"Time_Hipfracture"],na.rm=T)
  
  return(Results)
}