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



Single_model_run <- function(
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
  
  ###Step 3: Run the model
  ###Loop over the patients one by one
  for (i in 1:as.numeric(GlobalOptions["Number_of_patients", "Value"])){
    #Work out the event order for this patient
    events <- subset(pat_chars_, select = c("Time_1st_Vertfracture","Time_2nd_Vertfracture",
                                           "Time_Hipfracture", "Time_Death"))
    #Select the right patient
    events <- events[i,]
    #Order the events by time
    events <- sort(events)
    #Set any event post-death to be missing
    events <- ifelse(events[]>events["Time_Death"], NA, events[])
    ###Count the number of events
    numb_events <- sum(ifelse(is.na(events),0,1))
    
    #initialise y
    y <- 1
    
    ##Loop over the events+++
    while(y <= numb_events){
      
      ##Compare the name of this event and see if it is a vert fracture, hip fracture or death
      #Vert Fracture, either first or 2nd
      if(names(events[y])=="Time_1st_Vertfracture"|
         names(events[y])=="Time_2nd_Vertfracture"){
        pat_chars_ <-  VertFracture(pat_chars_,i,events,y,GlobalOptions,parameter_)  
      }
      #Hip fracture
      else if(names(events[y])=="Time_Hipfracture"){
        pat_chars_ <-  HipFracture(pat_chars_,i,events,y,GlobalOptions, parameter_, Treatment_)  
        #if the patient has an instant death, set y to a very large number so the loop ends
        if(is.na(pat_chars_[i,"instanaeous_death_hip"])==F){ #Check if I add any data to the instanaeous_death_hip column, 
          #if I do don't look for any more events by setting y to a big number
          y <- numb_events
        }
      }#Death
      else{
        pat_chars_ <-  Death(pat_chars_,i,events,y,GlobalOptions, parameter_, Treatment_)  
      }
      #Increment y by 1 to move onto the next event
      y <- y + 1
    }
  }
  
  ##Step 4 : Collect the results
    Results[1,"Life Expectancy"]             <- mean(pat_chars_[,"Time_Death"])
    Results[1,"QALYs"]                       <- mean(pat_chars_[,"QALYs"])
    Results[1,"Discounted QALYs"]            <- mean(pat_chars_[,"Discounted QALYs"])
    Results[1,"Costs"]                       <- mean(pat_chars_[,"Costs"])
    Results[1,"Discounted Costs"]            <- mean(pat_chars_[,"Discounted Costs"])
    Results[1,"Mean Time Vert"]              <- mean(pat_chars_[,"Time_1st_Vertfracture"])
    Results[1,"Mean Time Hip"]               <- mean(pat_chars_[,"Time_Hipfracture"])
  
  return(Results)
}