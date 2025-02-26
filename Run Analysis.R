####Read in data
Params <- read.csv("Data/Parameters.csv")
GlobalOptions <- read.csv("Data/Global Options.csv", row.names = 1)

###Create the patient characteristics
Individual_variables <- c("Time_1st_Vertfracture", "Time_2nd_Vertfracture", "Time_Hipfracture",
                          "Time_Death", "instanaeous_death_hip", "Costs", "Discounted Costs",
                          "QALYs", "Discounted QALYs", " Discounted Life Years", "Utility", "Number of Vert Fractures")
#Set up a matrix with one row for each patient and the number of columns equal to the number of datapoints we are collecting
pat_chars <- matrix(data = NA, nrow = as.numeric(GlobalOptions["Number_of_patients", "Value"]), 
                    ncol = length(Individual_variables))

##Assigning names to the pat_chars matrix
colnames(pat_chars) <- Individual_variables

###
ResultsVariables <- c("Life Expectancy", "Discounted Life Expectancy" ,"QALYs", 
                 "Discounted QALYs", "Costs", "Discounted Costs", "Mean Time Vert", "Mean Time Hip")

#Temp variable for number of rows we will need, 1 if deterministic, 
#1 for each PSA iteration if not

Temp <- ifelse(GlobalOptions["Run_PSA", "Value"]==TRUE, 
               as.numeric(GlobalOptions["Number_of_PSARuns","Value"]),
                          1)

#Set up the results matrix, which will store the model averaged results
Results <- matrix(data = NA, nrow = Temp, ncol = length(ResultsVariables))

###Assign names to the results matrix
colnames(Results) <- ResultsVariables

####Step 1a: Set up a text variable to control whether we are using intervention or control
Treatment <- "Baseline"
####Step 1b: Call in our user defined functions for this model
source("R/Continuous Discounting.R")
source("R/Standard Discounting.R")
source("R/Deaths.R")
source("R/Hip Fracture.R")
source("R/Vert Fracture.R")

###### create a loop so the model can iterate across PSA runs, or run once if it is deterministic

#Step 2: Select the Correct parameters for this model run
j <- 1 #Use this variable to count the current row number I want from the parameter matrix (this will change later)
parameter <- Params[j,] # save a vector called parameter that is a single row of the parameters matrix

###Step 2a: Sample the time of events for each individual
pat_chars[,"Time_1st_Vertfracture"] <- qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                             shape = parameter[,"DistVertWeibShape"],
                                             scale = parameter[,"DistVertWeibScale"])

pat_chars[,"Time_Hipfracture"] <- qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                           shape = parameter[,"DistHipWeibShape"],
                                           scale = parameter[,"DistHipWeibScale"])

pat_chars[,"Time_Death"] <- qnorm(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
                                        parameter[,"DeathDist_Norm_Mean"],
                                        parameter[,"DeathDist_Norm_SE"])
##Applying intervention effects
if(Treatment == "Intervention"){
  #Double the vert and hip fracture times
  pat_chars[,"Time_1st_Vertfracture"] <- pat_chars[,"Time_1st_Vertfracture"]*parameter[,"Multiplier_of_treatment"]
  pat_chars[,"Time_Hipfracture"] <- pat_chars[,"Time_Hipfracture"]*parameter[,"Multiplier_of_treatment"]
  #Add on a sample of the time until 2nd vert fracture (with treatment effect applied)
  #To the time of the first vert fractuce
  pat_chars[,"Time_2nd_Vertfracture"] <- pat_chars[,"Time_1st_Vertfracture"] + # add on a new time to event to the original vert fracture
    qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
             parameter[,"DistVertWeibShape"],
             parameter[,"DistVertWeibScale"])*parameter[,"Multiplier_of_treatment"]
}else{
##If none of that is true just take another random sample of hte time to vert fracture
##and add it onto the time of the first vert fracture
pat_chars[,"Time_2nd_Vertfracture"] <- pat_chars[,"Time_1st_Vertfracture"] + # add on a new time to event to the original vert fracture
  qweibull(runif(as.numeric(GlobalOptions["Number_of_patients", "Value"])),
           parameter[,"DistVertWeibShape"],
           parameter[,"DistVertWeibScale"])
}
###Step 2b: Set the other baseline characteristics 
pat_chars[,"Costs"] <- 0
pat_chars[,"Discounted Costs"] <- 0
pat_chars[,"QALYs"] <- 0
pat_chars[,"Discounted QALYs"] <- 0
pat_chars[," Discounted Life Years"] <- 0
pat_chars[,"Utility"]<- parameter[,"BaselineUtility"]
pat_chars[,"Number of Vert Fractures"] <- 0

###Step 3: Run the model
###Loop over the patients one by one
for (i in 1:as.numeric(GlobalOptions["Number_of_patients", "Value"])){
#Work out the event order for this patient
  events <- subset(pat_chars, select = c("Time_1st_Vertfracture","Time_2nd_Vertfracture",
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
  pat_chars <-  VertFracture(pat_chars,i,events,y,GlobalOptions,parameter)  
  }
    #Hip fracture
    else if(names(events[y])=="Time_Hipfracture"){
      pat_chars <-  HipFracture(pat_chars,i,events,y,GlobalOptions, parameter, Treatment)  
      #if the patient has an instant death, set y to a very large number so the loop ends
      if(is.na(pat_chars[i,"instanaeous_death_hip"])==F){ #Check if I add any data to the instanaeous_death_hip column, 
      #if I do don't look for any more events by setting y to a big number
      y <- numb_events
      }
    }#Death
    else{
      pat_chars <-  Death(pat_chars,i,events,y,GlobalOptions, parameter, Treatment)  
    }
    #Increment y by 1 to move onto the next event
    y <- y + 1
  }
}

##Step 4 : Collect the results

if(GlobalOptions["Run_PSA","Value"]==TRUE){
  Results[j-1,"Life Expectancy"]             <- mean(pat_chars[,"Time_Death"])
  Results[j-1,"QALYs"]                       <- mean(pat_chars[,"QALYs"])
  Results[j-1,"Discounted QALYs"]            <- mean(pat_chars[,"Discounted QALYs"])
  Results[j-1,"Costs"]                       <- mean(pat_chars[,"Costs"])
  Results[j-1,"Discounted Costs"]            <- mean(pat_chars[,"Discounted Costs"])
  Results[j-1,"Mean Time Vert"]              <- mean(pat_chars[,"Time_1st_Vertfracture"])
  Results[j-1,"Mean Time Hip"]               <- mean(pat_chars[,"Time_Hipfracture"])
  
}else{
  Results[1,"Life Expectancy"]             <- mean(pat_chars[,"Time_Death"])
  Results[1,"QALYs"]                       <- mean(pat_chars[,"QALYs"])
  Results[1,"Discounted QALYs"]            <- mean(pat_chars[,"Discounted QALYs"])
  Results[1,"Costs"]                       <- mean(pat_chars[,"Costs"])
  Results[1,"Discounted Costs"]            <- mean(pat_chars[,"Discounted Costs"])
  Results[1,"Mean Time Vert"]              <- mean(pat_chars[,"Time_1st_Vertfracture"])
  Results[1,"Mean Time Hip"]               <- mean(pat_chars[,"Time_Hipfracture"])
}

