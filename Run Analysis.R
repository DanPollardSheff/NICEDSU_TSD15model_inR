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
source("R/Single Model Run.R")

set.seed(123)

test <- Single_model_run(
  j_ = 1,
  parameter_ = Params,
  pat_chars_ = pat_chars,
  Treatment_ = Treatment,
  GlobalOptions_ = GlobalOptions,
  ResultsVariables_ = ResultsVariables
                         )


test