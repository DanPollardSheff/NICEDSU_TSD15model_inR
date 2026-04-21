##load necessary libraries
library(parallel)
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

####Step 1a: Set up a text variable to control whether we are using intervention or control
Treatment <- "Baseline"
####Step 1b: Call in our user defined functions for this model
source("R/Continuous Discounting.R")
source("R/Standard Discounting.R")
source("R/Deaths.R")
source("R/Hip Fracture.R")
source("R/Vert Fracture.R")
source("R/Single Model Run.R")
source("R/Run Model.R")
#Load in the vectorised model code
source("R/Hip Fracture Vectorised.R")
source("R/Vert Fracture Vectorised.R")
source("R/Deaths Vectorised.R")
source("R/Single Model Run Vectorised.R")
source("R/Run Model Vectorised.R")


##Vectorised code
set.seed(123)
StartTime <- Sys.time()
Test1 <- Single_model_run_vectorised(1,
                                     Params,
                                     pat_chars, 
                                     "Intervention",
                                     GlobalOptions, 
                                     ResultsVariables)
EndTime <- Sys.time()
VecTime <- as.numeric(EndTime - StartTime, units = "secs")

#Loop code
set.seed(123)
Test2 <- Single_model_run(1,
                          Params,
                          pat_chars, 
                          "Intervention",
                          GlobalOptions, 
                          ResultsVariables)
EndTime <- Sys.time()
LoopTime <- as.numeric(EndTime - StartTime, units = "secs")

VecTime/LoopTime