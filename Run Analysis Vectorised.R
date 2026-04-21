StartTime <- Sys.time()
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

source("R/Hip Fracture Vectorised.R")
source("R/Vert Fracture Vectorised.R")
source("R/Deaths Vectorised.R")
source("R/Single Model Run Vectorised.R")
source("R/Run Model Vectorised.R")

#Make the Results folder if it does not exist alread
if(!dir.exists("Results")){
  dir.create("Results")
}

##Base Case
##Intervention
BC_intervention <- run_model_vec(Params,
                             pat_chars,
                             "Intervention",
                             GlobalOptions,
                             ResultsVariables)

write.csv(BC_intervention, "Results/BaseCaseResultsInterventionDet.csv")
BC_control <- run_model_vec(Params,
                        pat_chars,
                        "baseline",
                        GlobalOptions,
                        ResultsVariables)
write.csv(BC_control, "Results/BaseCaseResultsControlDet.csv")

##For real tests uncomment the next line of code - currently only doing 16 PSA runs
GlobalOptions["Number_of_PSARuns", "Value"] <- 1000
GlobalOptions["Run_PSA", "Value"] <- TRUE
BC_intervention <- run_model_vec(Params,
                             pat_chars,
                             "Intervention",
                             GlobalOptions,
                             ResultsVariables)

write.csv(BC_intervention, "Results/BaseCaseResultsInterventionPSA.csv")
BC_control <- run_model_vec(Params,
                        pat_chars,
                        "baseline",
                        GlobalOptions,
                        ResultsVariables)
write.csv(BC_control, "Results/BaseCaseResultsControlPSA.csv")

##Assuming it is non-linear
##Scenario 1.5% discounting
GlobalOptions["Discount_rate_cost", "Value"] <- 0.015
GlobalOptions["Discount_rate_QALY", "Value"] <- 0.015

DR_1.5_intervention <- run_model_vec(Params,
                             pat_chars,
                             "Intervention",
                             GlobalOptions,
                             ResultsVariables)

write.csv(DR_1.5_intervention, "Results/1.5percdiscInterventionPSA.csv")

DR_1.5_control <- run_model_vec(Params,
                        pat_chars,
                        "baseline",
                        GlobalOptions,
                        ResultsVariables)
write.csv(DR_1.5_control, "Results/1.5percdiscResultsControlPSA.csv")
EndTime <- Sys.time()
VecTime <- as.numeric(EndTime - StartTime, units = "secs")
VecTime