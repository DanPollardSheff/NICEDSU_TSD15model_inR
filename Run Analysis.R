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

##PSA - single thread
set.seed(1)
start.time <- Sys.time()
test_PSA <- sapply(2:17, #note we now start on 2, as row 2 onwards as PSA samples! Here it is just so you can see that we are getting the same reuslts in sapply as running the function
                   Single_model_run, #Function, but only the name
                     parameter_ = Params, #Set each argument of the function to the right data
                     pat_chars_ = pat_chars, #use the same notation as if we were running the function normally
                     Treatment_ = Treatment,
                     GlobalOptions_ = GlobalOptions,
                     ResultsVariables_ = ResultsVariables
                   )
end.time <- Sys.time()
##Print run time
time_psa_singlethread <- end.time - start.time
time_psa_singlethread
test_PSA

#PSA - parallel processing


start.time <- Sys.time()
#set up parrallel processing
#these are the additional global variables that need to be set for parrallel
#processing functions
numCores <- detectCores()-1 #detect the number of cores on my current PC
#set the number of cores to use to be the minimum of the number in the GlobalOptions.csv 
#file or the number of detected cores on this PC. This is because the PC can crash if it 
#runs out of memory from running too many models at once.
numCores <- min (numCores, as.numeric(GlobalOptions["Number_of_cores", "Value"])) 

##Next 3 lines of code set up clustering (technical thing needed for parallel processing)
cl <- makeCluster(numCores)
#Set a random number seed for parallel processing
clusterEvalQ(cl, set.seed(1))

#Set up the cluster processing and 
#push all objects (data & user defined functions) in the global environment 
#to all the clusters
clusterExport(cl, ls(envir = .GlobalEnv))

test_PSA_par <- parSapply(cl = cl, #tell the parSapply what my clusters are for parrallel processing
                      2:17, #note we now start on 2, as row 2 onwards as PSA samples! Here it is just so you can see that we are getting the same reuslts in sapply as running the function
                   Single_model_run, #Function, but only the name
                   parameter_ = Params, #Set each argument of the function to the right data
                   pat_chars_ = pat_chars, #use the same notation as if we were running the function normally
                   Treatment_ = Treatment,
                   GlobalOptions_ = GlobalOptions,
                   ResultsVariables_ = ResultsVariables
)
end.time <- Sys.time()
##Print run time
time_psa_parallel <- end.time - start.time
time_psa_parallel

test_PSA_par