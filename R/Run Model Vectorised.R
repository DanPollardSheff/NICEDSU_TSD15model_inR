
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

run_model_vec <- function(parameter_,
                      pat_chars_,
                      Treatment_,
                      GlobalOptions_,
                      ResultsVariables_){
  if(GlobalOptions_["Run_PSA","Value"]==FALSE){
    #code to run deterministically
    Results <- Single_model_run_vectorised(1,
                                parameter_,
                                pat_chars_,
                                Treatment_,
                                GlobalOptions_,
                                ResultsVariables_)
  }else{
    #Vector telling R which rows we of the parameter matrix we want to run in the PSA
    #starts at 2, as row 1 is deterministic
    #ends at the number of PSA iterations we want + 1, as we started at row 2
    PSAvector <- 2:(as.numeric(GlobalOptions_["Number_of_PSARuns","Value"])+1)
    
    #set up parrallel processing
    #these are the additional global variables that need to be set for parrallel
    #processing functions
    numCores <- detectCores()-1 #detect the number of cores on my current PC
    #set the number of cores to use to be the minimum of the number in the GlobalOptions.csv 
    #file or the number of detected cores on this PC. This is because the PC can crash if it 
    #runs out of memory from running too many models at once.
    numCores <- min (numCores, as.numeric(GlobalOptions_["Number_of_cores", "Value"])) 
    
    ##Next 3 lines of code set up clustering (technical thing needed for parallel processing)
    cl <- makeCluster(numCores)
    #Set a random number seed for parallel processing
    clusterEvalQ(cl, set.seed(1))
    
    #Set up the cluster processing and 
    #push all objects (data & user defined functions) in the global environment 
    #to all the clusters
    clusterExport(cl, ls(envir = .GlobalEnv))
    
    Results <- parSapply(cl = cl, #tell the parSapply what my clusters are for parrallel processing
                              PSAvector, #note we now start on 2, as row 2 onwards as PSA samples! Here it is just so you can see that we are getting the same reuslts in sapply as running the function
                              Single_model_run_vectorised, #Function, but only the name
                              parameter_ = Params, #Set each argument of the function to the right data
                              pat_chars_ = pat_chars, #use the same notation as if we were running the function normally
                              Treatment_ = Treatment,
                              GlobalOptions_ = GlobalOptions,
                              ResultsVariables_ = ResultsVariables)
    
    stopCluster(cl=cl)
  }
  return(Results)
}