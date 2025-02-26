The code is licenced under an MIT licence, see the licence file for details. The data needed to run the model is provided as is. 

Data is located in the Data folder and contains .csv files. 
  Parameters.csv, contains model parameter information that has 2nd order uncertainty. The 1st row are headings, the 2nd row are mean values / median values. The 3rd row and down are different random samples from probability distributions. 
  GlobalOptions.csv, this contains all of the options needed to run the model. The first column is the model option name. The 2nd column is the model option values. The 3rd column is description of the model options. The 4th column gives details of the         source for the model option
  
User defined functions are in the R folder and contains .R files. Each user defined function is in a seperate file
  Continuous Discounting.R, this function calculates the discounted time between any two sampled times, given an input annualised discount rate
  Deaths.R, this function contains the code that runs to estimate costs and QALYs upon the death event in the model
  Hip Fracture.R, this function contains the code that runs to estimate costs and QALYs upon a hip fracture event in the model
  Standard Discounting.R, this function calculates 1/(1+r)^t for inputed values of r and t
  Vert Fracture.R, this function contains the code that runs to estimate costs and QALYs upon a vert fracture event in the model
  
RunAllAnalyses.R, contains the code that applies the Discrete Event Simulation in HAR672, which is based on the NICEDSU TSD15 model.   
