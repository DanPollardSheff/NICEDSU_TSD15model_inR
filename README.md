# NICEDSU_TSD15model_inR
This software is called NICEDSU_TSD15model_inR. It implements the discrete event simulation detailed in [NICE DSU TSD 15](https://sheffield.ac.uk/nice-dsu/tsds/patient-level-simulation) in R software. 

# Licence
The code is licenced under an **MIT licence**, see either licence file for details. The data needed to run the model is provided as is. 

# Contact details 
Authors: Dan Pollard, d.j.pollard@sheffield.ac.uk; Matt Stevenson m.d.stevenson@sheffield.ac.uk  
Maintainer: Dan Pollard, d.j.pollard@sheffield.ac.uk

# Installation
1. You need R
2. You need R studio


# Audience
Health Economists familiar with either discrete event simulation or R

# Dependencies
Base R, parallel package (which is installed with base R). The code was developed in R version 4.5.1. 

# Data 
Data to run the model is located in the Data folder and contains .csv files. 
  Parameters.csv, contains model parameter information that has 2nd order uncertainty. The 1st row are headings; the 2nd row are mean values / median values. The 3rd row and down are different random samples from probability distributions. 
  GlobalOptions.csv, this contains all of the options needed to run the model. The first column is the model option name. The 2nd column is the model option values. The 3rd column is description of the model options. The 4th column gives details of the source for the model option.
  
# R functions
User defined functions are in the R folder and contains .R files. Each user defined function is in a separate file  

  *Utility functions*  

  Standard Discounting.R, this applies the standard discounting formula in health economics of 1/(1+r)^t  
  Continuous Discounting.R, this uses integration of 1/(1+r)^t with respect to t to calculate the discounted time between any two points in time  

  
  *Loop based model functions*
  Deaths.R, this function contains the code that runs to estimate costs and QALYs upon the death event in the model for one patient  
  Hip Fracture.R, this function contains the code that runs to estimate costs and QALYs upon a hip fracture event in the model for one patient  
  Vert Fracture.R, this function contains the code that runs to estimate costs and QALYs upon a vert fracture event in the model for one patient  
  Single model run.R, this function runs the model for one set of parameters and runs the simulation looping over patients  
  Run model.R, this function controls the model and allows you to run the model deterministically or probabilistically  

  *Vectorised model functions*
  Deaths Vectorised.R, vectorised equivalent of deaths.R running the code for every patient who died in a particular event loop  
  Hip Fracture Vectorised.R, vectorised equivalent of Hip fracture.R. This code runs for every patient who had a hip fracture in a particular event loop  
  Vert Fracture Vectorised.R,  vectorised equivalent of Vert Fracture.R. This code runs for every patient who had a vert fracture in a particular event loop  
  Single Model Run Vectorised.R, this function simulates peoples times of hip fracture, vert fracture(s) and death. It loops over the maximum number of possible events (4). It determines who had a vert, hip or death in a given part of the loop and calls the correct vectorised function  
  Run Model Vectorised.R, this function is a wrapper function around the code in Single Model Run Vectorised.R. It sets the vectorised model up correctly to run deterministically or probabilistically.  


# Run the software   
1. Open the .Rproj file in RStudio
2. You open one of the .R files in the main folder (Run Analysis.R, Run Analysis Vectorised.R, Vectorised model v Loop model.R)
3. Select all code in the script editor and run
