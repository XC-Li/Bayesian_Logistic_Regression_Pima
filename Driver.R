# Comparison of Traditional Logistic Regression and Bayesian Logistic Regression
# Abstract Part
# Author: Li Du, Yupeng Yang, Xiaochi Li
# Bayesian Method for Data Science (DATS 6450-11, Spring 2018)
# Data Science @ GWU  

#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# #.............................................................................
setwd("D:/OneDrive/DataScience/Bayesian/Final Project/Project_Code")
source("Preprocessor.R")
myData = read.csv( file="diabetes.csv" ) #data 5
Preprocessor(myData,standardize = TRUE,balance = TRUE) #data 1
Preprocessor(myData,standardize = TRUE,balance = FALSE) #data 2
Preprocessor(myData,standardize = FALSE,balance = TRUE) #data 3
Preprocessor(myData,standardize = FALSE,balance = FALSE) #data 4
# #.............................................................................
which_data_to_use = 1 # only change here to change the data source


if(which_data_to_use == 1 | which_data_to_use==2){ 
  # auto switch for whether use standardized parameter in bayesian logistic model
  standardized_parameter = TRUE
}else{
  standardized_parameter = FALSE
}

all_data = list("diabetes_std_bal","diabetes_standarized","diabetes_balanced","diabetes_noMissing","diabetes")
fileNameRoot = all_data[[which_data_to_use]]
use_data = read.csv(paste0(all_data[which_data_to_use],".csv"))
if (which_data_to_use != 5){
  use_data = use_data[,-1]
}
# #.............................................................................
# # We changed the file name and variable names here 
yName = "Outcome"
xName = c("Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI"
          ,"DiabetesPedigreeFunction","Age")

numSavedSteps=15000 ; thinSteps=2
graphFileType = "pdf" 
source("MCMC_logistic.R")
# #.............................................................................
# # Train Test split
split_data = train_test_split(use_data,0.3)
train_set = split_data[["train"]]
test_set = split_data[["test"]]
rm(split_data,use_data,myData,all_data)
# #.............................................................................
# use the traditional logistic regression to build model
# evaluate performance by ROC and AUC on test_set
GLM_coef = traditional_logistic(train_set,test_set)

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
startTime = proc.time()
mcmcCoda = genMCMC( data=train_set , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )
stopTime = proc.time()
duration = stopTime - startTime
show(duration)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
graphics.off()
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=train_set , xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType, GLM_coef=GLM_coef )
graphics.off()
#------------------------------------------------------------------------------- 
# read from "pima-SummaryInfo.csv" produced by MCMC, get the parameters and build the logistic function
# evaluate performance by ROC and AUC on test_set
parameter_file = read.csv(paste0(fileNameRoot,"SummaryInfo.csv"),row.names = 1)
# which parameter are used to build the logistic function
if (standardized_parameter == FALSE){
  parameter = parameter_file[1:9,"Mode"] #use standardized parameter in bayesian logistic model 
}else{
    parameter = parameter_file[10:18,"Mode"]
    }
bayesian_logistic(train_set,test_set,parameter)
# #.............................................................................
