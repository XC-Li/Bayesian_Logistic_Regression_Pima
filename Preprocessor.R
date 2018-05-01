#DATS6450 Bayesian method for data science final project
#Title: Diabetes Prediction using Bayesian Logistic Regression
#Author: Li Du, Xiaochi Li, Yupeng Yang (Group 11)

#Data preprocessor

Preprocessor = function (myData, standardize=TRUE, balance=TRUE) {
  #check for null values in the raw data
  print(paste0("Any null values in the data? ", all(is.na(myData))))
  
  #In order to make sure all the records are complete, biological predictors 
  #except "Pregnancies" with zero values will be set to NA
  data_bioPred = colnames(myData)[!colnames(myData) %in% c("Pregnancies", "Outcome")]
  bool = myData[data_bioPred] == 0
  myData[data_bioPred][bool] = NA
  
  #dataset without missing values
  data_complete = myData[complete.cases(myData),]
  
  if (!any(standardize, balance)) {
    write.csv(data_complete, "diabetes_noMissing.csv")
    print("Raw data have only been cleaned without standardization and balance...")
    print(paste0(nrow(data_complete), " in the output file.", nrow(myData)-nrow(data_complete), " records were removed..."))
  }
  
  if (standardize == TRUE) {
    #standardize the data
    data_std = as.data.frame(scale(data_complete[,-9],center = TRUE, scale = TRUE))
    data_std$Outcome = data_complete$Outcome
    
    if (balance == FALSE) {
      write.csv (data_std, "diabetes_standarized.csv")
      print("Raw data have been cleaned and standardized...")
      print(paste0(nrow(data_std), " in the output file.", nrow(myData)-nrow(data_std), " records were removed..."))
    }
    
    #balance == TRUE
    else {
      #Because there are 130 1's and 232 0's (almost twice as much as 1's), the imbalanced classes may have impact on the final results
      #As an alternative case, 130 samples were randomly pulled from the 232 0's to balance the two classes
      
      #separate the records with Outcome as 0 and 1
      zeros = data_std[data_std$Outcome == 0,]
      ones = data_std[data_std$Outcome == 1,]
      
      #randomly pull 130 samples from the records with outcomes as 0
      set.seed(0)
      rowidx = sample(nrow(zeros), nrow(ones), replace = FALSE)
      zeros_balanced = zeros[rowidx,]
      #the new dataframe with equal number of 1's and 0's as the outcomes
      data_std_balanced = rbind(ones, zeros_balanced)
      
      write.csv(data_std_balanced, "diabetes_std_bal.csv")
      print("Raw data have been cleaned, standardized and balanced...")
      print(paste0(nrow(data_std_balanced), " in the output file.", nrow(myData)-nrow(data_std_balanced), " records were removed..."))
    }
  }
  
  #only balance the 1's and 0's without standardizing the data
  if (standardize == FALSE & balance == TRUE) {
    zeros = data_complete[data_complete$Outcome == 0,]
    ones = data_complete[data_complete$Outcome == 1,]
    
    #randomly pull 130 samples from the records with outcomes as 0
    set.seed(0)
    rowidx = sample(nrow(zeros), nrow(ones), replace = FALSE)
    zeros_balanced = zeros[rowidx,]
    #the new dataframe with equal number of 1's and 0's as the outcomes
    data_balanced = rbind(ones, zeros_balanced)
    
    write.csv(data_balanced, "diabetes_balanced.csv")
    print("Raw data have been cleaned and balanced...")
    print(paste0(nrow(data_balanced), " in the output file.", nrow(myData)-nrow(data_balanced), " records were removed..."))
  }
}