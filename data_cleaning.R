library(dplyr)
library(tidyr)
library(xlsx)
##Load the data set
#datatest <- read.xlsx("C:/Users/Notis/Desktop/example_bank_movements.xlsx",1,header = FALSE,stringsAsFactors=FALSE)

data_cleaning <- function(file){
  
  ##Load the data set
  datatest <- read.xlsx(file,1,header = FALSE,stringsAsFactors=FALSE)
  
  ##cleaning data, drop empty columns and rows
  test <- datatest %>%
    select_if(function(x) !(all(is.na(x)) | all(x==""))) %>%
    na.omit()
  
  #setting the correct column names
  tempDF <- test 
  tempDF[] <- lapply(test, as.character)
  colnames(test) <- tempDF[1, ]
  test <- test[-1,]
  View(test)
  
  ##convert importe to numeric
  
  if (class(test$`IMPORTE EUR`) != "numeric"){
    test$`IMPORTE EUR` <- as.numeric(test$`IMPORTE EUR`)
  }
  
  ###creating Type variable
  test=mutate(test,Type=NA)
  
  
  
  ##put entries into type category according if they are income or expenses
  
  for (i in 1:nrow(test)){
    if (test$`IMPORTE EUR`[i]>0){
      test$Type[i] = "Income"} else{
        (test$Type[i]= "Expense") }
    
  }
  return(test)
}
