datac <- read.csv('classification-output-data.csv', stringsAsFactors = F)
head(datac)
library(DataExplorer)
create_report(datac)

# how to calculate Accuracy 

library(caret)
set.seed(1)
# transform data into training and test set 
sample <- sample(c(TRUE,FALSE), nrow(datac), replace = T, prob = c(0.7,0.3))
# build training set
reduced_training <- datac[sample,]
#build testing set 
reduced_test <- datac[!sample, ]


library(MASS)
# create a model 
conf_matrix <- table(datac$class, datac$scored.class)
conf_matrix
conf_matrix[1,1]

get_Accuracy <- function(confusion_matrix){
  TN <- confusion_matrix[1,1] # True Negative
  FP <- confusion_matrix[1,2] # False Positive
  FN <- confusion_matrix[2,1] # False Negative
  TP <- confusion_matrix[2,2] # True Positive
  
  # calculate accuracy 
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  
  # Classification Error Rate 
  class_err_rate <- (FP + FN) / (TP + FP + TN + FN)
  
  # Use cat() to print multiple values
  cat('The accuracy is: ', accuracy, '\n')
  cat('The Classification Error Rate is: ', class_err_rate, '\n')
  cat('The sum of both accuracy and classification error is ', sum(accuracy, class_err_rate))
}

get_Accuracy(conf_matrix)
precision(data = as.factor(datac$scored.class), 
          reference = as.factor(datac$class),
          positive = '0')

TN <- conf_matrix[1,1] # True Negative
FP <- conf_matrix[1,2] # False Positive
FN <- conf_matrix[2,1] # False Negative
TP <- conf_matrix[2,2] # True Positive

precision <- (TP)/ (TP + FP)
precision


## Let do it on caret 
data <- datac
data$class <- as.factor(data$class)
data$scored.class <- as.factor(data$scored.class)


set.seed(1)
# testing precision 


# use function in caret package to do it 
confusion <- confusionMatrix(data$scored.class, data$class, positive= '1')
confusion
conf_matrix
get_Accuracy(conf_matrix)
confusion$byClass


###############################################################
# Let calculate the the precision 
precision <- (TP)/ (TP + FP)
precision

caret_precision <- confusion$byClass['Precision']
caret_precision

get_precision <- function(confusion_matrix){
  TN <- confusion_matrix[1,1] # True Negative
  FP <- confusion_matrix[1,2] # False Positive
  FN <- confusion_matrix[2,1] # False Negative
  TP <- confusion_matrix[2,2] # True Positive
  
  # calculate accuracy 
  precision <-  (TP)/ (TP + FP)
  
  
  
  # Use cat() to print multiple values
  cat('The precision is: ', precision, '\n')
  
}


################
# sensitivity 
get_sensitivity <- function(confusion_matrix){
  TN <- confusion_matrix[1,1] # True Negative
  FP <- confusion_matrix[1,2] # False Positive
  FN <- confusion_matrix[2,1] # False Negative
  TP <- confusion_matrix[2,2] # True Positive
  
  # calculate accuracy 
  sensitivity <-  (TP)/ (TP + FN)
  
  
  
  # Use cat() to print multiple values
  cat('The Sensitivity is: ', sensitivity, '\n')
  
}
get_sensitivity(conf_matrix)
# other way to calculated 
caret_sensitivity <- confusion$byClass['Sensitivity']
caret_sensitivity



# sensitivity 
get_sensitivity <- function(confusion_matrix){
  TN <- confusion_matrix[1,1] # True Negative
  FP <- confusion_matrix[1,2] # False Positive
  FN <- confusion_matrix[2,1] # False Negative
  TP <- confusion_matrix[2,2] # True Positive
  
  # calculate accuracy 
  sensitivity <-  (TP)/ (TP + FN)
  
  
  
  # Use cat() to print multiple values
  cat('The Sensitivity is: ', sensitivity, '\n')
  
}
get_sensitivity(conf_matrix)
# other way to calculated 
caret_sensitivity <- confusion$byClass['Sensitivity']
caret_sensitivity