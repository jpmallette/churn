##########################################################################################################
##  Author : Jean-Philippe                                                                               #
##                                                                                                       #
##  Purpose : sample on how to prepare data and do prediction. The real preparation                      #
##  involved more work and was done using SAS and SAS Enterprise Miner.                                  #
##  This code convert some SAS code into R code. The real work involed :                                 #
##                                                                                                       #
##  - more exploration (correlation, descriptive statistics,tree for                                     #
##    meaningful variables and interaction)                                                              #
##  - consider and shrink categorical variables with tree                                                #                                                    #
##  - different methods for selecting variables (stepwise, randomForest)                                 #
##  - more data preparation (different imputation methods, log variables)                                # 
##  - many more algorithm test & tuning and data preparation base on the                                 #
##    quality of each algorithm : Neural Network, Random Forest, Gradient Boosting,                      #
##    SVM, logistic regression.                                                                          #
##  - customized metric : Sensitivity * .30 + Specificity * .30 + misclassification * .40                # 
##########################################################################################################

# Initialisation ---------------------------------------------------------------
setwd("/Users/jpmallette/Documents/Churn/")
library(caret)
library(doMC)
library(MASS)
library (party)
library(DMwR)
library(randomForest)
library(tree)
library(rpart)
library(partykit)
source('function.r')

set.seed(998)
registerDoMC(cores = 4) # parallel processing

# read and rename
churn <- read.csv("churn_train.csv")
names(churn) <- tolower(names(churn))

# Data split ---------------------------------------------------------------
trainIndex <- createDataPartition(churn$churn, p = .6,
                                  list = FALSE,
                                  times = 1)
churn_train <- churn[ trainIndex,]
churn_validation  <- churn[-trainIndex,]

# Quick Data exploration----------------------------------------------------
dim(churn_train)
str(churn_train)
summary(churn_train)
sapply(churn_train,table)
sapply(churn_train, function(x) sum(is.na(x)))

# Quick Data preparation ----------------------------------------------------
churn_train <- remove.clientid(churn_train)
churn_train <- false_missing_value(churn_train)
churn_train <- churn_as_factor(churn_train)
churn_train <- integer_to_numeric_var(churn_train)
churn_train <- new.variables(churn_train)
churn_train <- new.interaction.variables(churn_train)
churn_train <- delete_variables_too_many_missing_value(churn_train)

table(sapply(churn_train,class))

# Model Selection ----------------------------------------------------
# Only consider numeric variables for demonstration.
# categorical variables are longueur to treat

numeric_features_churn <- churn_train[,sapply(churn_train,is.numeric)]
numeric_features_churn$churn <- churn_train$churn

table(sapply(numeric_features_churn,class))

# Quick variable selection with tree. 
important_features <- variables_selection_tree(numeric_features_churn)

# Quick prediction with random Forest
numeric_features_subset <- subset(numeric_features_churn, select = important_features)
numeric_features_subset <- data.frame(apply(numeric_features_subset,
                                            2,median.imputation))

rf_model <- train(x = numeric_features_subset,
                 y = churn_train$churn,
                 preProcess = c('scale','center'),
                 method = "rf",
                 ntree=1000)

# Validation Result -------------------------------------------------------

churn_validation <- dataPreparationValidation(churn_validation) 
validation_features <- subset(churn_validation, select = important_features)
validation_predictor <- churn_validation$churn
  
test_pred <- predict(rf_model,newdata =  validation_features)

confusion <- confusionMatrix(data = test_pred,reference = churn_validation$churn)

# Final Model Prediction  -------------------------------------------------

churn_test <- read.csv("churn_test.csv")
names(churn_test) <- tolower(names(churn_test))
churn_test <- dataPreparationTest(churn_test) 

data_churn <- predict(rf_model, newdata = churn_test)
final_data <- data.frame(churn_test,churn)


