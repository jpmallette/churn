# Purpose : sample on how to prepare data and do prediction. The real preparation
# involved more work by was done using SAS and SAS Enterprise Miner.
# This code convert some SAS code into R code. The real work involed : 
#  - more exploration (correlation)
#  - different methods for selecting variables (stepwise, randomForest)
#  - more data preparation configuration test (standardize & center, imputation methods)
#  - many more algorithm test & tuning and data preparation base on the
# quality of each algorithm : Neural Network, Random Forest, Gradient Boosting, 
# SVM, logistic regression.
#  - customized metric : Sensitivity * .30 + Specificity * .30 + misclassification * .40

# Initialisation ---------------------------------------------------------------
setwd("/Users/jpmallette/Desktop/Project/Churn/")
library(caret)
source('function.r')

set.seed(998)
registerDoMC(cores = 4) # parallel processing

# read and rename training data
churn_train <- read.csv("churn_train.csv")
churn_test <- read.csv("churn_test.csv")
names(churn_train) <- tolower(names(churn_train))
names(churn_test) <- tolower(names(churn_test))


# Quick Data exploration----------------------------------------------------

attach(churn_train)
dim(churn_train)
str(churn_train)
summary(churn_train)
sapply(churn_train,table)
sapply(churn_train, function(x) sum(is.na(x)))

# Data Preparation ----------------------------------------------------

# remove clientid
churn_train <- churn_train[ , -which(names(churn_train %in% 'clientid'))]

# Treating false missing value & aberante value 
churn_train$eqdays <- ifelse(churn_train$eqdays < 0, 0,churn_train$eqdays)

# Transform required variables to factor. See in the documentation dictionary
churn_train$churn <- as.factor(churn_train$churn)

# shorten factor variables

# creation of log variables

# creation of new variables
new_variables <- new.variables(churn_train)
churn_train <- data.frame(churn_train,new_variables)

# creation of interaction variables
new_interaction_variables <-new.interaction.variables(churn_train)
churn_train <- data.frame(churn_train,new_interaction_variables)

# Model Selection ----------------------------------------------------

# stepwise if needed 

# Random Forest

preProcess(churn_train, 
            method = c("center", "scale","medianImpute"),
            pcaComp = NULL,
            metric = 'Accuracy',
            na.remove = TRUE) 
           
rfGrid <-  expand.grid(.mtry = c(2, 4, 8, 16))

rf_model<-train(churn~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=10),
                prox=TRUE,allowParallel=TRUE,
                na.action = na.omit(),
                n.trees = (1:30)*50,
                tuneGrid = rfGrid)

cbind(varImp(fm),importance(rf_model$finalModel))
summary(fm)

# Model Prediction -------------------------------------------------

predict(rf_model, newdata = churn_test)

  


#### back-up code

# gradient boosting

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit <- train(churn ~ ., data = training,
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE,
                allowParallel=TRUE,
                tuneGrid = gbmGrid
)

#
predict(gbmFit3, newdata = head(testing))


# performance and comparing different model performance 
confusionMatrix(data = plsClasses, testing$Class)

resamps <- resamples(list(RF = rf_model,
                          GBM = gbmFit1))
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

# alternatives to compare performance 

difValues <- diff(resamps)
bwplot(difValues, layout = c(3, 1))

