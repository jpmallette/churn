# function for prediction 

dataPreparationValidation <- function(churn_validation) {
  
  churn_validation <- remove.clientid(churn_validation)
  churn_validation <- false.missing.value(churn_validation)
  churn_validation <- churn.as.factor(churn_validation)
  churn_validation <- integer.to.numeric.var(churn_validation)
  
  churn_validation <- new.variables(churn_validation)
  churn_validation <- new.interaction.variables(churn_validation)
  churn_validation <- delete.variables.too.many.missing.value(churn_validation)
  
  numeric_features   <- churn_validation[,sapply(churn_validation,is.numeric)]
  numeric_features$churn   <- churn_validation$churn
  
  churn_validation   <- data.frame(apply(numeric_features,
                                              2,median.imputation))
  return(churn_validation)
}

dataPreparationTest <- function(churn_test) {
  
  churn_test <- false.missing.value(churn_test)
  churn_test <- integer.to.numeric.var(churn_test)
  churn_test <- new.variables(churn_test)
  churn_test <- new.interaction.variables(churn_test)
  churn_test <- delete.variables.too.many.missing.value(churn_test)
  
  numeric_features   <- churn_test[,sapply(churn_test,is.numeric)]
  numeric_features$churn   <- churn_test$churn
  churn_test   <- data.frame(apply(numeric_features,
                                         2,median.imputation))
  return(churn_test)
}


variables.selection.tree <- function(numeric_features_churn)  {
  
rpart_tree <- rpart(churn ~ ., data = numeric_features_churn,
                    cp = c(0.001),maxdepth = 5) 

output <- head(varImp(rpart_tree),n = 30)
output[order(-output$Overall),,drop=FALSE]
important_features <- rownames(subset(output,Overall > 0))
return(important_features)

}

remove.clientid <- function(churn_train) {
  churn_train <- churn_train[ , -which(names(churn_train) == 'clientid')]
  return(churn_train)
}

false.missing.value <- function(churn_train) {
  churn_train$eqpdays <- ifelse(churn_train$eqpdays < 0, 0,churn_train$eqpdays)
  return(churn_train)
} 
churn.as.factor <- function(churn_train) {
  churn_train$churn <- as.factor(churn_train$churn)
  return(churn_train)
}

integer.to.numeric.var <- function(churn_train) {
  integer_var <- churn_train[,sapply(churn_train,is.integer)]
  integer_var_to_num <- sapply(integer_var,as.numeric)
  churn_train[,sapply(churn_train,is.integer)] <- integer_var_to_num
  return(churn_train)
}

delete.variables.too.many.missing.value <- function(churn_train) {
   
  n_row <- nrow(churn_train)
  missing <- sapply(churn_train, function(x) sum(is.na(x)))
  percent_missing <- missing/n_row
  keep_column <- percent_missing[which(percent_missing <= 0.50)]
  keep_column_name <- names(keep_column)
  churn_train <- churn_train[,c(keep_column_name)] 
  return(churn_train)
}

new.variables <- function(churn_train) {
  
  suppressMessages(attach(churn_train))
  
  # minutes used
  changemou3_6       <- avg3mou - avg6mou 
  changemou3_life    <- avg3mou - avgmou  
  changeqty3_6       <- avg3qty - avg6qty   
  changeqty3_life    <- avg3qty  - avgqty 
  changerev3_6       <- avg3rev - avg6rev  
  
  changemou3_div_6       <- avg3mou / avg6mou 
  changemou3_div_life    <- avg3mou / avgmou  
  changeqty3_div_6       <- avg3qty / avg6qty   
  changeqty3_div_life    <- avg3qty  / avgqty 
  changerev3_div_6       <- avg3rev / avg6rev  
  
  # change in handset price 
  change_hnd_price <- hnd_price - pre_hnd_price  
  int_model_hnd_price <- models * hnd_price 
  
  # quality of services
  ratio_att_complete  <- attempt_mean / complete_mean   
  ratio_drop_complete <- drop_blk_mean / complete_mean 
  
  rat1 <- comp_dat_mean / (unan_dat_mean+plcd_dat_mean) 
  rat3 <- comp_vce_mean / (unan_vce_mean + plcd_vce_mean)     
  
  # percent paid 
  pct_paid_vs_reccu_charges_3 <- avg3rev / totmrc_mean
  pct_paid_vs_reccu_charges_6 <- avg6rev / totmrc_mean
  gen_min_per_call <- totmou/totcalls
  
  df <- data.frame(changemou3_6,changemou3_life,changeqty3_6,changeqty3_life,
                   changemou3_div_6,changemou3_div_life,
                   changeqty3_div_6,changeqty3_div_life,changerev3_div_6,
                   change_hnd_price,
                   int_model_hnd_price,ratio_att_complete,ratio_drop_complete,rat1,rat3,
                   pct_paid_vs_reccu_charges_3,pct_paid_vs_reccu_charges_6,gen_min_per_call
                   )
  churn_train <- data.frame(churn_train,df)
  return(churn_train)
  }

# found with the decision Tree algorithm 
new.interaction.variables <- function(churn_train) {
  
  suppressMessages(attach(churn_train))
    
  eqpdays_change_hnd_price    <- eqpdays * change_hnd_price  
  eqpdays_months              <- eqpdays * months 
  eqpdays_changemou3_life     <- eqpdays * changemou3_life
  change_hnd_price_months     <- change_hnd_price * months  
  changemou3_life_months      <- changemou3_life * months  
  months_hnd_price            <- months * hnd_price  
  mou_mean_change_mou         <- mou_mean * change_mou      
  changemou3_life_change_mou  <- changemou3_life * change_mou  
  months_pre_hnd_price        <- months * pre_hnd_price 
  eq_hnd_price                <- eqpdays * hnd_price 
  
  
  df <- data.frame(eqpdays_change_hnd_price,eqpdays_months,eqpdays_changemou3_life,
             change_hnd_price_months,
             changemou3_life_months,months_hnd_price,mou_mean_change_mou,
             changemou3_life_change_mou,months_pre_hnd_price,
             eq_hnd_price)
  
  churn_train <- data.frame(churn_train,df)
  return(churn_train)
}

formula.creation <- function(features_names,predictor_name) {
  
  for (features_position in 1:length(features_names)) {
    
    if (features_position == 1) {
      formula <- paste(predictor_name,'~',features_names[features_position])
    } else {
      append_formula <- paste(' +',features_names[features_position])
      formula <- paste0(formula,append_formula)
    }
  }
  return(formula)
}

median.imputation <- function(features) {
  features<-as.numeric(as.character(features))
  features[is.na(features)] <- median(features, na.rm=TRUE)
  features[is.infinite(features)] <- median(features, na.rm=TRUE)
  features[is.nan(features)] <- median(features, na.rm=TRUE)
  return(features)
}
