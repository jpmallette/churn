# function for prediction 

new.variables <- function(churn_train) {
  
  suppressMessages(attach(churn_train))
  
  changemou3_6       <- avg3mou - avg6mou 
  changemou3_life    <- avg3mou - avgmou  
  changeqty3_6       <- avg3qty - avg6qty   
  changeqty3_life    <- avg3qty  - avgqty 
  changerev3_6       <- avg3rev - avg6rev  
  changerev3_life    <- avg3rev - avgrev 
  
  changemou3_div_6       <- avg3mou / avg6mou 
  changemou3_div_life    <- avg3mou / avgmou  
  changeqty3_div_6       <- avg3qty / avg6qty   
  changeqty3_div_life    <- avg3qty  / avgqty 
  changerev3_div_6       <- avg3rev / avg6rev  
  changerev3_div_life    <- avg3rev / avgrev 
  
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
                   changerev3_6,changerev3_life,changemou3_div_6,changemou3_div_life,
                   changeqty3_div_6,changeqty3_div_life,changerev3_div_6,changeqty3_div_life,
                   changerev3_div_6,changerev3_div_life,change_hnd_price,
                   int_model_hnd_price,ratio_att_complete,ratio_drop_complete,rat1,rat3,
                   pct_paid_vs_reccu_charges_3,pct_paid_vs_reccu_charges_6,gen_min_per_call
                   )
  return(df)
  }

# interactions variables identified with the decision tree 
# " we can find meaningful interaction variables by running decision tree
# " by looking at two or three features at the time, we can find 
#  interesting interaction features 

# t <- train(churn ~ eqpdays + change_hnd_price,churn_train,method = "rpart",cp=0.002,maxdepth=8)
# plot(t$finalmodel)

#  add log variables 

# Changing integer variables to numeric variables
new.log.variables <- function(churn_train) {

# convert all integer to numeric
integer_var <- churn_train[,sapply(churn_train,is.integer)]
integer_var_to_num <- sapply(integer_var,as.numeric)
churn_train[,sapply(churn_train,is.integer)] <- integer_var_to_num

# subset only numeric column
numeric_var <- churn_train[,sapply(churn_train,is.numeric)]

# replace NA with median imputation. Need to recode NA with Null
preProcess(numeric_var, method = c("medianImpute"))
          
# find all the minimum of all column 
columum_min <- sapply(numeric_var,min)

# add the minimum to all column 

# transform all variables to log 

# return df
return(df)
}





new.interaction.variables <- function(churn_train) {
  
  suppressMessages(attach(churn_train))
  
  eqpdays_change_hnd_price    <- eqpdays * change_hnd_price  
  eqpdays_log_months          <- eqpdays * log_months 
  eqpdays_changemou3_life     <- eqpdays * changemou3_life
  change_hnd_price_log_months <- change_hnd_price * log_months  
  log_months_charge_vs_basic  <- log_months * charge_vs_basic  
  changemou3_life_log_months  <- changemou3_life * log_months  
  log_months_hnd_price        <- log_months * hnd_price  
  log_mou_mean_change_mou     <- log_mou_mean * change_mou      
  changemou3_life_change_mou  <- changemou3_life * change_mou  
  ch_vs_b_log_mou_o_r         <- charge_vs_basic * log_mou_opkv_range  
  months_handsetprice         <- months * hnd_price   
  months_pre_hnd_price        <- months * pre_hnd_price 
  eq_hnd_price                <- eqpdays * hnd_price 
  change_h_p_ch_vs_ba         <- change_hnd_price *  charge_vs_basic  
  
  data.frame(eqpdays_change_hnd_price,eqpdays_log_months,eqpdays_changemou3_life,
             change_hnd_price_log_months,change_hnd_price_log_months,log_months_charge_vs_basic,
             changemou3_life_log_months,log_months_hnd_price,log_mou_mean_change_mou,
             changemou3_life_change_mou,ch_vs_b_log_mou_o_r,months_handsetprice,months_pre_hnd_price,
             eq_hnd_price,change_h_p_ch_vs_ba)
  
  return(df)
}




# shrink categorical variables 

# recode(myColors, "'red'='rot'; 'blue'='blau'; 'purple'='violett'")
# recode(myColors, "c('red', 'blue')='basic'; else='complex'")

