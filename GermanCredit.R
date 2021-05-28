setwd('C:\\RLang\\CreditRiskModelling')
# DATA SELECTION
creditdata <- read.csv('german_credit.csv')
str(creditdata)
# PREPROCESSING
S <- c(1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21)
for(i in S) creditdata[, i] <- as.factor(creditdata[, i])
creditdata_new <- creditdata[,S]
str(creditdata_new)
# Sample Indexes
 indexes = sample(1:nrow(creditdata), size=0.3*nrow(creditdata))
# Split data
 credit_test = creditdata_new[indexes,]
 credit_train = creditdata_new[-indexes,]
 dim(credit_test)
 dim(credit_train)
 set.seed(1)
 #FEATURES SELECTION
 LogisticModel <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family = binomial, data = credit_train)
 LogisticModel
 #ENTER INTEGERS VALUES, CONVERTED TO FACTORS TO BUILD PREDICTIVE MODEL
 balance <- readline(prompt="What is your account balance (1-4)?")
 paymentStatus <- readline(prompt="What is your payment status (0-4)?")
 purpose <- readline(prompt="Purpose of loan (0-10)?")
 employmentLength <- readline(prompt="What is the length of your employment (1-5)?")
 maritalStatus <- readline(prompt="Marital status (1-5)?")
# newapplicant <- data.frame(Account.Balance=as.factor(balance), Payment.Status.of.Previous.Credit=as.factor(paymentStatus), Purpose=as.factor(purpose), Length.of.current.employment=as.factor(employmentLength), Sex...Marital.Status=as.factor(maritalStatus))
 newapplicant <- data.frame(Account.Balance=as.factor(4), Payment.Status.of.Previous.Credit=as.factor(2), Purpose=as.factor(1), Length.of.current.employment=as.factor(4), Sex...Marital.Status=as.factor(2))
 #EVALUATE PREDICTION
 result <- predict(LogisticModel, type = 'response', newdata = newapplicant)
 result
 if(result>0.6) {credibility = 1} else {credibility = 0}
  credibility
predicted_values <- predict(LogisticModel, type = 'response', newdata = credit_test)
plot(predicted_values)
pred_value_labels = rep(as.factor("0"),length(credit_test))
pred_value_labels = rep("0",length(credit_test[,1]))
pred_value_labels[predicted_values>.6] = "1"
pred_value_labels <- as.factor(pred_value_labels)
pred_value_labels
#Receiver Operating Characteristic (ROC) curve
install.packages("ROCR")
library(ROCR)
pred <- prediction(predicted_values, credit_test$Creditability)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)  
#Area under curve
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values
#Accuracy and cutoff
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
install.packages("caret")
library(caret)
install.packages('e1071', dependencies=TRUE)
#Confusion matrix
confusionMatrix(credit_test$Creditability,pred_value_labels)
print(c(accuracy= acc, cutoff = cutoff))
#Lending Club loan data
loandata <- read.csv('loan_data_2017.csv')
install.packages("magrittr") # package installations are only needed the first time you use it
library(magrittr) # needs to be run every time you start R and want to use %>%
install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)    # alternatively, this also loads %>%
install.packages("ggplot2") # Install it again
library(ggplot2) # Load the librarie (you have to do this one on each new session)
#Loan grades
g1 = loandata %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(default_count = n())
g1
g2 = loandata %>% group_by(grade) %>% summarise(count = n())
g3 <- g2 %>% left_join(g1) %>% mutate(default_rate = 100*default_count/count) %>% select(grade,count,default_count,default_rate)
#Joining, by = "grade"
g3
ggplot(g3, aes(x=grade, y=default_rate, fill=grade)) + geom_bar(stat="identity")
loandata$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = loandata$int_rate)))
x1 = loandata %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(int_rate=mean(int_rate))
ggplot(x1, aes(x=grade, y=int_rate, fill=grade)) + geom_bar(stat="identity",position="dodge")






 install.packages("corrplot")
 library(corrplot)
 install.packages("doParallel")
 library(doParallel)
 install.packages("pROC")
 library(pROC)
 install.packages("e1071")
 library(e1071)
 install.packages("kernlab")
library(kernlab)
 install.packages("xgboost")
library(xgboost)
 indexes = sample(1:nrow(loandata), size=0.3*nrow(loandata))
# Split data
data_test = loandata[indexes,]
dim(data_test)
data_train = loandata[-indexes,]
dim(data_train)
rm(loandata)
discard_column = c("collection_recovery_fee","emp_title",
                   "funded_amnt_inv","id",
                   "installment","last_credit_pull_d",
                   "last_fico_range_high","last_fico_range_low",
                   "last_pymnt_amnt","last_pymnt_d",
                   "loan_amnt","member_id",
                   "next_pymnt_d","num_tl_120dpd_2m",
                   "num_tl_30dpd","out_prncp",
                   "out_prncp_inv","recoveries",
                   "total_pymnt","total_pymnt_inv",
                   "total_rec_int","total_rec_late_fee",
                   "total_rec_prncp","url",
                   "zip_code"
)
install.packages("pROC")
library(pROC)
data_train = (data_train[,!(names(data_train) %in% discard_column)])
dim(data_train)
data_train$grade = NULL
data_train <- data_train[, -which(colMeans(is.na(data_train)) > 0.5)]

tmp = sort(sapply(data_train, function(x) sum(length(which(is.na(x)))))/nrow(data_train),decreasing = TRUE)
install.packages('caret')
library(caret)
median_impute_model = preProcess(data_train[names(tmp)],method="medianImpute")
data_train = predict(median_impute_model,data_train)
sort(sapply(data_train, function(x) sum(length(which(is.na(x)))))/nrow(data_train),decreasing = TRUE)
str(data_train)
discard_column = c("hardship_flag","hardship_type",
                   "hardship_reason","hardship_status",
                   "hardship_start_date","hardship_end_date",
                   "payment_plan_start_date","hardship_loan_status",
                   "disbursement_method","debt_settlement_flag",
                   "debt_settlement_flag_date","settlement_status",
                   "settlement_date"
)
data_train = (data_train[,!(names(data_train) %in% discard_column)])
data_train$revol_util = (as.numeric(gsub(pattern = "%",replacement = "",x = data_train$int_rate))) 
install.packages("lubridate")
library(lubridate)
install.packages('dplyr')
library(dplyr)
install.packages('stringr')
library('stringr')

data_train$earliest_cr_line = parse_date_time(str_c("01",data_train$issue_d),"dmy" ) - parse_date_time(str_c("01",data_train$earliest_cr_line),"dmy" )
data_train$earliest_cr_line = as.numeric(data_train$earliest_cr_line,units = "days")
data_train$issue_m = sapply( data_train$issue_d ,function(x){str_split(x,"-")[[1]][1]})
install.packages('rms')
library(rms)
install.packages('dplyr')
library(dplyr)

tmp = data_train %>% filter(loan_status=="Default") %>% group_by(issue_m) %>% summarise(default_count = n())
tmp2 = data_train %>% group_by(issue_m) %>% summarise(count = n())
tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
data_train$issue_m = NULL
data_train$issue_d = NULL
rm(tmp,tmp2)
getNumericColumns<-function(t){
  tn = sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}
# Returns the character columns from a dataset
getCharColumns<-function(t){
  tn = sapply(t,function(x){is.character(x)})
  return(names(tn)[which(tn)])
}
# Returns the factor columns in a dataset 
getFactorColumns<-function(t){
  tn = sapply(t,function(x){is.factor(x)})
  return(names(tn)[which(tn)])
}
# Returns index of columns along with the column names
getIndexsOfColumns <- function(t,column_names){
  return(match(column_names,colnames(t)))
}
tmp = apply(data_train[getCharColumns(data_train)],2,function(x){length(unique(x))})
tmp = tmp[tmp==1]

tmp2 = apply(data_train[getNumericColumns(data_train)],2,function(x){(sd(x))})
tmp2 = tmp2[tmp2==0]

discard_column = c(names(tmp),names(tmp2))
discard_column
data_train = (data_train[,!(names(data_train) %in% discard_column)])
table(data_train$purpose)
data_train$title = NULL
str(data_train$desc)
data_train$desc = NULL
install.packages('stringr')
library('stringr')
tmp = data_train %>% filter(loan_status=="Default") %>% group_by(addr_state) %>% summarise(default_count = n())
tmp2 = data_train %>% group_by(addr_state) %>% summarise(count = n())
tmp3 = tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
#order by highest default rate

high_default = (tmp3 %>% filter(count > 1000) %>% arrange(desc(default_rate)))[1:10,"addr_state"]$addr_state

high_default
# order by lowest default rate

low_default = (tmp3 %>% filter(count > 1000) %>% arrange((default_rate)))
low_default
data_train$is_ny = ifelse(data_train$addr_state=="NY",1,0)
data_train$is_pa = ifelse(data_train$addr_state=="PA",1,0)
data_train$is_nj = ifelse(data_train$addr_state=="NJ",1,0)
data_train$is_oh = ifelse(data_train$addr_state=="OH",1,0)
data_train$is_fl = ifelse(data_train$addr_state=="FL",1,0)

data_train$is_co = ifelse(data_train$addr_state=="CO",1,0)
data_train$is_ga = ifelse(data_train$addr_state=="GA",1,0)
data_train$is_va = ifelse(data_train$addr_state=="VA",1,0)
data_train$is_az = ifelse(data_train$addr_state=="AZ",1,0)
data_train$is_ca = ifelse(data_train$addr_state=="CA",1,0)
data_train$addr_state = NULL 
rm(tmp, tmp2, tmp3, high_default, low_default)
corrplot(cor(data_train[getNumericColumns(data_train)], use="na.or.complete"))
high_corr <- findCorrelation(cor(data_train[getNumericColumns(data_train)]), cutoff = .75)
high_corr = getNumericColumns(data_train)[high_corr]
high_corr
data_train = (data_train[,!(names(data_train) %in% high_corr)])
str(data_train[getNumericColumns(data_train)])
data_train$annual_inc = data_train$annual_inc/data_train$funded_amnt
data_train$revol_bal = data_train$revol_bal/data_train$funded_amnt
data_train$avg_cur_bal = data_train$avg_cur_bal/data_train$funded_amnt
data_train$bc_open_to_buy = data_train$bc_open_to_buy/data_train$funded_amnt
str(data_train[getCharColumns(data_train)])
data_train$verification_status_joint = NULL
table(data_train$home_ownership)
data_train %>% filter(pymnt_plan=="y") %>% select(pymnt_plan, loan_status)
data_train$pymnt_plan = NULL
loan_status = data_train$loan_status
table(loan_status)
loan_status
 dummy_model = dummyVars(loan_status ~ .,data_train,fullRank = TRUE)
 data_train = as.data.frame(predict(dummy_model,data_train))
 data_train$loan_status = loan_status
 rm(loan_status)
#set loan with status 'Fully Paid' as a positive sample
data_train$loan_status = ifelse(data_train$loan_status == "Fully Paid","Fully.Paid",data_train$loan_status)
data_train$loan_status = factor(data_train$loan_status,levels=c("Default","Fully.Paid"))
trans_model = preProcess(data_train,method=c("center","scale"))
data_train = predict(trans_model, data_train)
model = lrm(loan_status ~ .,data_train)
model
data_train = (data_train[,!(names(data_train) %in% tmp$feature)])
rm(model,tmp)
colnames(data_train) = str_replace_all(colnames(data_train)," ","_")
colnames(data_train) = str_replace_all(colnames(data_train),"<","_")
colnames(data_train) = str_replace_all(colnames(data_train),"/","_")
str_c("'",paste(colnames(data_train),collapse="','"),"'")
keep_columns = c('term_60_months','sub_gradeA2','sub_gradeA3','sub_gradeA4','sub_gradeA5','sub_gradeB1','sub_gradeB2','sub_gradeB3','sub_gradeB4','sub_gradeB5','sub_gradeC1','sub_gradeC2','sub_gradeC3','sub_gradeC4','sub_gradeC5','sub_gradeD1','sub_gradeD2','sub_gradeD3','sub_gradeD4','sub_gradeD5','sub_gradeE1','sub_gradeE2','sub_gradeE3','sub_gradeE4','sub_gradeE5','sub_gradeF1','sub_gradeF2','sub_gradeF3','sub_gradeF4','sub_gradeF5','sub_gradeG1','sub_gradeG2','sub_gradeG3','sub_gradeG4','sub_gradeG5','emp_lengthn_a','home_ownershipRENT','verification_statusSource_Verified','verification_statusVerified','purposecredit_card','purposedebt_consolidation','purposehome_improvement','purposemajor_purchase','purposemedical','purposemoving','purposeother','purposesmall_business','purposevacation','dti','delinq_2yrs','earliest_cr_line','mths_since_last_delinq','application_typeJoint_App','max_bal_bc','all_util','inq_fi','total_cu_tl','avg_cur_bal','bc_open_to_buy','mort_acc','mths_since_recent_bc','num_actv_bc_tl','num_bc_tl','num_tl_90g_dpd_24m','percent_bc_gt_75','is_ny','is_pa','is_nj','is_co','is_ga','loan_status')
applyFeatureTransformations <- function(dt,use_kept_column = keep_columns,use_median_impute_model=median_impute_model,  use_dummy_model=dummy_model,use_trans_model=trans_model){
  #consolidate loan status
  dt$loan_status = ifelse(str_detect(dt$loan_status,"Paid"),dt$loan_status,"Default")
  #parse int_rate
  dt$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = dt$int_rate)))
  #impute median
  dt = predict(median_impute_model,dt)
  #parse revol_util
  dt$revol_util = (as.numeric(gsub(pattern = "%",replacement = "",x = dt$int_rate)))
  dt$earliest_cr_line = parse_date_time(str_c("01",dt$issue_d),"dmy" ) - parse_date_time(str_c("01",dt$earliest_cr_line),"dmy" )
  dt$earliest_cr_line = as.numeric(dt$earliest_cr_line,units = "days")
  #binary variables for addr_state
  dt$is_ny = ifelse(dt$addr_state=="NY",1,0)
  dt$is_pa = ifelse(dt$addr_state=="PA",1,0)
  dt$is_nj = ifelse(dt$addr_state=="NJ",1,0)
  dt$is_oh = ifelse(dt$addr_state=="OH",1,0)
  dt$is_fl = ifelse(dt$addr_state=="FL",1,0)
  dt$is_co = ifelse(dt$addr_state=="CO",1,0)
  dt$is_ga = ifelse(dt$addr_state=="GA",1,0)
  dt$is_va = ifelse(dt$addr_state=="VA",1,0)
  dt$is_az = ifelse(dt$addr_state=="AZ",1,0)
  dt$is_ca = ifelse(dt$addr_state=="CA",1,0)
  #transform transactions
  dt$annual_inc = dt$annual_inc/dt$funded_amnt
  dt$revol_bal = dt$revol_bal/dt$funded_amnt
  dt$avg_cur_bal = dt$avg_cur_bal/dt$funded_amnt
  dt$bc_open_to_buy = dt$bc_open_to_buy/dt$funded_amnt
  #if purpose falling outside of recognized values
  all_purpose = c('debt_consolidation','small_business','other','credit_card','major_purchase','moving','home_improvement','house','car','medical','renewable_energy','vacation','wedding')
  dt$purpose = ifelse(dt$purpose %in% all_purpose,dt$purpose,"other")
  #create dummy variables
  loan_status = dt$loan_status
  dt = as.data.frame(predict(use_dummy_model,dt))
  dt$loan_status = loan_status
  #center,scale data
  trans_model_test = preProcess(dt,method=c("center","scale"))
  dt = predict(trans_model_test, dt)    
  #remove all unused features
  colnames(dt) = str_replace_all(colnames(dt)," ","_")
  colnames(dt) = str_replace_all(colnames(dt),"<","_")
  colnames(dt) = str_replace_all(colnames(dt),"/","_")
  dt = dt[use_kept_column]
  #set loan with status 'Fully Paid' as a positive sample
  dt$loan_status = ifelse(dt$loan_status == "Fully Paid","Fully.Paid",dt$loan_status)
  dt$loan_status = factor(dt$loan_status,levels = c("Default","Fully.Paid"))
  return(dt)
}
data_test
#later used for evaluate investment
data_test_grade = data_test$grade
data_test_funded_amnt = data_test$funded_amnt
data_test_total_pymnt = data_test$total_pymnt
data_test = applyFeatureTransformations(data_test)
