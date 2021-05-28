require(methods)
setwd('C:\\RLang\\CreditRiskModelling')
loandata <- read.csv('loan_data_2017.csv')
str(loandata)

 install.packages("ROCR")
library(ROCR)
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
install.packages("magrittr") # package installations are only needed the first time you use it
library(magrittr) # needs to be run every time you start R and want to use %>%
install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)    # alternatively, this also loads %>%
install.packages("ggplot2") # Install it again
library(ggplot2) #

g1 = loandata %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(default_count = n())
str(loandata)
g1
g2 = loandata %>% group_by(grade) %>% summarise(count = n())
 g3 <- g2 %>% left_join(g1) %>% mutate(default_rate = 100*default_count/count) %>% select(grade,count,default_count,default_rate)
 g3
ggplot(g3, aes(x=grade, y=default_rate, fill=grade)) + geom_bar(stat="identity")
loandata$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = loandata$int_rate)))

x1 = loandata %>% filter(loan_status == "Default") %>% group_by(grade) %>% summarise(int_rate=mean(int_rate))
ggplot(x1, aes(x=grade, y=int_rate, fill=grade)) + geom_bar(stat="identity",position="dodge")
install.packages('dplyr')
 library(dplyr)
 install.packages('stringr')
 library('stringr')
install.packages('ggplot2')
 library('ggplot2')
install.packages('caret')
library(caret)
install.packages("lubridate")
library(lubridate)
install.packages("corrplot")
library(corrplot)
install.packages("rms")
 library(rms)

#install.packages("doMC")
#library(doMC)
install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
install.packages("pROC")
library(pROC)
install.packages("e1071")
 library(e1071)
install.packages("kernlab")
library(kernlab)
install.packages("xgboost")
library(xgboost)
# Sample Indexes
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

str(data_train$earliest_cr_line)
str(data_train$issue_d)
data_train = (data_train[,!(names(data_train) %in% discard_column)])
 dim(data_train)
data_train$grade = NULL
data_train <- data_train[, -which(colMeans(is.na(data_train)) > 0.5)]
 tmp = sort(sapply(data_train, function(x) sum(length(which(is.na(x)))))/nrow(data_train),decreasing = TRUE)

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
data_train$earliest_cr_line = parse_date_time(str_c("01",data_train$issue_d),"dmy" ) - parse_date_time(str_c("01",data_train$earliest_cr_line),"dmy" )
data_train$earliest_cr_line = as.numeric(data_train$earliest_cr_line,units = "days")

data_train$issue_m = sapply( data_train$issue_d ,function(x){str_split(x,"-")[[1]][1]})

tmp = data_train %>% filter(loan_status=="Default") %>% group_by(issue_m) %>% summarise(default_count = n())
tmp2 = data_train %>% group_by(issue_m) %>% summarise(count = n())
tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
data_train$issue_m = NULL
data_train$issue_d = NULL
data_train$earliest_cr_line=NULL
rm(tmp,tmp2)
# Returns the Numeric columns from a dataset
getNumericColumns<-function(t){
    tn = sapply(t,function(x){is.numeric(x)})
    return(names(tn)[which(tn)])
}
# Returns the character columns from a dataset
getCharColumns<-function(t){
    tn = sapply(t,function(x){is.character(x)})
    return(names(tn)[which(tn)])
}
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
table(data_train$purpose)
table(data_train$title)
data_train = (data_train[,!(names(data_train) %in% discard_column)])
data_train$title = NULL
str(data_train$desc)
data_train$desc = NULL
tmp = data_train %>% filter(loan_status=="Default") %>% group_by(addr_state) %>% summarise(default_count = n())
tmp2 = data_train %>% group_by(addr_state) %>% summarise(count = n())
tmp3 = tmp2 %>% left_join(tmp) %>% mutate(default_rate = default_count/count)
tmp3
#order by highest default rate

high_default = (tmp3 %>% filter(count > 1000) %>% arrange(desc(default_rate)))[1:10,"addr_state"]$addr_state

high_default
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
corrplot(cor(data_train[getNumericColumns(data_train)],use="na.or.complete"))
 #str(findCorrelation(cor(data_train[getNumericColumns(data_train)]), cutoff = .75))
cor(data_train[getNumericColumns(data_train)])
 high_corr <- findCorrelation(cor(data_train[getNumericColumns(data_train)]), cutoff = .75)
 high_corr = getNumericColumns(data_train)[high_corr]
 high_corr
data_train = (data_train[,!(names(data_train) %in% high_corr)])
str(data_train[getNumericColumns(data_train)])
data_train$annual_inc = data_train$annual_inc/data_train$funded_amnt
data_train$revol_bal = data_train$revol_bal/data_train$funded_amnt
data_train$avg_cur_bal = data_train$avg_cur_bal/data_train$funded_amnt
data_train$bc_open_to_buy = data_train$bc_open_to_buy/data_train$funded_amnt
data_train$funded_amnt = NULL
str(data_train[getCharColumns(data_train)])
data_train$verification_status_joint = NULL
table(data_train$home_ownership)
data_train %>% filter(pymnt_plan=="y") %>% select(pymnt_plan, loan_status)
data_train$pymnt_plan = NULL
loan_status = data_train$loan_status
table(loan_status)
 dummy_model = dummyVars(loan_status ~ .,data_train,fullRank = TRUE)
 data_train = as.data.frame(predict(dummy_model,data_train))
 data_train$loan_status = loan_status
 #rm(loan_status)

#set loan with status Fully Paid as a positive sample
 data_train$loan_status = ifelse(data_train$loan_status == "Fully Paid","Fully.Paid",data_train$loan_status)
data_train$loan_status = ifelse(data_train$loan_status != "Fully.Paid","Default",data_train$loan_status)

 data_train$loan_status = factor(data_train$loan_status,levels = c("Default","Fully.Paid"))
# data_train$loan_status = ifelse(data_train$loan_status != "Fully.Paid","Default",data_train$loan_status)

 

#data_train$loan_status = ifelse(data_train$loan_status != "Fully.Paid","Default",data_train$loan_status)

 #data_train$loan_status = ifelse(data_train$loan_status == "Charged Off","Charged.Off",data_train$loan_status)
#data_train$loan_status = ifelse(data_train$loan_status != "Fully.Paid","Default",data_train$loan_status)


 # data_train$loan_status = as.factor(data_train$loan_status)
# data_train$loan_status = factor(data_train$loan_status,levels = c("Fully.Paid", "Current","Charged.Off"))
#data_train$loan_status = ifelse(data_train$loan_status == "Fully Paid",'1',data_train$loan_status)
#data_train$loan_status = ifelse(data_train$loan_status != "1",'0',data_train$loan_status)
#data_train$loan_status = factor(data_train$loan_status,levels = c('1', '0'))

trans_model = preProcess(data_train,method=c("center","scale"))
data_train = predict(trans_model, data_train)
install.packages("rms")
 library(rms)
#install.packages("DescTools")
#    library(DescTools)
model = lrm(loan_status ~ .,data_train)

model
tmp = as.data.frame(anova(model))
 tmp$feature = rownames(tmp)
 tmp = tmp %>% filter(P > 0.01) %>% select(feature,P)
 tmp
data_train = (data_train[,!(names(data_train) %in% tmp$feature)])
rm(model,tmp)
colnames(data_train) = str_replace_all(colnames(data_train)," ","_")
colnames(data_train) = str_replace_all(colnames(data_train),"<","_")
colnames(data_train) = str_replace_all(colnames(data_train),"/","_")
str_c("'",paste(colnames(data_train),collapse="','"),"'")
keep_columns = c('term_60_months','sub_gradeA2','sub_gradeA3','sub_gradeA4','sub_gradeA5','sub_gradeB1','sub_gradeB2','sub_gradeB3','sub_gradeB4','sub_gradeB5','sub_gradeC1','sub_gradeC2','sub_gradeC3','sub_gradeC4','sub_gradeC5','sub_gradeD1','sub_gradeD2','sub_gradeD3','sub_gradeD4','sub_gradeD5','sub_gradeE1','sub_gradeE2','sub_gradeE3','sub_gradeE4','sub_gradeE5','sub_gradeF1','sub_gradeF2','sub_gradeF3','sub_gradeF4','sub_gradeF5','sub_gradeG1','sub_gradeG2','sub_gradeG3','sub_gradeG4','sub_gradeG5','emp_lengthn_a','home_ownershipRENT','verification_statusSource_Verified','verification_statusVerified','purposecredit_card','purposedebt_consolidation','purposehome_improvement','purposemajor_purchase','purposemedical','purposemoving','purposeother','purposesmall_business','purposevacation','dti','delinq_2yrs','earliest_cr_line','mths_since_last_delinq','application_typeJoint_App','max_bal_bc','all_util','inq_fi','total_cu_tl','avg_cur_bal','bc_open_to_buy','mort_acc','mths_since_recent_bc','num_actv_bc_tl','num_bc_tl','num_tl_90g_dpd_24m','percent_bc_gt_75','is_ny','is_pa','is_nj','is_co','is_ga','loan_status') 
keep_columns
use_kept_column = keep_columns
applyFeatureTransformations <- function(dt,use_kept_column ,use_median_impute_model=median_impute_model,  use_dummy_model=dummy_model,use_trans_model=trans_model){
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
#dim(data_test)

data_test
data_test$disbursement_method
#later used for evaluate investment
data_test_grade = data_test$grade
data_test_funded_amnt = data_test$funded_amnt
data_test_total_pymnt = data_test$total_pymnt

data_test = applyFeatureTransformations(data_test)
100*nrow(data_test %>% filter(loan_status=="Fully.Paid"))/nrow(data_test)
set.seed(100)
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
 table(samp$loan_status)
train_index = createDataPartition(samp$loan_status,p = 0.05,list=FALSE,times=1)
ctrl <- trainControl(method = "cv",
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    number = 3
    )
#install.packages("ROCR”)
#library(ROCR)

glmnGrid = expand.grid(.alpha = seq(0, 1, length = 10), .lambda = 0.01)
#install.packages("pROC")
#library(pROC)

glmnTuned = train(samp[train_index,-getIndexsOfColumns(samp,"loan_status")],y = samp[train_index,"loan_status"],method = "glmnet",tuneGrid = glmnGrid,metric = "ROC",trControl = ctrl)
plot(glmnTuned)
glmnTuned
install.packages("glmnet")
 
library(glmnet)
#samp$loan_status=NULL
x = as.matrix(samp[-getIndexsOfColumns(samp,"loan_status")])
newx = as.matrix(data_test[-getIndexsOfColumns(data_test,"loan_status")])
x=x[1:nrow(newx),]
newx=newx[,1:ncol(x)]

model = glmnet(
    x = as.matrix(samp[-getIndexsOfColumns(samp,"loan_status")]),
    y=samp$loan_status,
    alpha = 1,
    lambda = 0.01,
    family = "binomial",
    standardize = FALSE)
table_perf = data.frame(model=character(0),
                        auc=numeric(0),
                        accuracy=numeric(0),
                        sensitivity=numeric(0),
                        specificity=numeric(0),
                        kappa=numeric(0),
                        stringsAsFactors = FALSE
                        )



predict_loan_status_logit = predict(model,newx ,s="lambda.min",type="response")


library(pROC)
rocCurve_logit = roc(response = data_test$loan_status,
               predictor = predict_loan_status_logit)
auc_curve = auc(rocCurve_logit)
plot(rocCurve_logit,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(Logistic Regression)")
rocCurve_logit
predict_loan_status_label = ifelse(predict_loan_status_logit<0.5,"Default","Fully.Paid")
positive="Fully.Paid"
 c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),as.character(positive))
 table_perf[1,] = c("logistic regression",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
 rm(samp,train_index)
 tail(table_perf,1)
set.seed(200)
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
 table(samp$loan_status)
#choose small data for tuning 
train_index_tuning = createDataPartition(samp$loan_status,p = 0.05,list=FALSE,times=1)
#choose small data for re-train
train_index_training = createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
library("kernlab")
svmGrid = expand.grid(
                .sigma = as.numeric(sigest(loan_status ~.,data = samp[train_index_tuning,],scaled=FALSE)),
                .C = c(0.1,1,10)
                )

svmTuned = train(
    samp[train_index_tuning,-getIndexsOfColumns(samp,"loan_status")],
    y = samp[train_index_tuning,"loan_status"],
    method = "svmRadial",
    tuneGrid = svmGrid,
    metric = "ROC",
    trControl = ctrl,
    preProcess = NULL,
    scaled = FALSE,
    fit = FALSE)
plot(svmTuned)
svmTuned
svm_model = ksvm(loan_status ~ .,
                 data = samp[train_index_training,],
                 kernel = "rbfdot",
                 kpar = list(sigma=0.003796662),
                 C = 0.1,
                 prob.model = TRUE,
                 scaled = FALSE)
predict_loan_status_svm = predict(svm_model,data_test,type="probabilities")
predict_loan_status_svm = as.data.frame(predict_loan_status_svm)$Fully.Paid
rocCurve_svm = roc(response = data_test$loan_status,
               predictor = predict_loan_status_svm)
auc_curve = auc(rocCurve_svm)
plot(rocCurve_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc_curve
predict_loan_status_label = ifelse(predict_loan_status_svm<0.5,"Default","Fully.Paid")

c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive)
table_perf[2,] = c("SVM",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )

 tail(table_perf,1)
set.seed(300)
#down sampling again so than we get more info when stacking
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
#choose small data for tuning 
train_index_tuning = createDataPartition(samp$loan_status,p = 0.05,list=FALSE,times=1)
#choose small data for re-train
train_index_training = createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
rfGrid = expand.grid(
                .mtry = as.integer(seq(2,ncol(samp), (ncol(samp) - 2)/4))
                )
#Install random forest package
library(randomForest)
rfTuned = train(
    samp[train_index_tuning,-getIndexsOfColumns(samp,"loan_status")],
    y = samp[train_index_tuning,"loan_status"],
    method = "rf",
    tuneGrid = rfGrid,
    metric = "ROC",
    trControl = ctrl,
    preProcess = NULL,
    ntree = 100
    )
plot(rfTuned)
rfTuned
rf_model = randomForest(loan_status ~ . ,data = samp[train_index_training,],mtry = 2,ntree=400)
predict_loan_status_rf = predict(rf_model,data_test,"prob")
predict_loan_status_rf = as.data.frame(predict_loan_status_rf)$Fully.Paid
rocCurve_rf = roc(response = data_test$loan_status,
               predictor = predict_loan_status_rf)
auc_curve = auc(rocCurve_rf)
plot(rocCurve_rf,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(RandomForest)")
 rocCurve_rf
predict_loan_status_label = ifelse(predict_loan_status_rf<0.5,"Default","Fully.Paid")

c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive)

table_perf[3,] = c("RandomForest",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
tail(table_perf,1)
set.seed(400)
#down sampling again so than we get more info when stacking
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
#choose small data for validating
train_index_tuning= createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
etas = c(0.1,0.3)
alphas = c(0,0.5,1)
lambdas = c(0,0.5,1)

install.packages("xgboost")
library(xgboost)

test_watchlist = list(
    test = xgb.DMatrix(
        data = as.matrix(samp[train_index_tuning,][getNumericColumns(samp)]),
        label = as.numeric(samp[train_index_tuning,"loan_status"])-1
    )
)

gbm_perf = data.frame(eta=numeric(0),alpha=numeric(0),lambda=numeric(0),auc=numeric(0))

for(eta in etas){
    for(alpha in alphas){
        for(lambda in lambdas){
            model = xgb.train(data= xgb.DMatrix(data = as.matrix(samp[-train_index_tuning,][getNumericColumns(samp)]),label = as.numeric(samp[-train_index_tuning,"loan_status"])-1),
                
                objective = "binary:logistic",
                nrounds = 350,
                watchlist = test_watchlist,
                eval_metric = "auc",
                early.stop.rounds = 10,
                alpha = alpha,
                lambda = lambda,
                eta = eta)
            gbm_perf[nrow(gbm_perf)+1,] = c(eta,alpha,lambda,model$bestScore)#model$bestScore
           nrow(gbm_perf)

        }
    }
}

gbm_perf %>% arrange(desc(auc))
set.seed(400)
test_watchlist = list(
    test = xgb.DMatrix(
        data = as.matrix(samp[train_index_tuning,][getNumericColumns(samp)]),
        label = as.numeric(samp[train_index_tuning,"loan_status"])-1
    )
)

xgb_model = xgb.train(
                data= xgb.DMatrix(
                    data = as.matrix(samp[-train_index_tuning,][getNumericColumns(samp)]),
                    label = as.numeric(samp[-train_index_tuning,"loan_status"])-1
                ),
                objective = "binary:logistic",
                nrounds = 350,
                watchlist = test_watchlist,
                eval_metric = "auc",
                early.stop.round = 10,
                alpha = 0.5,
                lambda = 1.0,
                eta = 0.1)

xgb_model
d=xgb.DMatrix(
                    data = as.matrix(samp[-train_index_tuning,][getNumericColumns(samp)]),
                    label = as.numeric(samp[-train_index_tuning,"loan_status"])-1
                )
colnames(samp[-train_index_tuning,][getNumericColumns(samp)])
colnames(data_test[getNumericColumns(data_test)])
dtrnCols=colnames(samp[-train_index_tuning,][getNumericColumns(samp)])
dtstCols=colnames(data_test[getNumericColumns(data_test)])
dtrnCols
dtstCols
dtrn=samp[-train_index_tuning,][getNumericColumns(samp)]
dtst=data_test[getNumericColumns(data_test)]
new_dtst=dtst[,colnames(dtrn)]
ncol(dtrn)
ncol(new_dtst)
intersect(dtrnCols, dtstCols)
dtst[,colnames(dtrn)]
print ("THIS IS TRAINING")
dtrn
print ("THIS IS TEST")
dtst
itersct=intersect(colnames(samp[-train_index_tuning,][getNumericColumns(samp)]),colnames(data_test[getNumericColumns(data_test)]) )


#predict_loan_status_xgb = predict(xgb_model,as.matrix(data_test[getNumericColumns(data_test)]))
predict_loan_status_xgb = predict(xgb_model,as.matrix(new_dtst))

rocCurve_xgb = roc(response = data_test$loan_status,
               predictor = predict_loan_status_xgb)

auc_curve = auc(rocCurve_xgb)

plot(rocCurve_xgb,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(XGB)")
rocCurve_xgb
predict_loan_status_label = ifelse(predict_loan_status_xgb<0.5,"Default","Fully.Paid")
c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive="Fully.Paid")

table_perf[4,] = c("XGB",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
rm(samp,train_index)
tail(table_perf,1)
set.seed(200)
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
 table(samp$loan_status)
#choose small data for tuning 
train_index_tuning = createDataPartition(samp$loan_status,p = 0.05,list=FALSE,times=1)
#choose small data for re-train
train_index_training = createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
library("kernlab")
svmGrid = expand.grid(
                .sigma = as.numeric(sigest(loan_status ~.,data = samp[train_index_tuning,],scaled=FALSE)),
                .C = c(0.1,1,10)
                )

svmTuned = train(
    samp[train_index_tuning,-getIndexsOfColumns(samp,"loan_status")],
    y = samp[train_index_tuning,"loan_status"],
    method = "svmRadial",
    tuneGrid = svmGrid,
    metric = "ROC",
    trControl = ctrl,
    preProcess = NULL,
    scaled = FALSE,
    fit = FALSE)
plot(svmTuned)
svmTuned
svm_model = ksvm(loan_status ~ .,
                 data = samp[train_index_training,],
                 kernel = "rbfdot",
                 kpar = list(sigma=0.003796662),
                 C = 0.1,
                 prob.model = TRUE,
                 scaled = FALSE)
predict_loan_status_svm = predict(svm_model,data_test,type="probabilities")
predict_loan_status_svm = as.data.frame(predict_loan_status_svm)$Fully.Paid
rocCurve_svm = roc(response = data_test$loan_status,
               predictor = predict_loan_status_svm)
auc_curve = auc(rocCurve_svm)
 plot(rocCurve_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc_curve
predict_loan_status_label = ifelse(predict_loan_status_svm<0.5,"Default","Fully.Paid")
c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive="Fully.Paid")

table_perf[2,] = c("SVM",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )

 tail(table_perf,1)
set.seed(300)
#down sampling again so than we get more info when stacking
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
#choose small data for tuning 
train_index_tuning = createDataPartition(samp$loan_status,p = 0.05,list=FALSE,times=1)
#choose small data for re-train
train_index_training = createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
rfGrid = expand.grid(
                .mtry = as.integer(seq(2,ncol(samp), (ncol(samp) - 2)/4))
                )
#Install random forest package
library(randomForest)
rfTuned = train(
    samp[train_index_tuning,-getIndexsOfColumns(samp,"loan_status")],
    y = samp[train_index_tuning,"loan_status"],
    method = "rf",
    tuneGrid = rfGrid,
    metric = "ROC",
    trControl = ctrl,
    preProcess = NULL,
    ntree = 100
    )
plot(rfTuned)
rfTuned
rf_model = randomForest(loan_status ~ . ,data = samp[train_index_training,],mtry = 2,ntree=400)
predict_loan_status_rf = predict(rf_model,data_test,"prob")
predict_loan_status_rf = as.data.frame(predict_loan_status_rf)$Fully.Paid
rocCurve_rf = roc(response = data_test$loan_status,
               predictor = predict_loan_status_rf)
auc_curve = auc(rocCurve_rf)
plot(rocCurve_rf,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(RandomForest)")
rocCurve_rf
predict_loan_status_label = ifelse(predict_loan_status_rf<0.5,"Default","Fully.Paid")
c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive="Fully.Paid")

table_perf[3,] = c("RandomForest",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
tail(table_perf,1)
set.seed(400)
#down sampling again so than we get more info when stacking
samp = downSample(data_train[-getIndexsOfColumns(data_train, c( "loan_status") )],data_train$loan_status,yname="loan_status")
#choose small data for validating
train_index_tuning= createDataPartition(samp$loan_status,p = 0.1,list=FALSE,times=1)
etas = c(0.1,0.3)
alphas = c(0,0.5,1)
lambdas = c(0,0.5,1)

install.packages("xgboost”)
library(xgboost)

test_watchlist = list(
    test = xgb.DMatrix(
        data = as.matrix(samp[train_index_tuning,][getNumericColumns(samp)]),
        label = as.numeric(samp[train_index_tuning,"loan_status"])-1
    )
)

gbm_perf = data.frame(eta=numeric(0),alpha=numeric(0),lambda=numeric(0),auc=numeric(0))
tryCatch( expr={
for(eta in etas){
    for(alpha in alphas){
        for(lambda in lambdas){
            model = xgb.train(
                data= xgb.DMatrix(
                    data = as.matrix(samp[-train_index_tuning,][getNumericColumns(samp)]),
                    label = as.numeric(samp[-train_index_tuning,"loan_status"])-1
                ),
                objective = "binary:logistic",
                nrounds = 350,
                watchlist = test_watchlist,
                eval_metric = "auc",
                early.stop.rounds = 10,
                alpha = alpha,
                lambda = lambda,
                eta = eta)
            gbm_perf[nrow(gbm_perf)+1,] = c(eta,alpha,lambda,model$bestScore)#model$bestScore
        }
    }
}
},
 error= function(e) {
  message('found error')
  print(e)
 },
 warning=function(w){
  message('warning')
  print(w)
 }
)


gbm_perf %>% arrange(desc(auc))
set.seed(400)
test_watchlist = list(
    test = xgb.DMatrix(
        data = as.matrix(samp[train_index_tuning,][getNumericColumns(samp)]),
        label = as.numeric(samp[train_index_tuning,"loan_status"])-1
    )
)
install.packages("xgboost")
library(xgboost)
xgb_model = xgb.train(
                data= xgb.DMatrix(
                    data = as.matrix(samp[-train_index_tuning,][getNumericColumns(samp)]),
                    label = as.numeric(samp[-train_index_tuning,"loan_status"])-1
                ),
                objective = "binary:logistic",
                nrounds = 350,
                watchlist = test_watchlist,
                eval_metric = "auc",
                early.stop.round = 10,
                alpha = 0.5,
                lambda = 1.0,
                eta = 0.1)
xgb_model

colnames(samp[-train_index_tuning,][getNumericColumns(samp)])
colnames(data_test[getNumericColumns(data_test)])
dtrnCols=colnames(samp[-train_index_tuning,][getNumericColumns(samp)])
dtstCols=colnames(data_test[getNumericColumns(data_test)])
dtrnCols
dtstCols
dtrn=samp[-train_index_tuning,][getNumericColumns(samp)]
dtst=data_test[getNumericColumns(data_test)]
new_dtst=dtst[,colnames(dtrn)]
colnames(new_dtst)

#predict_loan_status_xgb = predict(xgb_model,as.matrix(data_test[getNumericColumns(data_test)]))
predict_loan_status_xgb = predict(xgb_model,as.matrix(new_dtst))

rocCurve_xgb = roc(response = data_test$loan_status,
               predictor = predict_loan_status_xgb)

auc_curve = auc(rocCurve_xgb)

plot(rocCurve_xgb,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(XGB)")
rocCurve_xgb
predict_loan_status_label = ifelse(predict_loan_status_xgb<0.5,"Default","Fully.Paid")
c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive="Fully.Paid")

table_perf[4,] = c("XGB",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
tail(table_perf,1)
predict_loan_status_ensemble = predict_loan_status_logit +
                               predict_loan_status_svm +
                               predict_loan_status_rf +
                               predict_loan_status_xgb
predict_loan_status_ensemble = predict_loan_status_ensemble / 4
rocCurve_ensemble = roc(response = data_test$loan_status,
               predictor = predict_loan_status_ensemble)
auc_curve = auc(rocCurve_ensemble)
plot(rocCurve_ensemble,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(Ensemble Avg.)")
rocCurve_ensemble
predict_loan_status_label = ifelse(predict_loan_status_ensemble<0.5,"Default","Fully.Paid")
c = confusionMatrix(as.factor(predict_loan_status_label),as.factor(data_test$loan_status),positive="Fully.Paid")

table_perf[5,] = c("Ensemble",
  round(auc_curve,3),
  as.numeric(round(c$overall["Accuracy"],3)),
  as.numeric(round(c$byClass["Sensitivity"],3)),
  as.numeric(round(c$byClass["Specificity"],3)),
  as.numeric(round(c$overall["Kappa"],3))
  )
tail(table_perf,1)



