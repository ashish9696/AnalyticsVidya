rm(list=ls())
setwd("H:\\my work\\knocktober")

#load required libraries
library(data.table)
library(lubridate)

#load data
train <- fread("Train.csv", na.strings = c("NA"," ",NA))
test <- fread("Test.csv",na.strings = c("NA"," ",NA))
camp_detail <- fread("Health_Camp_Detail.csv",na.strings = c("NA"," ",NA))
patient_profile <- fread("Patient_Profile.csv",na.strings = c("NA"," ",NA))
attended_health_camp_1 <- fread("First_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_2 <- fread("Second_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))
attended_health_camp_3 <- fread("Third_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))


#create train and test data based on information given
# camp_detail[,Camp_Start_Date := dmy(Camp_Start_Date)]
# camp_detail[,Camp_End_Date := dmy(Camp_End_Date)]
# camp_detail[, datediff:=difftime(Camp_End_Date, Camp_Start_Date, units = "days")]
# camp_detail[, weekdiff:=difftime(Camp_End_Date, Camp_Start_Date, units = "weeks")]
train <- camp_detail[train,on="Health_Camp_ID"]
test <- camp_detail[test,on="Health_Camp_ID"]

# patient_profile[,First_Interaction := dmy(First_Interaction)]
train <- patient_profile[train,on="Patient_ID"]
test <- patient_profile[test,on="Patient_ID"]
#registration date
# train[,Registration_Date := dmy(Registration_Date)]
# test[,Registration_Date := dmy(Registration_Date)]
# train[, aware_time:=difftime( Registration_Date,First_Interaction, units = "days")]
# test[, aware_time:=difftime( Registration_Date,First_Interaction, units = "days")]
# train[, aware_week:=difftime( Registration_Date,First_Interaction, units = "weeks")]
# test[, aware_week:=difftime( Registration_Date,First_Interaction, units = "weeks")]

#prepare to create and add target variable in train file
attended_health_camp_1[,Target := 1L]
attended_health_camp_2[,Target := 1L]
attended_health_camp_3[Last_Stall_Visited_Number != 0, Target := 1L]
attended_health_camp_3[Last_Stall_Visited_Number < 1, Target := 0L]
attended_health_camp_1 <- attended_health_camp_1[,.(Health_Camp_ID,
                                                    Patient_ID,Target)]
attended_health_camp_2 <- attended_health_camp_2[,.(Health_Camp_ID,
                                                    Patient_ID,Target)]
attended_health_camp_3 <- attended_health_camp_3[,.(Health_Camp_ID,
                                                    Patient_ID,Target)]

attended_all <- rbindlist(list(attended_health_camp_1,
                               attended_health_camp_2,attended_health_camp_3),fill = TRUE)
attended_all$Target[is.na(attended_all$Target)] <- 1

train <- attended_all[train,on=c("Health_Camp_ID","Patient_ID")]
train$Target[is.na(train$Target)] <- 0
test <- attended_all[test,on=c("Health_Camp_ID","Patient_ID")]
test$Target=NULL
#check count of 1 and 0
table(train$Target) #train data ready

# Set Date class ---------------------------------------------------------------

index <- c(11,14,15,19)
train[,(index) := lapply(.SD,
                         function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]
index <- c(10,13,14,18)
test[,(index) := lapply(.SD, 
                        function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]

# write.csv(train,"trainf.csv",row.names = F)
#  write.csv(test,"testf.csv",row.names = F)

#Simple Features + Modeling

#Test Age
test[Age == "30",Age := 31]
sort(unique(test$Age),decreasing = T)
sort(unique(train$Age),decreasing = T)
#Convert numeric to categorical/binning
test$Age[test$Age >= 30 & test$Age <= 45] = "Low"
test$Age[test$Age > 45 & test$Age <= 60] = "Medium"
test$Age[test$Age > 60 & test$Age <= 80] = "High"
##
train$Age[train$Age >= 30 & train$Age <= 45] = "Low"
train$Age[train$Age > 45 & train$Age <= 60] = "Medium"
train$Age[train$Age > 60 & test$Age <= 80] = "High"
###
sort(unique(train$Education_Score),decreasing = T)
#creating variables
train[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
train[,Camp_Duration := as.numeric(as.character(Camp_Duration))]

test[,Camp_Duration := Camp_End_Date - Camp_Start_Date]
test[,Camp_Duration := as.numeric(as.character(Camp_Duration))]
train[,P_Month := month(First_Interaction,label=TRUE)]

test[,P_Month := month(First_Interaction,label=TRUE)]



train[,R_Month := month(Registration_Date,label=TRUE)]

test[,R_Month := month(Registration_Date,label=TRUE)]
train[,Camp_Month := month(Camp_End_Date,label=TRUE)]

test[,Camp_Month := month(Camp_End_Date,label=TRUE)]
train[,Camp_Start := month(Camp_Start_Date,label=TRUE)]

test[,Camp_Start := month(Camp_Start_Date,label=TRUE)]

train[,c("Camp_Start_Date","Camp_End_Date") := NULL]
test[,c("Camp_Start_Date","Camp_End_Date") := NULL]

train[,Patient_Response := Registration_Date - First_Interaction]
train[,Patient_Response := as.numeric(as.character(Patient_Response))]

test[,Patient_Response := Registration_Date - First_Interaction]
test[,Patient_Response := as.numeric(as.character(Patient_Response))]

train[,c("Registration_Date","First_Interaction") := NULL]
test[,c("Registration_Date","First_Interaction") := NULL]

#removing ID variables
train <- train[,-c("Patient_ID"),with=FALSE]
test_ID <- test[,.(Patient_ID,Health_Camp_ID)]
test <- test[,-c("Patient_ID"),with=FALSE]
# train$weekdiff=as.integer(train$weekdiff)
# train$aware_week=as.integer(train$aware_week)
# test$weekdiff=as.integer(test$weekdiff)
# test$aware_week=as.integer(test$aware_week)
##category1
# test$Category1 = factor(test$Category1, labels=(1:length(levels(factor(test$Category1)))))
# test$Category1 = as.numeric(test$Category1)
# train$Category1 = factor(train$Category1, labels=(1:length(levels(factor(train$Category1)))))
# train$Category1 = as.numeric(train$Category1)
##health camp
test$Health_Camp_ID = factor(test$Health_Camp_ID, 
                             labels=(1:length(levels(factor(test$Health_Camp_ID)))))
test$Health_Camp_ID = as.numeric(test$Health_Camp_ID)
train$Health_Camp_ID = factor(train$Health_Camp_ID, 
                              labels=(1:length(levels(factor(train$Health_Camp_ID)))))
train$Health_Camp_ID = as.numeric(train$Health_Camp_ID)
##age group
test$Age = factor(test$Age, 
                  labels=(1:length(levels(factor(test$Age)))))
test$Age = as.numeric(test$Age)
train$Age = factor(train$Age, 
                   labels=(1:length(levels(factor(train$Age)))))
train$Age = as.numeric(train$Age)
##income
test$Income = factor(test$Income, 
                     labels=(1:length(levels(factor(test$Income)))))
test$Income = as.numeric(test$Income)
train$Income = factor(train$Income, 
                      labels=(1:length(levels(factor(train$Income)))))
train$Income = as.numeric(train$Income)
##category
test$Category1 = factor(test$Category1, 
                        labels=(1:length(levels(factor(test$Category1)))))
test$Category1 = as.numeric(test$Category1)
train$Category1 = factor(train$Category1, 
                         labels=(1:length(levels(factor(train$Category1)))))
train$Category1 = as.numeric(train$Category1)
##category2
test$Category2 = factor(test$Category2, 
                        labels=(1:length(levels(factor(test$Category2)))))
test$Category2 = as.numeric(test$Category2)
train$Category2 = factor(train$Category2, 
                         labels=(1:length(levels(factor(train$Category2)))))
train$Category2 = as.numeric(train$Category2)
##Education_Score
test$Education_Score = factor(test$Education_Score, 
                              labels=(1:length(levels(factor(test$Education_Score)))))
test$Education_Score = as.numeric(test$Education_Score)
train$Education_Score = factor(train$Education_Score, 
                               labels=(1:length(levels(factor(train$Education_Score)))))
train$Education_Score = as.numeric(train$Education_Score)
##Employer_Category
test$Employer_Category = factor(test$Employer_Category, 
                                labels=(1:length(levels(factor(test$Employer_Category)))))
test$Employer_Category = as.numeric(test$Employer_Category)
train$Employer_Category = factor(train$Employer_Category, 
                                 labels=(1:length(levels(factor(train$Employer_Category)))))
train$Employer_Category = as.numeric(train$Employer_Category)
##month
##Employer_Category
test$P_Month = factor(test$P_Month, 
                                labels=(1:length(levels(factor(test$P_Month)))))
test$P_Month = as.numeric(test$P_Month)
train$P_Month = factor(train$P_Month, 
                                 labels=(1:length(levels(factor(train$P_Month)))))
train$P_Month = as.numeric(train$P_Month)
###month

test$R_Month = factor(test$R_Month, 
                      labels=(1:length(levels(factor(test$R_Month)))))
test$R_Month = as.numeric(test$R_Month)
train$R_Month = factor(train$R_Month, 
                       labels=(1:length(levels(factor(train$R_Month)))))
train$R_Month = as.numeric(train$R_Month)
##month

test$Camp_Month = factor(test$Camp_Month, 
                      labels=(1:length(levels(factor(test$Camp_Month)))))
test$Camp_Month = as.numeric(test$Camp_Month)
train$Camp_Month = factor(train$Camp_Month, 
                       labels=(1:length(levels(factor(train$Camp_Month)))))
train$Camp_Month = as.numeric(train$Camp_Month)
##month

test$Camp_Start = factor(test$Camp_Start, 
                         labels=(1:length(levels(factor(test$Camp_Start)))))
test$Camp_Start = as.numeric(test$Camp_Start)
train$Camp_Start = factor(train$Camp_Start, 
                          labels=(1:length(levels(factor(train$Camp_Start)))))
train$Camp_Start = as.numeric(train$Camp_Start)
##City_Type
test$City_Type = factor(test$City_Type, 
                        labels=(1:length(levels(factor(test$City_Type)))))
test$City_Type = as.numeric(test$City_Type)
train$City_Type = factor(train$City_Type, 
                         labels=(1:length(levels(factor(train$City_Type)))))
train$City_Type = as.numeric(train$City_Type)
test$Health_Camp_ID=NULL
train$Health_Camp_ID=NULL

train$Share_Score<-(train$Facebook_Shared+train$LinkedIn_Shared+train$Twitter_Shared+train$Online_Follower)
test$Share_Score<-(test$Facebook_Shared+test$LinkedIn_Shared+test$Twitter_Shared+test$Online_Follower)
# train$Facebook_Shared=NULL
# train$Online_Follower=NULL
# train$LinkedIn_Shared=NULL
# train$Twitter_Shared=NULL
# train$Facebook_Shared=NULL
# test$Facebook_Shared=NULL
# test$Online_Follower=NULL
# test$LinkedIn_Shared=NULL
# test$Twitter_Shared=NULL
sort(unique(test$Education_Score),decreasing = T)
write.csv(test,"test_modified.csv",row.names = F)
write.csv(train,"train_modified.csv",row.names = F)
#predictions
train$Var1=as.numeric(train$Var1)
train$Var5=as.numeric(train$Var5)
train$Var4=as.numeric(train$Var4)
train$Var2=as.numeric(train$Var2)
train$Share_Score=as.numeric(train$Share_Score)
# 
test$Share_Score=as.numeric(test$Share_Score)
test$Var1=as.numeric(test$Var1)
test$Var5=as.numeric(test$Var5)
test$Var4=as.numeric(test$Var4)
test$Var2=as.numeric(test$Var2)

#####################################################
#################################################
#####data analysis done #################################
#######################################################
#######################################################


library(rpart)
#logistic regression
library(randomForest)
library(gbm)
library(rpart)
library(arm)
library(gam)
library(kernlab)
library(rpart)
# ##7947
# library(randomForest)
# trainf[is.na(trainf)] <- -1
# 
# test[is.na(test)] <- 0
# 
# fit_regrex = randomForest(Target ~ ., train, ntree = 500, importance = TRUE)
# pred = predict(fit_regrex, test)
# importance(fit_regrex, type = 1)

###
## run our first predictive model
h2o.init()
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
features<-c(2:23)

##gbm3   0.79279
gbm3 <- h2o.gbm(
  training_frame = train.h2o,     ##
  ##
  x=features,                     ##
  y=1,                       ## 
  ntrees = 27,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.091,           ## increase the learning rate even further
  max_depth = 5,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 10,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 3000000) 
predict.reg <- as.data.frame(h2o.predict(gbm3, test.h2o))

sub_reg <- data.frame(  Patient_ID=test_ID$Patient_ID,
                        Health_Camp_ID=test_ID$Health_Camp_ID,Outcome=predict.reg$predict)
write.csv(sub_reg, file = "sub_reg.csv", row.names = F)



#################################################33333


library(mlr)
#create task
training.task <- makeRegrTask(data=train, target = "Target")

train.task <- makeRegrTask(data=train,target = "Target")
test$Target<-0
test.task <- makeRegrTask(data=test,target = "Target")

#get variable importance chart based on information gain

var_imp <- generateFilterValuesData(training.task, method = c("information.gain"))

plotFilterValues(var_imp,feat.type.cols = TRUE)

train$Category3=NULL
train$Var3=NULL

#create learner

set.seed(1122)

listLearners("regr")[c("class","type")]
library(deeplearning)


# build model on train data -----------------------------------------------

#create learner

set.seed(1123)

listLearners("regr")[c("class","type")]

gbm2.learner <- makeLearner("regr.gbm")
gbm3.learner <- makeLearner("regr.gbm")
gbm1.learner <- makeLearner("regr.gbm")

##0.011 7890  0.091  7892 0.099 79008 30 6 7904 5 0.7915
gbm2.learner$par.vals <- list(
  
  interaction.depth = 5,
  
  shrinkage = 0.099,
  
  n.trees = 30
  
)

#train model79144
gbm3.learner$par.vals <- list(
  
  interaction.depth = 3,
  
  shrinkage = 0.099,
  
  n.trees = 25
  
)

t_gbm <- train(gbm2.learner,train.task)
l_gbm <- train(gbm3.learner,train.task)

# prediciton
p_gbm <- predict(t_gbm,test.task)
r_gbm <- predict(l_gbm,test.task)

#write prediction file

f_pred1 <- (0.05*p_gbm$data$response+0.03*r_gbm$data$response+0.92*predict.reg$predict)
f_pred2 <- (0.15*p_gbm$data$response+0.12*r_gbm$data$response+0.73*predict.reg$predict)
f_pred<-(0.8*f_pred1+0.2*f_pred2)
prediction <- data.frame(Patient_ID=test_ID$Patient_ID,
                         Health_Camp_ID=test_ID$Health_Camp_ID, Outcome = f_pred1)

write.csv(prediction,"Student_Hunt_Sub.csv",row.names = FALSE)
##deep learning

