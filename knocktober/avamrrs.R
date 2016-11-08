setwd("C:\\Users\\SA31\\Downloads\\av_medcamp\\Train_2\\Train")



#load required libraries

library(data.table)

library(lubridate)

#library(randomForest)

library(ROCR)

#library(ROSE)





#load data

train <- fread("Train.csv", na.strings = c("NA"," ",NA))

test <- fread("Test.csv",na.strings = c("NA"," ",NA))

camp_detail <- fread("Health_Camp_Detail.csv",na.strings = c("NA"," ",NA))

patient_profile <- fread("Patient_Profile.csv",na.strings = c("NA"," ",NA))

attended_health_camp_1 <- fread("First_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))

attended_health_camp_2 <- fread("Second_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))

attended_health_camp_3 <- fread("Third_Health_Camp_Attended.csv",na.strings = c("NA"," ",NA))





#create train and test data based on information given



#train_f <- train[train_f,on='Patient_ID']



train <- camp_detail[train,on="Health_Camp_ID"]

test <- camp_detail[test,on="Health_Camp_ID"]



train <- patient_profile[train,on="Patient_ID"]

test <- patient_profile[test,on="Patient_ID"]



#prepare to create and add target variable in train file

attended_health_camp_1[,Target := 1L]

attended_health_camp_2[,Target := 1L]

attended_health_camp_3[Last_Stall_Visited_Number != 0, Target := 1L]

attended_health_camp_3[Last_Stall_Visited_Number < 1, Target := 0L]

attended_health_camp_1 <- attended_health_camp_1[,.(Health_Camp_ID,Patient_ID,Target)]

attended_health_camp_2 <- attended_health_camp_2[,.(Health_Camp_ID,Patient_ID,Target)]

attended_health_camp_3 <- attended_health_camp_3[,.(Health_Camp_ID,Patient_ID,Target)]



attended_all <- rbindlist(list(attended_health_camp_1,attended_health_camp_2,attended_health_camp_3))





#m1 <- merge(attended_all,patient_profile,by='Patient_ID',all.x = T)



#m2 <- merge(m1,camp_detail,by='Health_Camp_ID',all.x = T)



#t <- train[unique(train$Patient_ID) %in% train$Patient_ID,]



#m3 <- merge(train,m2,all = T)





train <- attended_all[train,on=c("Health_Camp_ID","Patient_ID")]

train$Target[is.na(train$Target)] <- 0





#train <- na.omit(train)





#train <- tidyr::drop_na(train,Target)



#zeros <- train[train$Target==0,]





#zeros2 <- zeros[sample(1:nrow(zeros),2000,replace=T),]



#trains <- train





#check count of 1 and 0

table(train$Target) #train data ready



table(attended_all$Target)



# Set Date class ---------------------------------------------------------------





index <- c(11,14,15,19)

train[,(index) := lapply(.SD,function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]

index <- c(9,13,14,18)

test[,(index) := lapply(.SD, function(x) as.Date(x,format="%d-%b-%y")),.SDcols = index]



# write.csv(train,"trainf.csv",row.names = F)

# write.csv(test,"testf.csv",row.names = F)



#Simple Features + Modeling



#Test Age

#test[Age == "30",Age := 31]

#sort(unique(test$Age),decreasing = T)

#sort(unique(train$Age),decreasing = T)



#creating variables

train[,Camp_Duration := Camp_End_Date - Camp_Start_Date]

train[,Camp_Duration := as.numeric(as.character(Camp_Duration))]



test[,Camp_Duration := Camp_End_Date - Camp_Start_Date]

test[,Camp_Duration := as.numeric(as.character(Camp_Duration))]





train[,Camp_Month := month(Camp_End_Date,label=TRUE)]

test[,Camp_Month := month(Camp_End_Date,label=TRUE)]









train[,c("Camp_Start_Date","Camp_End_Date") := NULL]

test[,c("Camp_Start_Date","Camp_End_Date") := NULL]



train[,Patient_Response := Registration_Date - First_Interaction]

train[,Patient_Response := as.numeric(as.character(Patient_Response))]



test[,Patient_Response := Registration_Date - First_Interaction]

test[,Patient_Response := as.numeric(as.character(Patient_Response))]





train[,P_Month := month(First_Interaction,label=TRUE)]

test[,P_Month := month(First_Interaction,label=TRUE)]



train[,R_Month := month(Registration_Date,label=TRUE)]

test[,R_Month := month(Registration_Date,label=TRUE)]





train[,c("Registration_Date","First_Interaction") := NULL]

test[,c("Registration_Date","First_Interaction") := NULL]



#removing ID variables

train <- train[,-c("Patient_ID","Health_Camp_ID"),with=FALSE]

test_ID <- test[,.(Patient_ID,Health_Camp_ID)]

test <- test[,-c("Patient_ID","Health_Camp_ID"),with=FALSE]



train$cat <- paste0(train$Category1,train$Category2,train$Category3)

test$cat <- paste0(test$Category1,test$Category2,test$Category3)





#creating new feature

train$shared <- as.factor(ifelse(train$LinkedIn_Shared+train$Twitter_Shared+train$Facebook_Shared > 0, 1,0))

test$shared <- as.factor(ifelse(test$LinkedIn_Shared+test$Twitter_Shared+test$Facebook_Shared > 0, 1,0))





train$Ages <- ifelse(train$Age<50,'Less',ifelse(train$Age<60,'Mid','High'))

test$Ages <- ifelse(test$Age<50,'Less',ifelse(test$Age<60,'Mid','High'))



train$Income[train$Income=='None'] <- 0



test$Income[test$Income=='None'] <- 0







train$Edu <- ifelse(train$Education_Score<80,'Low','High')

test$Edu <- ifelse(test$Education_Score<80,'Low','High')







table(train$Education_Score)



#numeric conversion



train$Age <- as.numeric(train$Age)

test$Age <- as.numeric(test$Age)



train$Age <- ifelse(is.na(train$Age),median(train$Age,na.rm=T),train$Age)

test$Age <- ifelse(is.na(test$Age),median(test$Age,na.rm=T),test$Age)





train$Education_Score <- as.numeric(train$Education_Score)

test$Education_Score <- as.numeric(test$Education_Score)



train$Education_Score <- ifelse(is.na(train$Education_Score),median(train$Education_Score,na.rm=T),train$Education_Score)

test$Education_Score <- ifelse(is.na(test$Education_Score),median(test$Education_Score,na.rm=T),test$Education_Score)



#trains <- cbind(train[train$Target==1,])

#zeros <- train[train$Target==0 & train$City_Type!='',]





#train <- cbind(trains,zeros)



#sampling



#train2 <- ROSE::ovun.sample(Target~.,data=train,method='over',N=30000,seed=145)$data



#table(train2$Target)





#train <- train2







#training test split





set.seed(345)







ind <- sample(1:nrow(train2),30000,replace = T)



train1 <- train2[ind,]

test1 <- train2[-ind,]





#logistic regression

model <- glm(Target ~ R_Month+P_Month+Income+Ages+City_Type+Edu+Category1+Category2+Category3+Camp_Duration+Patient_Response+shared+Var1+Var2+Var3+Var4+Var5, data=train, family = binomial(link="logit"))



#model <- glm(Target ~ Income+Education_Score+Age+City_Type+Employer_Category+Category1+Category2+Category3+Camp_Duration+Patient_Response+shared+Online_Follower, data=train, family = binomial(link="logit"))



#model$xlevels[["Camp_Month"]] <- union(log_model$xlevels[["Camp_Month"]], levels(test$Camp_Month))



#model$xlevels[["Age"]] <- union(log_model$xlevels[["Age"]], levels(test$Age))





#predict <- predict(log_model,newdata = train,type = "response")





#pred <- prediction(predict,train$Target)



#performance(pred,'auc')





auc.tmp <- performance(pred,"auc"); (auc <- as.numeric(auc.tmp@y.values))



predict <- predict(model,newdata = test,type = "response")





submission <- data.table(Patient_ID = test_ID$Patient_ID, Health_Camp_ID = test_ID$Health_Camp_ID, Outcome = predict)

write.csv(submission,"Medcamp5ovsmonths.csv",row.names = F) #Public Score - 0.78



table(is.na(predict))