
library(xgboost)
library(rpart)
library(Ckmeans.1d.dp)
# Creating a dataframe containing only columns that interest us and converting the data into numerics in order to use xgboost
combi2 <- train[c("Patient_ID","Health_Camp_ID",
                  "Var1","Var2","Var3","Var4","Var5",
                  "year","month","dayofyear","dayofweek","day","week")]
# Converting factors to numerics and making the variables start at 0 since this is a requirement of the xgboost package
train$Var1 <- as.numeric(train$Var1)
train$Patient_ID <- as.numeric(train$Patient_ID)
train$Health_Camp_ID <- as.numeric(train$Health_Camp_ID)
# convert the new dataframe into a matrix 
trainf<-train
testf<-test
train <- as.matrix(train)
test <- as.matrix(test)

# Splitting back to train and test sets
train <- combi2[1:891,]
test <- combi2[892:1309,]

# Using the cross validation to estimate our error rate:
param <- list("objective" = "binary:logistic")

cv.nround <- 15
cv.nfold <- 3

xgboost_cv = xgb.cv(param=param, data = train[, -c(1)], label = train[, c(1)], nfold = cv.nfold, nrounds = cv.nround, missing=NaN)
# [0]        train-error:0.130752+0.007009	test-error:0.178451+0.043771
# [1]	train-error:0.124018+0.005412	test-error:0.179573+0.032702
# [2]	train-error:0.120090+0.005412	test-error:0.178451+0.032119
# [3]	train-error:0.112795+0.001684	test-error:0.178451+0.032119
# [4]	train-error:0.112795+0.003367	test-error:0.177329+0.035367

# Fitting with the xgboost model
nround  = 15
fit_xgboost <- xgboost(param =param, data = train[, -c(1)], 
                       label = train[, c(1)], nrounds=nround, missing = NaN)

# Get the feature real names
names <- dimnames(train)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = fit_xgboost)

# Plotting
xgb.plot.importance(importance_matrix)

# Prediction on test and train sets
pred_xgboost_test <- predict(fit_xgboost, test)
pred_xgboost_train <- predict(fit_xgboost, train[, -c(2)], missing= NaN)

# Since xgboost gives a survival probability prediction, we need to find the best cut-off:
proportion <- sapply(seq(.3,.7,.01),function(step) 
  c(step,sum(ifelse(pred_xgboost_train<step,0,1)!=train[, c(2)])))
dim(proportion)
# Applying the best cut-off on the train set prediction for score checking
predict_xgboost_train <- ifelse(pred_xgboost_train<proportion[,which.min(proportion[2,])][1],0,1)
head(predict_xgboost_train)
score <- sum(train[, c(1)] == predict_xgboost_train)/nrow(train)
score

# Applying the best cut-off on the test set
predict_xgboost_test <- ifelse(pred_xgboost_test<proportion[,which.min(proportion[2,])][1],0,1)
test <- as.data.frame(test) # Conveting the matrix into a dataframe

# Creating the submitting file
submit <- data.frame(Patient_ID = test_ID$Patient_ID,Health_Camp_ID = test_ID$Health_Camp_ID, 
                     Outcome = pred_xgboost_test)
write.csv(submit, file = "firstxgboost.csv", row.names = FALSE)

