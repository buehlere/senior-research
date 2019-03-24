library(dplyr)
library(caret)
library(rpart.plot)

setwd("~/classes/350/randomForest")
creditdata = read.csv('credit_score.csv')


summary(creditdata)
cleanedcreditdata = filter(creditdata[,2:12], X5 > 0) #remove ID tag and those with no income and missing values
summary(cleanedcreditdata)

set.seed(1) #Set seed for replicability
part = createDataPartition(cleanedcreditdata$Dlqn, p = 0.7, list=FALSE)
training = cleanedcreditdata[part,]
test = cleanedcreditdata[-part,]

#Fit a single decision tree:
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 1)

#Train decision tree using information gain criteria:
dtree_fit = train(as.factor(Dlqn) ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit

#Visualize:
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

#Create predictions on test set:
test_pred = predict(dtree_fit, newdata = test)

confusionMatrix(test_pred, test$Dlqn )  #check performance


# Build using gini criteria 
dtree_fit_gini <- train(as.factor(Dlqn) ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        metric = "Kappa",
                        tuneLength = 10)

#Visualize:
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred_gini = predict(dtree_fit_gini, newdata = test)

confusionMatrix(test_pred_gini, test$Dlqn )  #check accuracy


###########################################
#Bagged trees:

#Consider even smaller training set for class time feasibility
training_small = cleanedcreditdata[createDataPartition(cleanedcreditdata$Dlqn, p = 0.1, list=FALSE),]

#Run random forest algorithm
tic = Sys.time()
Baggedmodel = train(as.factor(Dlqn) ~., data = training, method = 'treebag',
                   metric="Kappa", 
                   trControl=trctrl)
Sys.time() - tic
Baggedmodel

test_pred_Bagged = predict(Baggedmodel, newdata = test)
confusionMatrix(test_pred_Bagged, test$Dlqn )  #check accuracy

#Examine importance of variables:
Importance = varImp(Baggedmodel)
Importance
plot(Importance)
