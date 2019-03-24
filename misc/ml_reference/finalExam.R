#--------------------------
#title: "Final Exam"
#by: "Eric Buehler"
#output: html_document
#--------------------------------------------------------------------------------------------------

#Set Up: 
####Packages and Working Directory 
library(caret)
library(dplyr)
library(ggplot2)
library(reshape2)
setwd("~/classes/350/Final Exam")

####Reading in training and Test Data 
productTrain <- read.csv("ProductSalesTraining.csv")
productTest <- read.csv("ProductSalesTest.csv")
#-------------------------------------------------------------------------------------------------

#----------------------------------
#1) Unsupervised Methods:
#----------------------------------

####Variables:
#### Sales - the continuous response. How many sales of the product are made in the first month.
#### Longevity - the binary response. Whether a product lasts on the market 12 months or longer.
#### Similar Sales (Q) - the revenue made in similar product sales.
#### Prior demand (Q) - the demand for similar products in the prior month.
#### Advertising budget (Q) - the money spent advertising the product.
#### Similar Products (Q) - the number of similar products currently on the market from your company.
#### Competitor Products (Q) - the number of similar products offered by competitors.
#### Ads (Q) - the number of ads to promote the product.
#### Competitor Ads (Q) - the number of ads run by competitors for similar products.
#### Magazine (Q) - the number of magazine articles featuring your product.
#### Awards (Q) - the number of consumer awards the product won.
#### Innovative (C) - whether the product can be considered innovative.
#### Product necessity (C) - whether or not the product can be considered a necessity.
#### Type (C) - the type of product. Levels are Bath, Kitchen, Office, Toy.
#### Cheap (C) - whether the product can be considered cheap.
#### Disposable (C) - whether the product is disposable.
#### Tested (C) - whether the product was tested with a focus group.
#### Competitor Launch(C) - whether or not competitors launched a new similar product.
#### Warranty(C) - whether the product has a warranty.
#### Styles (C) - whether the product is available in multiple styles.

#------------------------------------------------------
## Unsupervised Methods: Principal Componant Analysis
#------------------------------------------------------

####Packages: 
library(ggfortify)

#### Pulling out continuous variables. Binary variables don't make much sense for PCA. None 
#### of the categorical variables seem to have a natural ordering. 

contVars <- c("Similar_Sales","Prior_Demand","Advertising_budget","Similar_Products",
                            "Competitor_Products","Ads","Competitor_Ads","Magazine","Awards")

#### PCA 
productPCA <- prcomp(productTrain[contVars],scale.=TRUE)

#### Looking at the Principal Components: 
round(productPCA$rotation[,1:3],2) 

#### The first principal seems to be products with a high number of similar products sold, Magazines articles, 
#### ads, and advertiseing budget. Products high on this component are most likely highly competitve and 
#### popular products. Most variables show a positive correlation, but similar sales, and prior demand 
#### are lowest. 

#### The second component on the other hand seems to be products with a low number of competitor ads 
#### and low awards, but with high amount of prior demand, competitor products, advertising budget. 
#### These are most likely cheap products that many competitors make, but our company dominates  
#### through advertising. 

#### Auto-Plot with labels:
autoplot(productPCA, label = TRUE, shape = FALSE, label.size = 2) 
productTrain[c(443,950),]
#### The Auto Plot is hard to read, but I can see some outliers that might need to be handled. 
#### The products I pulled from the plot seem consistent with my analysis above. 

#### Auto-Plot with components 
autoplot(productPCA, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#### The plot helps visualize some previous findings. High number of competitor Ads, magazine articles, 
#### and similar products sold correlate with the first component. High amount of advertising budget, and 
#### prior demand correlate with the second component. 

#### What number of components capture a high amount of varience? 
pca_var = productPCA$sdev^2
prop_varex <- pca_var/sum(pca_var) 
plot(cumsum(prop_varex),type='b')

#### we can capture 99% of the varience with 7 principal components

####Throwing out outliers seen in Auto-Plot with labels: 
#### identifying: 
mean(productTrain$Competitor_Ads)
mean(productTrain$Awards)
productTrain <- productTrain  %>% 
  filter(Competitor_Ads != 273 & Awards != 26) 

#----------------------------------
## Unsupervised Methods: Clustering
#----------------------------------
#### Plot within-cluster distances as a function of cluster size. This will help determine best number 
#### of clusters

#### Looking at best number of Clusters: 
wss = list(15)
for (k in 2:15){ 
  wss[k] = sum(kmeans(productTrain[contVars], k, nstart=10)$tot.withinss)
}
plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#### Zooming in on elbow:  
wss = list(10)
for (k in 6:10){ 
  wss[k] = sum(kmeans(productTrain[contVars], k, nstart=10)$tot.withinss)
}
plot(6:10, wss[6:10], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#### 8 seems to be the best number of clusters
k.means.fit = kmeans(productTrain[contVars], 8)
k.means.fit$centers
k.means.fit$size

#### The first cluster seems to capture a group similar to those high on the first principal component. 
#### These are products with high demand, high advertising budgets, and a lot of competition from 
#### competitors in the form of competitor Ads and competitor products. 

#### The eighth cluster seems capture a group similar to those high on the second principal component.
#### These are products with a lot of competition in the form of competitor products and ads, but 
#### our company seems to dominate in terms of advertising budget. 

#### Most of the other clusters seem to fall inbetween these two major groups. Cluster 3
#### seems a little unique it is similar to cluster 8 but it seems that competitors are running
#### more adds. 

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


#----------------------------------------
#2) Supervised Methods For Regression 
#----------------------------------------
#### Setting up Regression Training set by removing Longevity and Type: 
regressTrain <- productTrain[,!(names(productTrain) %in% "Longevity")] 
regressTrain <- regressTrain[,!(names(regressTrain) %in% "Type")] 
productTest <- productTest[,!(names(productTest) %in% "Type")] 

#### Transforming Type Variable into dummies, THIS DOESN'T HELP 
#### for Train 
#### type <- data.frame(model.matrix( ~ Type - 1, data=regressTrain))
#### regressTrain<- bind_cols(regressTrain,type)
#### for Test 
### type <- data.frame(model.matrix( ~ Type - 1, data=productTest))
### productTest <- bind_cols(productTest,type)

#---------------------------------------------------------------------
## Supervised Methods For Regression: Linear Regression 
#---------------------------------------------------------------------
#### with caret
trctrl = trainControl(method="cv",number = 10,repeats = 3)
lm_model = train(Sales~.,regressTrain, trControl=trctrl, method="lm")
lm_model$results

#### 26084 RMSE 

#### checking variables significance
lmvar = lm(Sales~.,regressTrain)
summary(lmvar)

#### Looking at the variables we can see similar sales, advertising budget, similar products, and 
#### innovation are all positvely significant at a confidence of 99%. On the other hand, competitor ads 
#### and number of magazines are negativly significant. This gives us more knowledge about how variables 
#### seen in principal component analysis like advertising budget, and number of magazines related to 
#### our variable of interest, sales. This also gives us an idea of a possible subset that we might 
#### want to try training our models on. 

#### pulling out significant variables
smallregressTrain <- regressTrain[,c("Sales","Similar_Sales","Advertising_budget","Similar_Products",
                                     "Ads","Competitor_Ads","Magazine","Awards","Innovative",
                                     "Product_necessity")]
trctrl = trainControl(method="cv",number = 10,repeats = 3)
lm_model2 = train(Sales~.,smallregressTrain, trControl=trctrl, method="lm")
lm_model2$results

#### doesn't help. 

#### PCA 
trctrl = trainControl(method="cv",number = 10,repeats = 3)
lm_model3 = train(Sales~.,regressTrain, trControl=trctrl, method="lm",preProcOptions = list(pcaComp = 8))
lm_model3$results


linearPredictions = predict(lm_model3, newdata = productTest)

#### SUMMARY:
#### Predictions made using 3rd model, which was trained on the principal compenents. 
#### This results in a RMSE of 25789.41. The predictions look strange though with negative values. 
#### definitly need to try different methods. 
#---------------------------------------------------------------------
## Supervised Methods For Regression: K-Nearest Neighbors Regression 
#---------------------------------------------------------------------

#### Let caret search for best number of neighbors: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(1:20))
knn_try1 = train(Sales~., data = regressTrain, method = "knn",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneGrid = tunegrid)

plot(knn_try1)
#### seem to get diminishing returns for RMSE after 10

#### Looking closer at best k: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(10:20))
knn_try2 = train(Sales~., data = regressTrain, method = "knn",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid = tunegrid)

plot(knn_try2)

#### RMSE fluctuates a lot after 10, but definitly increase after 16 

#### one last look at a closer range: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(10:16))
knn_try3 = train(Sales~., data = regressTrain, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = tunegrid)

plot(knn_try3)
knn_try3$results$RMSE
#### I've run this a few times, and gotten some varying results. All things said a k after 10 almost 
#### always does better, but a k too close to 16 might perform worse. A k of 13 seems like a reasonable choice.
#### It results in an RMSE in the high 24000.

#### trying with smaller subset: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(13))
knn_try4 = train(Sales~., data = smallregressTrain, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = 13)

plot(knn_try4)
knn_try4$results$RMSE
#### smaller subset doesn't seem to help. 

#### trying PCA: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3,preProcOptions = list(pcaComp = 8))
knn_try5 = train(Sales~., data = regressTrain, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 13)

plot(knn_try5)
knn_try5$results$RMSE

#### This results in a RMSE in the mid 24000, seems like the best RMSE so far. 

#### Test best fit KNN on test data
knnPredictions = predict(knn_try5, newdata = productTest)

#### SUMMARY:
#### Predictions made using 5th model with a k of 13, and trained on the principal compenents. 
#### This results in a RMSE in the mid 24000, the best so far. The predictions look reasonable. 
#### This looks to be a promising choice. 

#---------------------------------------------------------------------
## Supervised Methods For Regression: Random Forest Regression 
#---------------------------------------------------------------------
#### helpful packages for Random Forest: 
library(rpart)
library(rpart.plot)

#### Comparing Information and GINI: 

#### using information criteria:
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit_information = train(Sales~., 
                              data = regressTrain, method = "rpart", 
                              parms = list(split = "information"), 
                              trControl=trctrl, tuneLength = 10)

#### using gini criteria:
dtree_fit_gini = train(Sales~., 
                       data = regressTrain, method = "rpart", 
                       parms = list(split = "gini"), 
                       trControl=trctrl, tuneLength = 10)

#### results: 
dtree_fit_information$results
dtree_fit_gini$results

#### Information seems to work better 

#### visualizing tree: 
prp(dtree_fit_information$finalModel, 
    box.palette = "Blues", 
    tweak = 1.2)

#### Advertising, Magazine, Similar Product, Prior Demand seem to be some of the more important variables.
#### Some familiar variables from the PCA, and the linear regression model.  


#### Trying different number of grides and trees: 

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(1:10))
dtree_fit_information = train(Sales~., 
                              data = regressTrain, method = "rf", 
                              parms = list(split = "information"), 
                              trControl=trctrl, tuneGrid = tunegrid,ntrees = 20000)

plot(dtree_fit_information)
dtree_fit_information$results

#### Trying more trees, but gride size doesn't seem to change much: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(1))
dtree_fit_information2 = train(Sales~., 
                               data = regressTrain, method = "rf", 
                               parms = list(split = "information"), 
                               trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)

dtree_fit_information2$results

#### trying PCA set: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3,preProcOptions = list(pcaComp = 8))
tunegrid = expand.grid(mtry=c(1))
dtree_fit_information3 = train(Sales~., 
                              data = regressTrain, method = "rf", 
                              parms = list(split = "information"), 
                              trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)


dtree_fit_information3$results
#### One of the best models so far. RMSE of 25913.5 

#### trying small training set: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(2))
dtree_fit_information4 = train(Sales~., 
                               data = smallregressTrain, method = "rf", 
                               parms = list(split = "information"), 
                               trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)

dtree_fit_information4$results
#### doesn't seem to make a difference 

#### Prediction with best model: 
rfPredictions = predict(dtree_fit_information3, newdata = productTest)

#### SUMMARY: 
#### Predictions made using 2nd model with a high number of trees, and tuning paramet of 1 on 
#### the PCA set. This results in a RMSE of 25913.5. The predictions look reasonable. 
#### I tried to get the random forest model to beat knn, but haven't had any luck so far. 

#---------------------------------------------------------------------
## Supervised Methods For Regression: Neural Network Regression  
#---------------------------------------------------------------------
trctrl =trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid=expand.grid(size=c(1:15), decay=c(.01,.03,.05))

Network_try1 <- train(Sales~.,
                 regressTrain,
                 method = "nnet",
                 linout=TRUE, #Defines as a regression problem, NOT classification
                 trControl= trctrl,
                 preProcess=c("scale","center"),
                 tuneGrid = tunegrid,
                 na.action = na.omit,
                 skip=TRUE, 
                 verbose = FALSE,
                 maxit = 500
)
plot(Network_try1)
Network_try1$results
#### second try 
trctrl =trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid=expand.grid(size=c(9), decay=c(.01))

Network_try2 <- train(Sales~.,
                      regressTrain,
                      method = "nnet",
                      linout=TRUE, #Defines as a regression problem, NOT classification
                      trControl= trctrl,
                      preProcess=c("scale","center"),
                      tuneGrid = tunegrid,
                      na.action = na.omit,
                      skip=TRUE, 
                      verbose = FALSE,
                      maxit = 500
)
plot(Network_try2)
Network_try2$results


networkPredictions = predict(Network_try2, newdata = productTest)

#### SUMMARY: 
#### Having some trouble with these packages. The reported RMSE seems competitve with other methods 
#### around (2500), but the predictions look very strange. 
#### After tweaking decay and the tuning parameter a few times, I don't see the neural networks 
#### out-doing knn, or random forest.

#------------------------------
##Conclusion of Investigation: 
#------------------------------
#### K-Nearest neighbor gives the lowest RMSE with the most reasonable looking predictions. 
#### Random Forest comes in second place with a low RMSE and reasonable looking predictions. 
#### Linear regression and Neural Networks come up with competitive RMSEs, but the predictions look 
#### very strange even after trying multiple different subsets of data and tweaking model parameters.

#-------------------------------------------------------------------------------
## Supervised Methods For Regression: Estimating Models Performance 
#-------------------------------------------------------------------------------
#### Estimating models Performance: 

dummyregTest <- regressTrain[c(1:788),]
dummyregTrain <- regressTrain[c(789:1576),]

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3,preProcOptions = list(pcaComp = 8))
knn_final = train(Sales~., data = dummyregTrain, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 13)

knn_final$results$RMSE
kn_predict <- predict(knn_try5,dummyregTest)

sqrt(mean((kn_predict - dummyregTest$Sales)^2) / 788)

#-----------------------------------------------------------
## Supervised Methods For Regression: Final Predictions
#-----------------------------------------------------------

Regpredictions <- predict(knn_try5,productTest)

write.csv(Regpredictions,"Regression Predictions.csv")


#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

#----------------------------------------
#2) Supervised Methods For Classification 
#----------------------------------------
#### Setting up Classification Training set by removing Sales and Type: 
classTrain <- productTrain[,!(names(productTrain) %in% "Sales")] 
classTrain <- classTrain[,!(names(classTrain) %in% "Type")] 

dummyTest <- classTrain[c(1:788),]
dummyTrain <- classTrain[c(789:1576),]

library(ROSE)
table(classTrain$Longevity)
levels(classTrain$Longevity)[levels(classTrain$Longevity)==0] = "Valid" 
levels(classTrain$Longevity)[levels(classTrain$Longevity)==1] = "Fraud" 

set.seed(9560)
roseTrain <- ROSE(Longevity ~ ., data = classTrain)$data 
table(roseTrain$Longevity)

#---------------------------------------------------------------------
## Supervised Methods For Classification: Logistic Regression 
#---------------------------------------------------------------------
#### with caret
trctrl = trainControl(method="cv",number = 10)
log_model = train(as.factor(Longevity)~., data = classTrain,method = "glm", 
                  family = binomial ,trControl = trctrl)
log_model$results

#### 91% Accuracy

#### taking a look at significant variables to determine a good subset: 
logvar <- glm(as.factor(Longevity)~., data = classTrain,family = binomial) 
summary(logvar)

#### Making a smaller subset based on some variables signficance: 

smallclassTrain <- classTrain[,c("Longevity","Similar_Sales","Advertising_budget","Competitor_Products",
                                     "Ads","Competitor_Ads","Magazine","Awards","Innovative",
                                     "Product_necessity","Warranty","Competitor_Launch","Styles")]

#### Many of the significant variables are similar to what we saw in the linear model. However, some 
#### variables such as competitor launch, style, and product necessity seem to play a bigger role in 
#### predicting Longevity. This intuitively makes sense as we would expect competition, and variables 
#### tied to ingenuity to play a bigger part in a products longevity. 


#### with smaller training set: 
trctrl = trainControl(method="cv",number = 10)
log_model2 = train(as.factor(Longevity)~., data = smallclassTrain,method = "glm", 
                  family = binomial ,trControl = trctrl)
log_model2$results

#### Stays roughly the same, but slightly better.

#### with PCA
trctrl = trainControl(method="cv",number = 10,preProcOptions = list(pcaComp = 8))
log_model3 = train(as.factor(Longevity)~., data = classTrain,method = "glm", 
                  family = binomial ,trControl = trctrl)
log_model3$results

#### with Rose
trctrl = trainControl(method="cv",number = 10)
log_model4 = train(as.factor(Longevity)~., data = roseTrain,method = "glm", 
                   family = binomial ,trControl = trctrl)
log_model4$results

#### 84% accuracy, but worth considering later. 

#### SUMMARY: 
#### Both logistic models perform with an accuracy around 91%. However, 
#### the first model with the smaller subset performs slightly better by close to a percentage point
#### I plan to look at some other models first before investigating the best cut-off the models. 

#---------------------------------------------------------------------------
## Supervised Methods For Classification: K-Nearest Neighbors Classification  
#---------------------------------------------------------------------------
#### trying a large number of k: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(1:10))
knn_classtry1 = train(as.factor(Longevity)~., data = classTrain, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = tunegrid)

plot(knn_classtry1)
knn_classtry1$results

#### 1 seems to be the best number of neighbors by a significant amount. No need to try more ks.   

#### trying with the smaller subset: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(1))
knn_classtry2 = train(as.factor(Longevity)~., data = smallclassTrain, method = "knn",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = tunegrid)

knn_classtry2$results

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3,preProcOptions = list(pcaComp = 8))
tunegrid = expand.grid(k = c(1))
knn_classtr3 = train(as.factor(Longevity)~., data = classTrain, method = "knn",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = tunegrid)

knn_classtry3$results

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(k = c(1))
knn_classrose = train(as.factor(Longevity)~., data = roseTrain, method = "knn",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneGrid = tunegrid)

knn_classrose$results

#### The smaller subsets seems to make the model worse. 

#### SUMMARY: 
#### K-Nearest Neighbor perfromed best with 1 neighbors, and the regular training set. The accuracy of the 
#### model is 98%, which is more accurate than the logistic model 

#---------------------------------------------------------------------------
## Supervised Methods For Classification: Random Forest Classification  
#---------------------------------------------------------------------------
#### Comparing Information and GINI: 

#### using information criteria:
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_class_information = train(as.factor(Longevity)~., 
                              data = classTrain, method = "rpart", 
                              parms = list(split = "information"), 
                              trControl=trctrl, tuneLength = 10)

#### using gini criteria:
dtree_class_gini = train(as.factor(Longevity)~., 
                       data = classTrain, method = "rpart", 
                       parms = list(split = "gini"), 
                       trControl=trctrl, tuneLength = 10)

dtree_class_information$results
dtree_class_gini$results


#### Fairly similar performance, but information looks slightly better. 

#### Looking at variable performance: 
prp(dtree_class_gini$finalModel, 
    box.palette = "Blues", 
    tweak = 1.2)

#### Looking at the decision tree visual similar products,and competitor product seem to be important 
#### decision variables. This further demonstrates how predicting a products logevity is a different 
#### problem from sales. Factors concerning market saturation play a larger role in a products longevity.

#### tweaking tune grid parameter, already starting with large number of trees. 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(1:10))
dtree_class_information2 = train(as.factor(Longevity)~., 
                               data = classTrain, method = "rf", 
                               parms = list(split = "information"), 
                               trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)
plot(dtree_class_information2)
dtree_class_information2$results

#### seems to plateau at two 

#### slightly closer look, and even more trees: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(2,5,8,10:15))
dtree_class_information3 = train(as.factor(Longevity)~., 
                          data = classTrain, method = "rf", 
                          parms = list(split = "information"), 
                          trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)
plot(dtree_class_information3)
dtree_class_information3$results

#### After running a few times, 12 seems to be good tuning parameter



#### final, increasing trees 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(12))
dtree_class_information4 = train(as.factor(Longevity)~., 
                    data = classTrain, method = "rf", 
                    parms = list(split = "gini"), 
                    trControl=trctrl, tuneGrid = tunegrid,ntrees = 20000)

dtree_class_information4$results
dtree_class_information4$results

#### the increased number of trees doesn't seem to help much, but it can't hurt. 

#### rose train 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(1))
dtree_rose = train(as.factor(Longevity)~., 
                                 data = roseTrain, method = "rf", 
                                 parms = list(split = "information"), 
                                 trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)

dtree_rose$results

#### 90% accuracy for the rose trained model with an mtry set to 1, and 20,000 trees. 

#### SUMMARY: 
#### Random forest with a tuning parameter of twelve and number of trees 20000 improves upon the previous 
#### two models with an accuracy of 99%! 90% accuracy for rose trained. 

#------------------------------------------------------------------------
## Supervised Methods For Classification: Neural Network Classification 
#------------------------------------------------------------------------


#### first try: 
control = trainControl(method="repeatedcv",number = 10, repeats = 3)
tunegrid = expand.grid(layer1=c(1:5),layer2=c(1:5),layer3=c(1:5),hidden_dropout=0,visible_dropout=0)


networkclass_try1 = train(as.factor(Longevity)~., 
                    data = classTrain,
                    method = "dnn",
                    trControl= control,
                    preProcess=c("scale","center"),
                    tuneGrid = tunegrid,
                    na.action = na.omit) 
plot(networkclass_try1)
networkclass_try1$bestTune
networkclass_try1$results

#### same Accuracy for all layers, weird. 
#### trying a smaller network: 

#### trying with balanced classes: 
control = trainControl(method="repeatedcv",number = 10, repeats = 3)
tunegrid = expand.grid(layer1=c(1:5),layer2=c(1:3),layer3=(1:3),hidden_dropout=0,visible_dropout=0)
networkclass_try2 = train(as.factor(Longevity)~., 
                          data = roseTrain,
                          method = "dnn",
                          trControl= control,
                          preProcess=c("scale","center"),
                          tuneGrid = tunegrid,
                          na.action = na.omit) 
plot(networkclass_try2)
networkclass_try2$bestTune
networkclass_try2$results

#### seems to have fixed issue 

#### one more try, focuing on layer 1: 
control = trainControl(method="repeatedcv",number = 10, repeats = 3)
tunegrid = expand.grid(layer1=c(1:15),layer2=c(0),layer3=(0),hidden_dropout=0,visible_dropout=0)
networkclass_try3 = train(as.factor(Longevity)~., 
                          data = roseTrain,
                          method = "dnn",
                          trControl= control,
                          preProcess=c("scale","center"),
                          tuneGrid = tunegrid,
                          na.action = na.omit) 
plot(networkclass_try2)
networkclass_try3$bestTune
networkclass_try3$results

#### After a few runs, 11 nodes seems best.

#### SUMMARY: 
#### After a few runs, 11 nodes seems best, but it still only results in an Accuracy of 82% considerably 
#### lower than the other models.

#------------------------------------------------------------------------
## Supervised Methods For Classification: Linear Discriminant Analysis  
#------------------------------------------------------------------------

control = trainControl(method="repeatedcv",number = 10, repeats = 3)
lda_model1 = train(as.factor(Longevity)~., 
                   data=classTrain, 
                   method="lda", 
                   trControl = control)
lda_model1$results

#### Accuracy of 91% very comparable to the logistic model, still not better than random forest. 

#### trying smaller subset: 
control = trainControl(method="repeatedcv",number = 10, repeats = 3)
lda_model2 = train(as.factor(Longevity)~., 
                   data=smallclassTrain, 
                   method="lda", 
                   trControl = control)
lda_model2$results

#### smaller subset drops accuracy to 90%  

#### trying balanced data: 
control = trainControl(method="repeatedcv",number = 10, repeats = 3)
nonlda_model = train(as.factor(Longevity)~., 
                     data=roseTrain, 
                     method="lda", 
                     trControl = control)
nonlda_model$results

#### results in an accuracy of 93%.

#### SUMMARY: 
#### lda provides an Accuracy of 91%, comparable to the Accuracy of the logistic model. Still no where 
#### close to random forest's Accuracy. 

#------------------------------
##Conclusion of Investigation: 
#------------------------------

#### Random Forest gives the highest Accuracy at 98%. 
#### I plan on investigating a few additional considerations such as log loss, and best cut off 
#### when I do my estimation of the model's performance. LDA and logistic regression also prove to be 
#### solid models with Accuracies of 91%, but not as good as random forest. Neural Netorks once again 
#### prove to be somewhat troublesome, however after using a balanced data set I was able to improve 
#### the accuracy to 83%. In the code above I tried a varierty of data sets on the models. For example, 
#### I tried the Rose subset on nearly all the models. I'm torn on, which to use I think estimating 
#### my models performance will help with this. 

#-------------------------------------------------------------------------------
## Supervised Methods For Classification: Estimating Models Performance 
#-------------------------------------------------------------------------------

#### Looking at unbalanced performance: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(12))
class_final = train(as.factor(Longevity)~., 
                    data = dummyTrain, method = "rf", 
                    parms = list(split = "gini"), 
                    trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)

class_final$results

#### cut offs and performance 
dfclass_Prob = data.frame(predict(class_final,dummyTest, type="prob")[,2])
table(dfclass_Prob > 0.328, dummyTest$Longevity)
dfclass_Predict = predict(class_final,dummyTest)
LogLoss(as.numeric(as.character(dfclass_Predict)),as.numeric(as.character(dummyTest$Longevity)))

dfclass_Prob = data.frame(predict(class_final,dummyTrain, type="prob")[,2])
summary(dfclass_Prob)
performance = setNames(data.frame(matrix(ncol = 8, nrow = 101)), 
                       c("Cutoff","TN", "FN", "TP", "FP", "Sensitivity", "Specificity","Accuracy"))
performance$Cutoff = seq(0.3,0.4,.001) 
summary(dfclass_Prob)
for (i in 1:101){
  temp = table( dfclass_Prob > performance$Cutoff[i], dummyTrain$Longevity) 
  TN = temp[1,1]
  FN = temp[1,2]
  FP = temp[2,1]
  TP = temp[2,2]
  performance$TN[i] = TN
  performance$TP[i] = TP
  performance$FN[i] = FN
  performance$FP[i] = FP
  performance$Sensitivity[i] = TP/(FN+TP)
  performance$Specificity[i] = TN/(TN+FP)
  performance$Accuracy[i] = (TP+TN)/(FP+FN+TP+TN)
}

#### best cut off seems .328

#### looking at balanced performance: 

#### Training on half of training data using Rose: 
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunegrid = expand.grid(mtry=c(1))
dtree_class_informationrose = train(as.factor(Longevity)~., 
                                 data = roseTrain[c(1:300,770:1070),], method = "rf", 
                                 parms = list(split = "information"), 
                                 trControl=trctrl, tuneGrid = tunegrid,ntrees = 10000)

dtree_class_informationrose$results

#### testing on other half: 
dfclass_Prob = data.frame(predict(dtree_class_informationrose,classTrain[c(301:769,1071:1576),], 
                                  type="prob")[,2])
kable(table(dfclass_Prob > 0.579, classTrain[c(301:769,1071:1576),]$Longevity))
dfclass_Predict = predict(dtree_class_informationrose,classTrain[c(301:769,1071:1576),])
LogLoss(as.numeric(as.character(dfclass_Predict)),
        as.numeric(as.character(classTrain[c(301:769,1071:1576),]$Longevity)))

#### Pretty okay. 
dfclass_Prob = data.frame(predict(dtree_class_informationrose,classTrain[c(301:769,1071:1576),], 
                                  type="prob")[,2])
summary(dfclass_Prob)
performance = setNames(data.frame(matrix(ncol = 8, nrow = 101)), 
                       c("Cutoff","TN", "FN", "TP", "FP", "Sensitivity", "Specificity","Accuracy"))
performance$Cutoff = seq(0.5,0.6,.001) 
summary(dfclass_Prob)
for (i in 1:101){
  temp = table( dfclass_Prob > performance$Cutoff[i], classTrain[c(301:769,1071:1576),]$Longevity) 
  TN = temp[1,1]
  FN = temp[1,2]
  FP = temp[2,1]
  TP = temp[2,2]
  performance$TN[i] = TN
  performance$TP[i] = TP
  performance$FN[i] = FN
  performance$FP[i] = FP
  performance$Sensitivity[i] = TP/(FN+TP)
  performance$Specificity[i] = TN/(TN+FP)
  performance$Accuracy[i] = (TP+TN)/(FP+FN+TP+TN)
}
#### 0.579 seems like a decent cut off. 



#-----------------------------------------------------------
## Supervised Methods For Classification: Final Predictions
#-----------------------------------------------------------
#### Classification predicitons: 
dfclass_Prob = data.frame(predict(dtree_class_information4,productTest, type="prob")[,2])
dfclass_Predict = predict(dtree_class_information4,productTest)

Classificationpredictions = setNames(cbind(dfclass_Prob,dfclass_Predict),
                                      c("Class Predictions","Probability Predictions"))

write.csv(Classificationpredictions,"Classification predictions.csv")
