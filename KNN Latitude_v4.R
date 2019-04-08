#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 

## KNN_LATITUDE 

## DataSets ------------------------------------------------------------------------------

training_wifiNOZero
names(training_wifiNOZero)

validation_wifiNOZero
names(training_wifiNOZero)

## Taking the dependent variable ------------------------------------------------------------
## Sampling the dataset
## Removing columns - to predict LATITUDE by the values of WAPs

training_wifi_WAP <- select(training_wifiNOZero, 
                            -SPACEID, 
                            -RELATIVEPOSITION, 
                            -USERID, 
                            -PHONEID, 
                            -TIMESTAMP)



## Taking 30% of training_wifi_WAP -----------------------------------------

listsub <- createDataPartition(y = training_wifi_WAP$LATITUDE,
                               p = 0.3,
                               list = FALSE)


training_wifi_WAP30 <- training_wifi_WAP[listsub,] 



## Set seed -----------------------------------------------------------------------

set.seed(123)

## Creating Data Partition -------------------------------------------------------- 

inTrain <- createDataPartition(y = training_wifi_WAP30$LATITUDE,
                               p = 0.8,
                               list = FALSE)

## Separating the data - training and testing --------------------------------------

training <- training_wifi_WAP30[inTrain,]

testing <- training_wifi_WAP30[-inTrain,]

## Double checking LATITUDE as NUMERIC -------------------------------------------  

training$LATITUDE <- as.numeric(training$LATITUDE) 

testing$LATITUDE <- as.numeric(testing$LATITUDE)

## Cross Validation parameters ------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)



## Training KNN Model ---------------------------------------------------------------

modelKNN_LATITUDE <- train(LATITUDE~., 
                        data = training %>% 
                          select(starts_with("WAP"), LATITUDE), 
                        method = "knn", 
                        tuneLength = 1,
                        trControl = CrossValidation)

## Checking the model by having the metrics -------------------------------------------

modelKNN_LATITUDE

################## Predicting LATITUDE With the model from Training ####################

predLATITUDE_KNN <- predict(modelKNN_LATITUDE, testing)

## A new column with predicted data -------------------------------------------------

testing$predLATITUDE_KNN <- predLATITUDE_KNN

## LATITUDE and predLATITUDE_KNN as NUMERIC -------------------------------------------

testing$LATITUDE <- as.numeric (testing$LATITUDE)

testing$predLATITUDE_KNN <- as.numeric(testing$predLATITUDE_KNN)

## Checking the metrics by Post Resample ----------------------------------------

postResample(testing$predLATITUDE_KNN, testing$LATITUDE)

################ Predicting VALIDATION DATA ####################################

################## Predicting LATITUDE With the model from VALIDATION_WIFI ####################

predLATITUDE_KNN <- predict(modelKNN_LATITUDE, validation_wifiNOZero)

## A new column with predicted data ------------------------------------------

validation_wifiNOZero$predLATITUDE_KNN <- predLATITUDE_KNN

## Checking the metrics by Post Resample -------------------------------------------------

validation_wifiNOZero$LATITUDE <- as.numeric(validation_wifiNOZero$LATITUDE)

validation_wifiNOZero$predLATITUDE_KNN <- as.numeric (validation_wifiNOZero$predLATITUDE_KNN)

postResample(validation_wifiNOZero$predLATITUDE_KNN, validation_wifiNOZero$LATITUDE)

## Infos of Model -----------------------------------------------------------------

summary(modelKNN_LATITUDE)






