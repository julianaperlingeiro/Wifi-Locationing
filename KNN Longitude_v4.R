#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 

## KNN_LONGITUDE

## DataSets ------------------------------------------------------------------------

training_wifiNOZero
names(training_wifiNOZero)

validation_wifiNOZero
names(validation_wifiNOZero)

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

listsub <- createDataPartition(y = training_wifi_WAP$LONGITUDE,
                               p = 0.3,
                               list = FALSE)


training_wifi_WAP30 <- training_wifi_WAP[listsub,] 



## Set seed -----------------------------------------------------------------------

set.seed(123)

## Creating Data Partition -------------------------------------------------------- 

inTrain <- createDataPartition(y = training_wifi_WAP30$LONGITUDE,
                               p = 0.8,
                               list = FALSE)

## Separating the data - training and testing --------------------------------------

training <- training_wifi_WAP30[inTrain,]

testing <- training_wifi_WAP30[-inTrain,]

## Double checking LATITUDE as NUMERIC -------------------------------------------

training$LONGITUDE <- as.numeric(training$LONGITUDE) 

testing$LONGITUDE <- as.numeric(testing$LONGITUDE)

## ## Cross Validation parameters ------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)



## Training KNN Model -------------------------------------------------------------------

modelKNN_LONGITUDE <- train(LONGITUDE~., 
                           data = training %>% 
                           select(starts_with("WAP"), LONGITUDE), 
                           method = "knn", 
                           tuneLength = 1,
                           trControl = CrossValidation)

## Checking the model by having the metrics -------------------------------------------

modelKNN_LONGITUDE

################## Predicting LONGITUDE With the model from Training ####################

predLONGITUDE_KNN <- predict(modelKNN_LONGITUDE, testing)

## A new column with predicted data -------------------------------------------------

testing$predLONGITUDE_KNN <- predLONGITUDE_KNN

## LONGITUDE and predLONGITUDE_KNN as NUMERIC -------------------------------------------

testing$LONGITUDE <- as.numeric (testing$LONGITUDE)

testing$predLONGITUDE_KNN <- as.numeric(testing$predLONGITUDE_KNN)

## ## Checking the metrics by Post Resample ----------------------------------------

postResample(testing$predLONGITUDE_KNN, testing$LONGITUDE)

################ Predicting VALIDATION DATA ####################################

################## Predicting LONGITUDE With the model from VALIDATION_WIFI ####################

predLONGITUDE_KNN <- predict(modelKNN_LONGITUDE, validation_wifiNOZero)

## A new column with predicted data ------------------------------------------

validation_wifiNOZero$predLONGITUDE_KNN <- predLONGITUDE_KNN

## Checking the metrics by Post Resample -------------------------------------------------

validation_wifiNOZero$LONGITUDE <- as.numeric(validation_wifiNOZero$LONGITUDE)

validation_wifiNOZero$predLONGITUDE_KNN <- as.numeric (validation_wifiNOZero$predLONGITUDE_KNN)

postResample (validation_wifiNOZero$predLONGITUDE_KNN, validation_wifiNOZero$LONGITUDE)

## Infos of Model -----------------------------------------------------------------

summary(modelKNN_LONGITUDE)




