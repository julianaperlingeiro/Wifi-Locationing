#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 

## KNN_FLOORS 

## DataSets -------------------------------------------------------------------

training_wifiNOZero
names(training_wifiNOZero)

validation_wifiNOZero
names(training_wifiNOZero)

## Taking the dependent variable ------------------------------------------------------------
## Sampling the dataset
## Removing columns - to predict FLOOR by the values of WAPs

training_wifi_WAP <- select(training_wifiNOZero, 
                            -SPACEID, 
                            -RELATIVEPOSITION, 
                            -USERID, 
                            -PHONEID, 
                            -TIMESTAMP)


## Taking 30% of training_wifi_WAP -----------------------------------------

listsub <- createDataPartition(y = training_wifiNOZero$FLOOR,
                               p = 0.3,
                               list = FALSE)


training_wifi_WAP30 <- training_wifi_WAP[listsub,] 


## Set seed -----------------------------------------------------------------------

set.seed(123)

## Creating Data Partition -------------------------------------------------------- 

inTrain <- createDataPartition(y = training_wifi_WAP30$FLOOR,
                               p = 0.8,
                               list = FALSE)

## Separating the data - training and testing --------------------------------------

training <- training_wifi_WAP30[inTrain,]

testing <- training_wifi_WAP30[-inTrain,]

## Double checking FLOOR as factor ------------------------------------------- 

training$FLOOR <- as.factor(training$FLOOR) 

testing$FLOOR <- as.factor(testing$FLOOR)

## Cross Validation parameters ------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)


## Training KNN Model ---------------------------------------------------------------

modelKNN_FLOOR <- train(FLOOR~., 
                           data = training, 
                           method = "knn", 
                           tuneLength = 1,
                           trControl = CrossValidation)

## Checking the model by having the metrics -------------------------------------------------------------

modelKNN_FLOOR

################## Predicting FLOOR With the model from Training ####################

predFLOOR_KNN <- predict(modelKNN_FLOOR, testing)

## A new column with predicted data -------------------------------------------------

testing$predFLOOR_KNN <- predFLOOR_KNN

## FLOOR and predFLOOR_KNN as FACTORS -------------------------------------------

testing$FLOOR <- as.factor(testing$FLOOR)

testing$predFLOOR_KNN <- as.factor(testing$predFLOOR_KNN)

## Checking the metrics by CONFUSION MATRIX ----------------------------------------

confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)

################ Predicting VALIDATION DATA ####################################

################## Predicting FLOOR With the model from VALIDATION_WIFI ####################

predFLOOR_KNN <- predict(modelKNN_FLOOR, validation_wifiNOZero)

## A new column with predicted data ------------------------------------------

validation_wifiNOZero$predFLOOR_KNN <- predFLOOR_KNN

## Checking the metrics by CONFUSION MATRIX -------------------------------------------------

validation_wifiNOZero$FLOOR <- as.factor(validation_wifiNOZero$FLOOR)

validation_wifiNOZero$predFLOOR_KNN <- as.factor(validation_wifiNOZero$predFLOOR_KNN)

confusionMatrix(validation_wifiNOZero$predFLOOR_KNN, validation_wifiNOZero$FLOOR)

## Infos of Model -----------------------------------------------------------------

summary(modelKNN_FLOOR)


######################## Visualizing Errors #####################################

## Including a column with errors to the dataframe ---------------------------------------

validation_wifiNOZero$FLOOR <- as.integer(validation_wifiNOZero$FLOOR)

validation_wifiNOZero$predFLOOR_KNN <- as.integer(validation_wifiNOZero$predFLOOR_KNN)

validation_wifiNOZero  <- mutate(validation_wifiNOZero, errorsFLOOR = predFLOOR_KNN - FLOOR) 

## ## Plotting the ERRORS ---------------------------------------------------------------

plot_ly(validation_wifiNOZero, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('RED','GREEN')) %>%
  add_markers(color = ~errorsFLOOR == 0, size =1) %>%
  layout(title = "Errors predicted FLOOR's",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 




