#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 

## KNN_Building ID 

## DataSets -----------------------------------------------------------

training_wifiNOZero
names(training_wifiNOZero)

validation_wifiNOZero
names(training_wifiNOZero)


## Taking the dependent variable ------------------------------------------------------------
## Sampling the dataset
## Removing columns - to predict Building ID by the values of WAPs

training_wifi_WAP <- select(training_wifiNOZero, 
                            -SPACEID, 
                            -RELATIVEPOSITION, 
                            -USERID, 
                            -PHONEID, 
                            -TIMESTAMP)


## Taking 30% of training_wifi_WAP -----------------------------------------

listsub <- createDataPartition(y = training_wifi_WAP$BUILDINGID,
                               p = 0.3,
                               list = FALSE)


training_wifi_WAP30 <- training_wifi_WAP[listsub,]


## Set seed -----------------------------------------------------------------------

set.seed(123)

## Creating Data Partition -------------------------------------------------------- 

inTrain <- createDataPartition(y = training_wifi_WAP30$BUILDINGID,
                               p = 0.8,
                               list = FALSE)

## Separating the data - training and testing --------------------------------------

training <- training_wifi_WAP30[inTrain,]

testing <- training_wifi_WAP30[-inTrain,]

## Double checking Building ID as factor ------------------------------------------- 

training$BUILDINGID <- as.factor(training$BUILDINGID) 

testing$BUILDINGID <- as.factor(testing$BUILDINGID)

## Cross Validation parameters ------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)



## Training KNN Model ---------------------------------------------------------------

modelKNN_BUILDING <- train(BUILDINGID~., 
                           data = training, 
                           method = "knn", 
                           tuneLength = 1,
                           trControl = CrossValidation)

## Checking the model by having the metrics -------------------------------------------------------------

modelKNN_BUILDING

################## Predicting BUILDING ID With the model from Training ####################

predBULDING_KNN <- predict(modelKNN_BUILDING, testing)

## A new column with predicted data -------------------------------------------------

testing$predBULDING_KNN <- predBULDING_KNN

## BUILDIND and predBULDING_KNN as FACTORS -------------------------------------------

testing$BUILDINGID <- as.factor(testing$BUILDINGID)

testing$predBULDING_KNN <- as.factor(testing$predBULDING_KNN)

## Checking the metrics by CONFUSION MATRIX -------------------------------------------------------------

confusionMatrix(testing$predBULDING_KNN, testing$BUILDINGID)

################ Predicting VALIDATION DATA ##################################

################## Predicting BUILDING ID With the model from VALIDATION_WIFI ####################

predBUILDING_KNN <- predict(modelKNN_BUILDING, validation_wifiNOZero)

## A new column with predicted data -------------------------------------------------

validation_wifiNOZero$predBUILDING_KNN <- predBUILDING_KNN

## Checking the metrics by CONFUSION MATRIX -------------------------------------------------

validation_wifiNOZero$BUILDINGID <- as.factor(validation_wifiNOZero$BUILDINGID)

validation_wifiNOZero$predBUILDING_KNN <- as.factor(validation_wifiNOZero$predBUILDING_KNN)

confusionMatrix(validation_wifiNOZero$predBUILDING_KNN, validation_wifiNOZero$BUILDINGID)

## Infos of model -------------------------------------------------------------------------

summary(modelKNN_BUILDING)


######################## Visualizing Errors #####################################

## Including a column with errors to the dataframe ---------------------------------------

validation_wifiNOZero$BUILDINGID <- as.integer(validation_wifiNOZero$BUILDINGID)

validation_wifiNOZero$predBUILDING_KNN <- as.integer(validation_wifiNOZero$predBUILDING_KNN)

validation_wifiNOZero  <- mutate(validation_wifiNOZero, errorsBUILDING = predBUILDING_KNN - BUILDINGID) 

## Plotting the ERRORS - actually there is no ERRORS - just training the plot -------------

plot_ly(validation_wifiNOZero, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('RED','GREEN')) %>%
  add_markers(color = ~errorsBUILDING == 0, size =1) %>%
  layout(title = "Errors predicted BUILDINGID's",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 









