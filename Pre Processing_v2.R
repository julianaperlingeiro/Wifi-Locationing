#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 


## The files -----------------------------------------------------------

training_wifi <- read.csv(file = "trainingData.csv")

str((training_wifi[521:528]))
summary(training_wifi)
ls(training_wifi)
names(training_wifi)

validation_wifi <- read.csv(file = "validationData.csv")

str(training_wifi)
summary(training_wifi)
ls(training_wifi)
names(training_wifi)


## Making alterations in the values of the  WAPs ------------------------------------------

## Subsetting the WAPs -------------------------------------------------------------------- 

SubWAPS <- select(training_wifi, WAP001:WAP520)

## Substitute the value of 100 to -105 -----------------------------------------------------
## (as 100 is no signal and values)
## Signals higher than 90 = very low signal

SubWAPS <- replace(SubWAPS, SubWAPS==100, -105)

## Building a new DF by using the modified variables ---------------------------------------

selection <- select(training_wifi, 
                    LONGITUDE, 
                    LATITUDE, 
                    FLOOR, 
                    BUILDINGID, 
                    SPACEID, 
                    RELATIVEPOSITION, 
                    USERID, 
                    PHONEID,
                    TIMESTAMP)

training_wifi2 <- bind_cols(SubWAPS, selection) 



## Steps above for the Validation Set -----------------------------------------------------

SubWAPS <- select(validation_wifi, WAP001:WAP520)

SubWAPS <- replace(SubWAPS, SubWAPS==100, -105)

selection_1 <- select(validation_wifi, 
                      LONGITUDE, 
                      LATITUDE, 
                      FLOOR, 
                      BUILDINGID, 
                      SPACEID, 
                      RELATIVEPOSITION, 
                      USERID, 
                      PHONEID,
                      TIMESTAMP)

validation_wifi2 <- bind_cols(SubWAPS, selection_1)


## Converting values between 0 and -30 to no signal values (-105)
##(Values between 0 and -30 Mbps are out off the range of WiFi signals)

SubWAPS <- select(training_wifi2, WAP001:WAP520)

SubWAPS <- as.data.frame(lapply(SubWAPS, 
                                function(x){ifelse(x > -30 & x <0, -105, x+0)}))

selection <- select(training_wifi2, 
                    LONGITUDE, 
                    LATITUDE, 
                    FLOOR, 
                    BUILDINGID, 
                    SPACEID, 
                    RELATIVEPOSITION, 
                    USERID, 
                    PHONEID,
                    TIMESTAMP)

training_wifi2 <- bind_cols(SubWAPS, selection)


## Steps above for the Validation Set -----------------------------------------------------

SubWAPS <- select(validation_wifi2, WAP001:WAP520)

SubWAPS <- as.data.frame(lapply(SubWAPS, 
                                function(x){ifelse(x > -30 & x <0, -105, x+0)}))

selection_1 <- select(validation_wifi2, 
                      LONGITUDE, 
                      LATITUDE, 
                      FLOOR, 
                      BUILDINGID, 
                      SPACEID, 
                      RELATIVEPOSITION, 
                      USERID, 
                      PHONEID,
                      TIMESTAMP)

validation_wifi2 <- bind_cols(SubWAPS, selection_1) 


## Eliminating the COLUMNS with zero variance

training_wifiNOZero <- training_wifi2[, - as.numeric(which
                                                     (apply(training_wifi2,2, var) == 0))]


validation_wifiNOZero <- validation_wifi2[, - as.numeric(which
                                                          (apply
                                                          (training_wifi2,2,var) == 0))]



## Elimintaing the ROWS with zero variance in Training set

ZVRows_list <- apply(training_wifiNOZero%>% select(starts_with("WAP")),
                    1, 
                    var ) == 0

training_wifiNOZero <- training_wifiNOZero[!ZVRows_list,]


## ## Elimintaing the ROWS with zero variance in Validation set

ZVRows_list <- apply(validation_wifiNOZero %>% select(starts_with("WAP")),
                    1, 
                    var ) == 0

validation_wifiNOZero <- validation_wifiNOZero[!ZVRows_list,]


