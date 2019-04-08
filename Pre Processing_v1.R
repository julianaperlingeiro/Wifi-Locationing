#### Project: Evaluate Techniques for Wifi Locationing
#### Autor: Juliana Perlingeiro 
#### Date: 26/03/19 


## Instaling Packages --------------------------------------------------------------

##install.packages("readr")
##install.packages("caret")
##install.packages("ggplot2")
##install.packages("plotly")
##install.packages("scatterplot3d")
##install.packages("dplyr")
##install.packages("BBmisc")
##install.packages("anytime")
##install.packages("tidyr")
##install.packages("tidyverse")
##install.packages("plotly")

## Libraries -----------------------------------------------------------------------

library(readr)
library(caret)
library(lattice)
library(ggplot2)
library(scatterplot3d)
library(dplyr)
library(BBmisc)
library(anytime)
library(tidyr)
library(tidyverse)
library(plotly)


## Reading the files ----------------------------------------------------------------------

training_wifi <- read.csv(file = "trainingData.csv")

str(training_wifi)
summary(training_wifi)
ls(training_wifi)
names(training_wifi)
class("WAP510")
class("SPACEID")
class("LONGITUDE")
class("LATITUDE")
class("FLOOR")
class("BUILDINGID")
class("RELATIVEPOSITION")
class("USERID")
class("PHONEID")
class("TIMESTAMP")

validation_wifi <- read.csv(file = "validationData.csv")

str(validation_wifi)
summary(validation_wifi)
ls(validation_wifi)
names(validation_wifi)
class("SPACEID")
class("LONGITUDE")
class("LATITUDE")
class("FLOOR")
class("BUILDINGID")
class("RELATIVEPOSITION")
class("USERID")
class("PHONEID")
class("TIMESTAMP")


## Training set ----------------------------------------------------------
## floors, building ID, relative position user ID and phone ID as FACTORS


training_wifi$FLOOR <- as.factor(training_wifi$FLOOR)

training_wifi$BUILDINGID <- as.factor(training_wifi$BUILDINGID)

training_wifi$RELATIVEPOSITION <- as.factor(training_wifi$RELATIVEPOSITION)

training_wifi$USERID <- as.factor(training_wifi$USERID)

training_wifi$PHONEID <- as.factor(training_wifi$PHONEID)

training_wifi$SPACEID <-as.factor(training_wifi$SPACEID)             


## Validation set ----------------------------------------------------------------------- 
## floors and building ID as FACTORS

validation_wifi$FLOOR <- as.factor(validation_wifi$FLOOR)

validation_wifi$BUILDINGID <- as.factor(validation_wifi$BUILDINGID)


## Time variable from DATETIME ---------------------------------------------------------- 

testing_wifi$TIMESTAMP <- anytime(testing_wifi$TIMESTAMP)

validation_wifi$TIMESTAMP <- anytime(validation_wifi$TIMESTAMP)



