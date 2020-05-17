#Data Source :https://www.wyborkierowcow.pl/moc-w-przeliczeniu-na-zlotowki-zestawienie/
#MED Project
# dependent var(Y)= price
# independent Var(X)= speed, fueltype, fuelscore, enginecapacity
library(markdown)
library(dplyr)
library (ggplot2)
library(tidyverse)
library(psych)
library(GPArotation)
library(knitr)
install.packages("Metrics")
library(caret)

#data Preparation
library(readxl)
carPrice <- read_excel("C:\\Users\\khalida\\Documents\\Business Statistic\\Document\\Car Price\\Data\\carPrice.xlsx", 
                       n_max = 111)
##View(carPrice)
##carPrice
str(carPrice)
head(carPrice)
glimpse(carPrice)

nrow(carPrice)

##Checking categorical values
fuelTypeOnly <- carPrice %>% select(FuelType)
fueltypetrial <- as.character(unique(fuelTypeOnly))
fueltypetrial

##function changing fueltype from categorical into numerical
fueltypeFunc <- function(x, fueltypetrial){
  factor(x, levels = fueltypetrial, labels = fueltypetrial, ordered = TRUE)
}
#converting fueltype into numerical var
trial <- map_df(fuelTypeOnly, fueltypeFunc) %>% glimpse
trial <- map_df(trial, as.numeric)%>% glimpse
carPrice[6]<- trial
glimpse(carPrice)

##removing column
carPriceClean <-carPrice[c(2,4,5,6,7)]
carPriceClean
glimpse(carPriceClean)

##Linear Model
linmodel <- lm(carPriceClean$Price~carPriceClean$EngineCapacity+carPriceClean$Speed+carPriceClean$FuelType+carPriceClean$FuelScore,data = carPriceClean)
summary(linmodel)                  
linmodel
plot(linmodel)
abline(linmodel)

#linearequation y= mx+b
predictedPriceVolkswagenGolfGTI <--58887.488+ (1984*-2.526)+(245 *481.112)+(522*129.323)
predictedPriceVolkswagenGolfGTI


##########################################################
#VAlidation set approach
set.seed(123)
training.samples <- carPriceClean$Price %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- carPriceClean[training.samples, ]
test.data <- carPriceClean[-training.samples, ]
test.data


# Build the model
model <- lm(Price ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
#Predicted Value
data.frame(test.data, predictions)
data.frame( R2 = R2(predictions, test.data$Price),
            RMSE = RMSE(predictions, test.data$Price),
            MAE = MAE(predictions, test.data$Price))
print(test.data$Price)
#prediction error rate
RMSE(predictions, test.data$Price)/mean(test.data$Price)
#sum
summary(model)
model.
#######################################################################

#Leave one out cross validation - LOOCV
# Define training control
  train.control <- trainControl(method = "LOOCV")
# Train the model
modelLOOCV <- train(Price ~., data = carPriceClean, method = "lm",
               trControl = train.control)

# Summarize the results
print(modelLOOCV)
modelLOOCV$pred

#######################################################################

#K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 3)
# Train the model
model <- train(Price ~., data = carPriceClean, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#######################################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 3, repeats = 3)
# Train the model
model <- train(Price ~., data = carPriceClean, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

######################################################################



#Cross Validation K= 3


#mse
mse()


##plot(carPrice$Price~carPrice$EngineCapacity,data= carPrice)

