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
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
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

carPriceWithSignificantVar<- carPriceClean[c(1,2,3,5)]


##########################################################
#VAlidation set approach
set.seed(123)
training.samples <- carPriceWithSignificantVar$Price %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- carPriceWithSignificantVar[training.samples, ]
test.data <- carPriceWithSignificantVar[-training.samples, ]
test.data


# Build the model
model1 <- lm(Price ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model1 %>% predict(test.data)
#Predicted Value
data.frame(test.data, predictions)
data.frame( R2 = R2(predictions, test.data$Price),
            RMSE = RMSE(predictions, test.data$Price),
            MAE = MAE(predictions, test.data$Price))
print(test.data$Price)
#prediction error rate
RMSE(predictions, test.data$Price)/mean(test.data$Price)
MAE(predictions, test.data$Price)
#sum
summary(model)
model1

#######################################################################

#Leave one out cross validation - LOOCV
# Define training control
  train.control <- trainControl(method = "LOOCV")
# Train the model
modelLOOCV <- train(Price ~., data = carPriceWithSignificantVar, method = "lm",
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
model3 <- train(Price ~., data = carPriceWithSignificantVar, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#######################################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 3, repeats = 10)
# Train the model
model4 <- train(Price ~., data = carPriceWithSignificantVar, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

######################################################################
##Multiple Regression PREDICTION 
##using validation set approach
##New obj Volkswagen Golf GTI 1984 230 Benzyna 117000 509
##Czytaj wiecej: https://www.wyborkierowcow.pl/moc-w-przeliczeniu-na-zlotowki-zestawienie/

predict(model1, data.frame("EngineCapacity"=1984, "Speed"=230, "FuelType"=1, "FuelScore"=509),interval= "prediction")

predict(modelLOOCV, data.frame("EngineCapacity"=898, "Speed"=90, "FuelType"=1, "FuelScore"=509), interval= "prediction")

predict(model3, data.frame("EngineCapacity"=1984, "Speed"=230, "FuelType"=1, "FuelScore"=509), interval = "prediction")

predict(model4, data.frame("EngineCapacity"=1984, "Speed"=230, "FuelType"=1, "FuelScore"=509), interval= "prediction")

##########################################################################
##Regresion Tree

model1 <- rpart(Price ~ .,data= carPriceWithSignificantVar, method= "anova")
model1
rpart.plot(model1, type= 2, digits= 5, fallen.leaves=TRUE)
