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
#install.packages("Metrics")
library(caret)
library(readxl)

#data Preparation

# add code to read csv and code to change the csv to exel
# then combine two datebase and then read like exel file 
#Choose Read data
fileCsv <- read.csv(file.choose())
fileCsv
#If user choose different file then csv the thje window with choice will open again
x <- 1*nrow(fileCsv)
if(x > 0) {   
  carPriceCsv <-print.data.frame(fileCsv)
} else { 
      carPrice <-read_xlsx(file.choose())   #open again window with choice
      }
#if user choose csv the code convert csv to exel 
#it's two good idea, first teh user can convert every file from csv to exel
# and second: the code can calculate the data
x1<-1*nrow(carPriceCsv)
if(x1>0) {
  #write.xlsx(carPriceCsv, carPrice1.xlsx)  ##think
} else {
  print.data.frame(carPrice)   
}

head(carPrice)
head(carPrice1)


View(carPrice)
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

##removing column FuelType
#carPriceClean2 <-carPrice[c(1,2,3,5)]
#carPriceClean2
#glimpse(carPriceClean2)

##Linear Model 2
#linmodel2 <- lm(carPriceClean2$Price~carPriceClean2$EngineCapacity+carPriceClean2$Speed+carPriceClean2$FuelScore,data = carPriceClean2)
#summary(linmodel2)                  
#linmodel2
#plot(linmodel2)
#abline(linmodel2)

mean<-mean(carPriceClean$Price)
mean


#linearequation y= mx+b
#y= predictedPriceVolkswagenGolfGTI
predictedPriceVolkswagenGolfGTI <--58887.488+ (1984*-2.526)+(245 *481.112)+(522*129.323)
predictedPriceVolkswagenGolfGTI

carPriceWithSignificantVar<- carPriceClean[c(1,2,3,5)]
carPriceWithSignificantVar
glimpse(carPriceClean)
glimpse(carPriceWithSignificantVar)


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
print(model3)

#######################################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 3, repeats = 10)
# Train the model
model4 <- train(Price ~., data = carPriceWithSignificantVar, method = "lm",
               trControl = train.control)
# Summarize the results
print(model4)

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
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


carPriceWithSignificantVar<- carPriceClean[c(1,2,3,4,5)]
carPriceWithSignificantVar
glimpse(carPriceWithSignificantVar)


model1 <- rpart(Price ~ .,data= carPriceWithSignificantVar, method= "anova")
model1
rpart.plot(model1, type= 2, digits= 5, fallen.leaves=TRUE)


glimpse(carPriceClean)

carPrice_Price<- carPriceClean[c(1,2,4,5)]
carPrice_Price
glimpse(carPrice_Price)

model2 <- rpart(Price ~ .,data= carPrice_Price,  method= "anova")
model2
rpart.plot(model2, type= 2, digits= 5, fallen.leaves=TRUE)

carPrice_speed<- carPriceClean[c(1,2,3,4,5)]
carPrice_speed
glimpse(carPrice_speed)


model3 <- rpart(EngineCapacity ~ .,data= carPrice_speed, method= "anova")
model3
rpart.plot(model3, type= 2, digits= 5, fallen.leaves=TRUE)


model4 <- rpart(Speed ~ .,data= carPrice_speed, method= "poisson")
model4
rpart.plot(model4, type= 2, digits= 5, fallen.leaves=TRUE)

#I think taht will be very googd describe
model5 <- rpart(EngineCapacity ~ .,data= carPrice_speed, method= "poisson")
model5
rpart.plot(model5, type= 2, digits= 5, fallen.leaves=TRUE)


#write.table(model5, file="ExportedFileName.csv", sep=",")
