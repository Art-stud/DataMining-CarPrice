#Data Source : survey file: Data_survey

#MED Project
# dependent var(Y)= price
# independent Var(X)= vehicleType, 
#                     yearOfRegistration, 
#                     gearbox, 
#                     powerPS, 
#                     model, 
#                     kilometer, 
#                     fuelType, 
#                     brand, 
#                     notRepairedDamage
install.packages("Metrics")
install.packages("rio")
install.packages("visualize")
install.packages("writexl")
install.packages("xlsx")
install.packages("tidyverse")
library(markdown)
library(dplyr)
library(MASS)
library(gganimate)
library(tidyverse)
library(psych)
library(GPArotation)
library(knitr)
library(caret)
library(readxl)
library(rio)
library(visualize)
library(xlsx)
install.packages("CRAN")
install.packages("reldist")
library(ineq)
library(randomForest)
library(rpart)
library(rpart.plot)


#Choose Read data

file_csv <- read.csv(file.choose())
file_csv


x <- 1*nrow(file_csv)
if(x > 0) { 
  carPrice_wrote<-print.data.frame(file_csv)
  
} else { 
  print(paste("Choose the file with pattern .csv")) 
  carPrice_wrote <- read.csv(file.choose())
}


#show data
str(carPrice_wrote)
head(carPrice_wrote)
glimpse(carPrice_wrote)
nrow(carPrice_wrote)
ncol(carPrice_wrote)

## Choose only atribiutes for analyze from data
carPrice_selected <- select(carPrice_wrote, price, vehicleType, yearOfRegistration, gearbox, powerPS, model, kilometer, fuelType, brand, notRepairedDamage)
str(carPrice_selected)
head(carPrice_selected)
glimpse(carPrice_selected)
nrow(carPrice_selected)
min(carPrice_selected$price)

## Clean the data

## Change 0 to NA in all data
carPrice_selected[carPrice_selected == 0] <- NA
carPrice_selected

## Change empty cell to NA in all data
carPrice_selected[carPrice_selected == ""] <- NA
carPrice_selected

## Change "," to NA in all data
carPrice_selected[carPrice_selected == ","] <- NA
carPrice_selected

## Change "," to NA in all data
carPrice_selected[carPrice_selected == "."] <- NA
carPrice_selected


## Delete rows and column with NA cell
carPrice_clean_NA <-carPrice_selected[complete.cases(carPrice_selected),]
carPrice_clean_NA
glimpse(carPrice_clean_NA)


# OUTLIERS

#Outliers Of price

library(ggplot2)

box_plot_price_NA <- ggplot(carPrice_clean_NA, aes(y=price)) + theme_bw()
box_plot_price_NA + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

min(carPrice_clean_NA$price)
max(carPrice_clean_NA$price)

# Cut the outliers of price

quantPrice <- quantile(carPrice_clean_NA$price, probs=c(0.05, 0.95))
quantPrice

priceCut <- carPrice_clean_NA %>% filter(price > quantPrice[1], price < quantPrice[2])
priceCut

library(ggplot2)

box_plot_price <- ggplot(priceCut, aes(y=price)) + theme_bw()
box_plot_price + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

hist_price <- ggplot(priceCut, aes(x=price)) + theme_bw()
hist_price + geom_histogram(aes(y=..density..), fill="#756bb1", col="#f2f0f7", binwidth=300) +
  geom_density(col="#f1a340", lwd=1)

quantPrice_2 <- quantile(priceCut$price, probs=c(0.05, 0.935))
quantPrice_2

priceCut_2<- priceCut %>% filter(price > quantPrice_2[1], price < 10000)
priceCut_2

box_plot_price_2 <- ggplot(priceCut_2, aes(y=price)) + theme_bw()
box_plot_price_2 + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

hist_price_2 <- ggplot(priceCut_2, aes(x=price)) + theme_bw()
hist_price_2 + geom_histogram(aes(y=..density..), fill="#756bb1", col="#f2f0f7", binwidth=300) +
  geom_density(col="#f1a340", lwd=1)

carPrice_clean_NA_1 <- priceCut_2
glimpse(carPrice_clean_NA_1)
max(carPrice_clean_NA_1$price)
############################################################################

# Outliers of powerPS

library(ggplot2)

box_plot_powerPS_NA <- ggplot(carPrice_clean_NA_1, aes(y=powerPS)) + theme_bw()
box_plot_powerPS_NA + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

min(carPrice_clean_NA$powerPS)
max(carPrice_clean_NA$powerPS)

# Cut the outliers of powerPS

quantpowerPS <- quantile(carPrice_clean_NA_1$powerPS, probs=c(0.05, 0.95))
quantpowerPS

powerPSCut <- carPrice_clean_NA_1 %>% filter(powerPS > quantpowerPS[1], powerPS < quantpowerPS[2])
powerPSCut

library(ggplot2)

box_plot_powerPS <- ggplot(powerPSCut, aes(y=powerPS)) + theme_bw()
box_plot_powerPS + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

hist_powerPS <- ggplot(powerPSCut, aes(x=powerPS)) + theme_bw()
hist_powerPS + geom_histogram(aes(y=..density..), fill="#756bb1", col="#f2f0f7", binwidth=1) +
  geom_density(col="#f1a340", lwd=1)


carPrice_clean_NA_2 <- powerPSCut
glimpse(carPrice_clean_NA_2)
max(carPrice_clean_NA_2$powerPS)
########################################################

# Outliers of yearOfRegistration

library(ggplot2)

box_plot_yearOfRegistration_NA <- ggplot(carPrice_clean_NA_2, aes(y=yearOfRegistration)) + theme_bw()
box_plot_yearOfRegistration_NA + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

min(carPrice_clean_NA_2$yearOfRegistration)
max(carPrice_clean_NA_2$yearOfRegistration)

# Cut the outliers of yearOfRegistration

quantyearOfRegistration <- quantile(carPrice_clean_NA_2$yearOfRegistration, probs=c(0.05, 0.95))
quantyearOfRegistration

yearOfRegistrationCut <- carPrice_clean_NA_2 %>% filter(yearOfRegistration > 1998, yearOfRegistration < quantyearOfRegistration[2])
yearOfRegistrationCut

library(ggplot2)

box_plot_yearOfRegistration <- ggplot(yearOfRegistrationCut, aes(y=yearOfRegistration)) + theme_bw()
box_plot_yearOfRegistration + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

hist_yearOfRegistration <- ggplot(yearOfRegistrationCut, aes(x=yearOfRegistration)) + theme_bw()
hist_yearOfRegistration + geom_histogram(aes(y=..density..), fill="#756bb1", col="#f2f0f7", binwidth=1) +
  geom_density(col="#f1a340", lwd=1)


carPrice_clean_NA_3 <- yearOfRegistrationCut
glimpse(carPrice_clean_NA_3)
max(carPrice_clean_NA_2$yearOfRegistration)
 
##############################################################

# Outliers of kilometer

library(ggplot2)

box_plot_kilometer_NA <- ggplot(carPrice_clean_NA_3, aes(y=kilometer)) + theme_bw()
box_plot_kilometer_NA + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

min(carPrice_clean_NA_3$kilometer)
max(carPrice_clean_NA_3$kilometer)

# Cut the outliers of kilometer

quantkilometer <- quantile(carPrice_clean_NA_3$kilometer, probs=c(0.05, 0.95))
quantkilometer

kilometerCut <- carPrice_clean_NA_3 %>% filter(kilometer > 80000, kilometer < 110000)
kilometerCut

library(ggplot2)

box_plot_kilometer <- ggplot(kilometerCut, aes(y=kilometer)) + theme_bw()
box_plot_kilometer + geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=1)

hist_kilometer <- ggplot(kilometerCut, aes(x=kilometer)) + theme_bw()
hist_kilometer + geom_histogram(aes(y=..density..), fill="#756bb1", col="#f2f0f7", binwidth=3000) +
  geom_density(col="#f1a340", lwd=1)

carPrice_clean_0 <- kilometerCut
###########################################################################

min(carPrice_clean_0$price)
max(carPrice_clean_0$price)
min(carPrice_clean_0$powerPS)
max(carPrice_clean_0$powerPS)
min(carPrice_clean_0$yearOfRegistration)
max(carPrice_clean_0$yearOfRegistration)
min(carPrice_clean_0$kilometer)
glimpse(carPrice_clean_0)
nrow(carPrice_clean_0)

#export clean file from csv to xlsx

library(rio)
export(list(carPrice_clean_0 = carPrice_clean_0, iris=iris), "carPrice_clean_0.xlsx")

print("If you want save the xlsx file copy dicectory where the file was saved")

library("xlsx")

x <- read_xlsx(choose.files())

 if(x == "carPrice_clean_0.xlsx") { 
  
   carPrice_Clean_0_xlsx <- read_xlsx(choose.files())
  
} else { 
  
  carPrice_Clean_0_xlsx <- read_xlsx("carPrice_clean_0.xlsx")
}

carPrice_Clean_0_xlsx
carPrice_1 <- carPrice_Clean_0_xlsx

glimpse(carPrice_1)
head(carPrice_1)
nrow(carPrice_1)

library(dplyr)

##Checking categorical values
#fuelType
fuelTypeOnly_1<- carPrice_1 %>% select(fuelType)
fueltypetrial_1<-as.character(unique(fuelTypeOnly_1))
fueltypetrial_1

#model
modelOnly <- carPrice_1 %>% select(model)
modeltrial <- as.character(unique(modelOnly))
modeltrial

#vehicleType
vehicleTypeOnly <- carPrice_1 %>% select(vehicleType)
vehicleTypetrial <- as.character(unique(vehicleTypeOnly))
vehicleTypetrial

#gearbox
gearboxOnly <- carPrice_1 %>% select(gearbox)
gearboxtrial <- as.character(unique(gearboxOnly))
gearboxtrial

#brand
brandOnly <- carPrice_1 %>% select(brand)
brandtrial <- as.character(unique(brandOnly))
brandtrial

#notRepairedDamage
notRepairedDamageOnly <- carPrice_1 %>% select(notRepairedDamage)
notRepairedDamagetrial <- as.character(unique(notRepairedDamageOnly))
notRepairedDamagetrial

print('Checking categorical values:')
fueltypetrial_1
modeltrial
vehicleTypetrial
gearboxtrial
brandtrial
notRepairedDamagetrial

library(dplyr)

glimpse(carPrice_1)
##function changing fueltype from categorical into numerical
fueltypeFunc_1 <- function(carPrice_1, fueltypetrial_1){
  factor(carPrice_1, levels = fueltypetrial_1, labels = fueltypetrial_1, ordered = TRUE)
}

glimpse(carPrice_1)
#converting fueltype into numerical var
trial <- map_df(fuelTypeOnly_1, fueltypeFunc_1) %>% glimpse
trial <- map_df(trial, as.numeric)%>% glimpse
carPrice_1[8]<- trial
glimpse(carPrice_1)


##function changing model from categorical into numerical
modelFunc <- function(carPrice_1, modeltrial){
  factor(carPrice_1, levels = modeltrial, labels = modeltrial, ordered = TRUE)
}

glimpse(carPrice_1)
#converting model into numerical var
trial_2 <- map_df(modelOnly, modelFunc) %>% glimpse
trial_2 <- map_df(trial_2, as.numeric)%>% glimpse
carPrice_1[6]<- trial_2
glimpse(carPrice_1)

##function changing vehicleType from categorical into numerical
vehicleTypetrialFunc <- function(carPrice_1, vehicleTypetrial){
  factor(carPrice_1, levels = vehicleTypetrial, labels = vehicleTypetrial, ordered = TRUE)
}

glimpse(carPrice_1)
#converting vehicleType into numerical var
trial_3 <- map_df(vehicleTypeOnly, vehicleTypetrialFunc) %>% glimpse
trial_3 <- map_df(trial_3, as.numeric)%>% glimpse
carPrice_1[2]<- trial_3
glimpse(carPrice_1)


##function changing gearbox from categorical into numerical
gearboxtrialFunc <- function(carPrice_1, gearboxtrial){
  factor(carPrice_1, levels = gearboxtrial, labels = gearboxtrial, ordered = TRUE)
}

glimpse(carPrice_1)
#converting gearbox into numerical var
trial_4 <- map_df(gearboxOnly, gearboxtrialFunc) %>% glimpse
trial_4 <- map_df(trial_4, as.numeric)%>% glimpse
carPrice_1[4]<- trial_4
glimpse(carPrice_1)

##function changing brand from categorical into numerical
brandtrialFunc <- function(carPrice_1, brandtrial){
  factor(carPrice_1, levels = brandtrial, labels = brandtrial, ordered = TRUE)
}

glimpse(carPrice_1)
#converting brand into numerical var
trial_5 <- map_df(brandOnly, brandtrialFunc) %>% glimpse
trial_5 <- map_df(trial_5, as.numeric)%>% glimpse
carPrice_1[9]<- trial_5
glimpse(carPrice_1)

##function changing notRepairedDamage from categorical into numerical
notRepairedDamagetrialFunc <- function(carPrice_1, notRepairedDamagetrial){
  factor(carPrice_1, levels = notRepairedDamagetrial, labels = notRepairedDamagetrial, ordered = TRUE)
}

glimpse(carPrice_1)
#converting notRepairedDamage into numerical var
trial_6 <- map_df(notRepairedDamageOnly, notRepairedDamagetrialFunc) %>% glimpse
trial_6 <- map_df(trial_6, as.numeric)%>% glimpse
carPrice_1[10]<- trial_6
glimpse(carPrice_1)


# Filltering importand category from dataset
library(dplyr)
carPrice_1a <- select(carPrice_1, price, vehicleType, yearOfRegistration, powerPS, model, kilometer, fuelType)
carPrice_1a


carPrice_2 <- carPrice_1a[c(1,2,3,4,6)]
carPrice_2


library(rio)
library(xlsx)
export(list(carPrice_2 = carPrice_2, iris=iris), "carPrice_2.xlsx")

#create the vectors

car_kilometer<-carPrice_2$kilometer
car_powerPs <- carPrice_2$powerPS
car_yearOfRegistration<-carPrice_2$yearOfRegistration
car_vehicleType <- carPrice_2$vehicleType

###############################################################
##Linear Model
linmodel_0 <- lm(carPrice_2$price~car_vehicleType+car_yearOfRegistration+car_powerPs+car_kilometer,data = carPrice_2)
summary(linmodel_0)

##########################################################################

# remove column ehicleType

carPrice_3 <- carPrice_2[c(1,3,4,5)]
carPrice_3
####################################################
linmodel <- lm(carPrice_3$price~car_yearOfRegistration+car_powerPs+car_kilometer,data = carPrice_3)
summary(linmodel)
plot(linmodel)
# Choose the car form predictor for example

#n<-nrow(carPrice_Clean_0_xlsx)
#n
#car_predictor_by_User <- readline(prompt="Enter number of car: ")
#car_predictor_by_User <- as.integer(car_predictor_by_User)
#car_predictor_by_User
##########################################################################
# b value for construct predicted from dataset
# Choose on emodel with those b1,b2,b3, b4 values
# b1 is for kilometer, b2 is for powerPS, 

#car_predict_User <- carPrice_Clean_0_xlsx[paste0(car_predictor_by_User),]
#car_predict_User

#car_predict_numerical <- carPrice_2[paste0(car_predictor_by_User),]
#car_predict_numerical

#print(paste("The", car_predictor_by_User,"th car from dataset is:"))
#car_predict_model<-t(car_predict_User)
#car_predict_model

car_predict_0 <- carPrice_Clean_0_xlsx[11928,]
car_predict_0

car_predict_all_numerical <- carPrice_1[11928,]
car_predict_all_numerical

car_predict_numerical <- carPrice_3[11928,]
car_predict_numerical

car_predict_d<-t(car_predict_0)
car_predict_d

#Numerical version
car_predict_all_numerical<-t(car_predict_all_numerical)
car_predict_all_numerical

#Choose the best coefficient significantsto use to the model
car_predict<-t(car_predict_numerical)
car_predict

# b value for construct predicted from dataset
# Choose on emodel with those b1,b2,b3, b4 values
# b1 is for kilometer, b2 is for powerPS, 

Y<-car_predict[1] # b1 is for price
Y
b1<-car_predict[2] # b1 is for yearOfRegistration
b1
b2<-car_predict[3] # b1 is for powerPs
b2
b3<-car_predict[4] # b1 is for kilometer
b3

# Get automatical coefficients values from linmodel

linmodel_numerical<-coef(linmodel)
linmodel_numerical

linmodel_values<- t(linmodel_numerical)
linmodel_values

#from linmodel
###################################
intercept <-linmodel_values[1]
coef_yearOfRegistration <- linmodel_values[2]
coef_powerPS <- linmodel_values[3]
coef_kilometer <- linmodel_values[4]

##############################################

print("Data from linmodel")
linmodel_values
intercept
coef_yearOfRegistration
coef_powerPS
coef_kilometer

# predicted y = a +bx from linmodel
predictedPrice <- intercept+(b1*coef_yearOfRegistration)+(b2*coef_powerPS)+(b3*coef_kilometer)
predictedPrice


x_lower<- (predictedPrice-(predictedPrice)*0.005)
x_lower

x_highter<- (predictedPrice+(predictedPrice)*0.005)
x_highter


car_predicted <- carPrice_Clean_0_xlsx %>% filter(price > x_lower, price < x_highter)
car_predicted


########################################
car_predict[1]
predictedPrice
car_predicted



#create file for user edit example car
print("You can add yours new car, this file will be usfull later")

car_predict_user <- matrix(c("","","","","","","","","",""), ncol=10, byrow=TRUE)
colnames(car_predict_user) <- c("price","vehicleType","yearOfRegistration","gearbox","powerPS","model","kilometer","fuelType","brand","notRepairedDamage")
car_predict_user <- as.table(car_predict_user)
car_predict_user

library(rio)
library(xlsx)
export(list(car_predict_user = car_predict_user, iris=iris), "car_predict_user.xlsx")


###############################################
#linear reregretion y= mx+b

carPriceWithSignificantVar_3<- carPrice_3
carPriceWithSignificantVar_3
glimpse(carPrice_3)
glimpse(carPriceWithSignificantVar_3)

##########################################################
#VAlidation set approach

library(tidyverse)
library(caret)

set.seed(123)
training.samples <- carPriceWithSignificantVar_3$price %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data_1  <- carPriceWithSignificantVar_3[training.samples, ]
test.data_1 <- carPriceWithSignificantVar_3[-training.samples, ]
test.data_1


# Build the model
model_1 <- lm(price ~., data = train.data_1)
# Make predictions and compute the R2, RMSE and MAE
predictions_3 <- model_1 %>% predict(test.data_1)
#Predicted Value
data.frame(test.data_1, predictions_3)

data.frame(R2 = R2(predictions_3, test.data_1$price),
            RMSE = RMSE(predictions_3, test.data_1$price),
            MAE = MAE(predictions_3, test.data_1$price))
print(test.data_1$price)
#prediction error rate
RMSE(predictions_3, test.data_1$price)/mean(test.data_1$price)
MAE(predictions_3, test.data_1$price)
#sum
summary(model_1)
model_1

#######################################################################
#Leave one out cross validation - LOOCV - we don't use it method,
#because the proces the process is repeated as many times as there are data points
# and the points are 
nrow(carPriceWithSignificantVar_3)
#######################################################################
#K-fold cross-validation
# Define training control

set.seed(123) 
train.control_2 <- trainControl(method = "cv", number = 3)
# Train the model
model_2 <- train(price ~., data = carPriceWithSignificantVar_3, method = "lm",
                trControl = train.control)
# Summarize the results
print(model_2)
model_2
#######################################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 3, repeats = 10)
# Train the model
model_3 <- train(price ~., data = carPriceWithSignificantVar_3, method = "lm",
                trControl = train.control)
# Summarize the results
print(model_3)
model_3
######################################################################
##Multiple Regression PREDICTION 
##using validation set approach

glimpse(carPrice_clean_0)

#create automatic file with example car
car_predict_automatic <- matrix(c(6250,7,2009,2,105,104,125000,2,30,2), ncol=10, byrow=TRUE)
colnames(car_predict_automatic) <- c("price","vehicleType","yearOfRegistration","gearbox","powerPS","model","kilometer","fuelType","brand","notRepairedDamage")
car_predict_automatic <- as.table(car_predict_automatic)
car_predict_automatic

library(rio)
library(xlsx)
export(list(car_predict_automatic = car_predict_automatic, iris=iris), "car_predict_automatic.xlsx")

######################################################################################
library(rio)
library(xlsx)
print("You can use yours new car read file: car_predict_user or use car created automatic file: car_predict_automatic.xlsx")

car_price_predict <- read_xlsx(choose.files())


#x1 <- read_xlsx(choose.files())

  #if(x1 == "car_predict_user.xlsx" ) { 
  
  #car_price_predict <- read_xlsx(choose.files())
  
#} else { 
  
  #car_price_predict <- read_xlsx("car_predict_automatic.xlsx")
#}

head(car_price_predict)

##################################################################################
predict_1<-predict(model_1, data.frame(car_price_predict),interval= "prediction")

predict_2<-predict(model_2, data.frame(car_price_predict), interval = "prediction")

predict_3<-predict(model_3, data.frame(car_price_predict), interval= "prediction")

################################################################################
#created predictors check cars from the dataset

predict_1
predict_2
predict_3

# For predictor_1
predict_1
x_lower<- (predict_1-(predict_1)*0.005)
x_lower
x_highter<- (predict_1+(predict_1)*0.005)
x_highter
car_predicted_1 <- carPrice_Clean_0_xlsx %>% filter(price > x_lower, price < x_highter)
car_predicted_1

# For predictor_2

predict_2
x_lower<- (predict_2-(predict_2)*0.005)
x_lower
x_highter<- (predict_2+(predict_2)*0.005)
x_highter
car_predicted_2 <- carPrice_Clean_0_xlsx %>% filter(price > x_lower, price < x_highter)
car_predicted_2

# For predictor_3

predict_3
x_lower<- (predict_3-(predict_3)*0.005)
x_lower
x_highter<- (predict_3+(predict_3)*0.005)
x_highter
car_predicted_3 <- carPrice_Clean_0_xlsx %>% filter(price > x_lower, price < x_highter)
car_predicted_3

car_price_predict$price
predict_1
predict_2
predict_3

##########################################################################
##Regresion Tree
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

glimpse(carPrice_clean_0)
glimpse(carPrice_1)
glimpse(carPrice_2)
glimpse(carPrice_3)

# ANOVA

carPriceWithSignificantVar_4<- carPrice_1
carPriceWithSignificantVar_4
glimpse(carPriceWithSignificantVar_4)

model_4 <- rpart(price ~ .,data= carPriceWithSignificantVar_4, method= "anova")
model_4
triel_4<-rpart.plot(model_4, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_4)

carPriceWithSignificantVar_5<- carPrice_2

model_5 <- rpart(price ~ .,data= carPriceWithSignificantVar_5, method= "anova")
model_5
triel_5<-rpart.plot(model_5, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_5)

# ANOVA price

carPriceWithSignificantVar_6<- carPrice_3

model_6 <- rpart(price ~ .,data= carPriceWithSignificantVar_6, method= "anova")
model_6
rpart.plot(model_6, type= 2, digits= 5, fallen.leaves=TRUE)

# ANOVA powerPS

carPriceWithSignificantVar_7<- carPrice_3

model_7 <- rpart(powerPS ~ .,data=carPriceWithSignificantVar_7, method= "anova")
model_7
rpart.plot(model_7, type= 2, digits= 5, fallen.leaves=TRUE)

# ANOVA yearOfRegistration

carPriceWithSignificantVar_8<- carPrice_3

model_8 <- rpart(yearOfRegistration ~ .,data=carPriceWithSignificantVar_8, method= "anova")
model_8
rpart.plot(model_8, type= 2, digits= 5, fallen.leaves=TRUE)

# ANOVA kilometer

carPriceWithSignificantVar_9<- carPrice_3

model_9 <- rpart(kilometer ~ .,data=carPriceWithSignificantVar_9 , method= "anova")
model_9
rpart.plot(model_9, type= 2, digits= 5, fallen.leaves=TRUE)

# POISSON price carPrice_1

carPriceWithSignificantVar_10<- carPrice_1

model_10<- rpart(price ~ .,data= carPriceWithSignificantVar_10, method= "poisson")
model_10
rpart.plot(model_10, type= 2, digits= 5, fallen.leaves=TRUE)

# POISSON price carPrice_2

carPriceWithSignificantVar_11<- carPrice_1

model_11 <- rpart(price ~ .,data= carPriceWithSignificantVar_11, method= "poissona")
model_11
triel_11<-rpart.plot(model_11, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_11)

# POISSON price carPrice_3

carPriceWithSignificantVar_12<- carPrice_3

model_12 <- rpart(price ~ .,data= carPriceWithSignificantVar_12, method= "poissona")
model_12
triel_12<-rpart.plot(model_12, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_12)

# POISSON yearOfRegistration carPrice_3

carPriceWithSignificantVar_13<- carPrice_3

model_13 <- rpart(yearOfRegistration ~ .,data= carPriceWithSignificantVar_13, method= "poissona")
model_13
triel_13<-rpart.plot(model_13, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_13)

# POISSON powerPS carPrice_3

carPriceWithSignificantVar_14<- carPrice_3

model_14 <- rpart(powerPS ~ .,data= carPriceWithSignificantVar_14, method= "poissona")
model_14
triel_14<-rpart.plot(model_14, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_14)

# POISSON kilometer carPrice_3

carPriceWithSignificantVar_15<- carPrice_3

model_15 <- rpart(kilometer ~ .,data= carPriceWithSignificantVar_15, method= "poissona")
model_15
triel_15<-rpart.plot(model_15, type= 2, digits= 5, fallen.leaves=TRUE)
summary(triel_15)

#plot(triel)

library(ineq)
library(randomForest)
library(rpart)
library(rpart.plot)

text(model_1,use.n=TRUE)
Random_Forest<- randomForest(price ~ .,importance= TRUE, data=carPrice_3)
Random_Forest
varImpPlot(Ranfom_Forest)
y<- car_price_predict
y
predict(Random_Forest,y)

##############################################
#Conclution

car_price_predict
predict_1
car_predicted_1
car_price_predict
predict_2
car_predicted_2
car_price_predict
predict_3
car_predicted_3
car_price_predict
rpart.plot(model_6, type= 2, digits= 5, fallen.leaves=TRUE)
predict(Random_Forest,y)

car_price_predict
predict_1
predict_2
predict_3
rpart.plot(model_6, type= 2, digits= 5, fallen.leaves=TRUE)
predict(Random_Forest,y)
