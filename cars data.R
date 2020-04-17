# load the required libraries 
library(ggplot2)
library(caTools)
cars<- read.csv(choose.files()) # importing the dataset 
head(cars)
# structure of the datset
str(cars)

##checking for the missing values
colSums(is.na(cars))

##converting factor variables to numbers for statistical operations

cars$fueltype <- factor(cars$fueltype, levels = c("gas","diesel"), labels = c(0,1))
cars$aspiration <- factor(cars$aspiration, levels = c("std","turbo"), labels = c(0,1))
cars$doornumber <- factor(cars$doornumber, levels = c("two","four"), labels = c(0,1))
cars$carbody <- factor(cars$carbody, levels = c("convertible", "hardtop","hatchback","sedan","wagon"), labels = c(1,2,3,4,5))
cars$drivewheel <- factor(cars$drivewheel, levels = c("4wd","fwd","rwd"), labels = c(0,1,2))
cars$enginelocation <- factor(cars$enginelocation, levels = c("front","rear"), labels = c(0,1))
cars$enginetype <- factor(cars$enginetype, levels = c("dohc","dohcv","l","ohc","ohcf","ohcv","rotor"), labels = c(1,2,3,4,5,6,7))
cars$cylindernumber <- factor(cars$cylindernumber, levels = c("two","three","four","five","six","eight","twelve"), labels = c(1,2,3,4,5,6,7))
cars$fuelsystem <- factor(cars$fuelsystem, levels = c("1bbl","2bbl","4bbl","idi","mpfi","mfi","spdi","spfi"), labels = c(1,2,3,4,5,6,7,8))

View(cars)

## remove the the variable car_ID and since they are not statisticallt significant  and wont be influencing the dependant variable

cars<-cars[-1]
cars<-cars[-2]
head(cars)

## spliting the data set into training and test data set

set.seed(100)
split2<- sample.split(cars$price, SplitRatio = 0.70) ##split ratio = 70:30
train<-subset(cars, split2==T)
test<-subset(cars, split2==F)

## checking the number of observation and variables have splitted properly 

table(split2)
nrow(train)
nrow(test)
names(cars)

## Multiple Linear Regression Model bulding for the Training data set 

rg<-lm(price~.,data = train)
summary(rg)

## Building the Model only with statistically significant variables 

reg2 <- lm(price~carwidth+curbweight+enginetype+cylindernumber+enginesize+stroke+boreratio+peakrpm, data= train)
summary(reg2)


## predicting the price of the cars with the help of test data set and Regression Model
prd1<-predict(reg2, newdata = test)
prd1

##comparing the predicted price and the original price of the cars

combine1<- cbind(prd1,test$price)
combine1


## Visualization 

## comparing the pridect price and the original price
plot(test$price, col='blue',type="l",lty=3)
lines(prd1,col='red',type='l',lty=2.5)

## relation of price with different statistically signifincant independant variable 

plot(cars$price, cars$citympg, type ='hex', col= 'Blue')
plot(cars$price, cars$highwaympg, 'hex',col= 'green')
plot(cars$price, cars$stroke, 'hex', col= 'violet')
plot(cars$price, cars$enginesize, 'hex')
plot(cars$price, cars$peakrpm, 'hex')
plot(cars$price, cars$cylindernumber)
