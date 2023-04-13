#Importing required libraries and dataset
library(tidyverse)
dataset=read.csv('forestfires.csv')

#Exploring the forestfires.csv dataset
view(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Checking the dataset for missing values
#No missing values for this dataset
colSums(is.na(dataset))

#Plotting out the dataset for visualization
#Plotting rain
#Plot is unimodal, slight right-skewed, with an outlier
ggplot(data=dataset,
       aes(rain))+
  geom_histogram()

#Plotting temperature
#Plot is bimodal, with a moderate distribution, left-skewed with possible outlier
ggplot(data=dataset,
       aes(temp))+
  geom_histogram()

#Plotting area with histogram plot
ggplot(data=dataset,
       aes(area))+
  geom_histogram()

#Splitting the data
library(caTools)
set.seed(100)
split=sample.split(dataset$area, SplitRatio = 0.8) #80% Training, 20% Testing
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Training the data for MLR
names(dataset)
MLR=lm(formula=area~.,
       data=training_set)
summary(MLR)

#Mean Squared Error
summary=summary(MLR)
MSE=(mean(summary$residuals^2))
paste("The Mean Squared Error is: ", MSE)

#R-Square
summary(MLR)

#Testing set prediction(s)
y_pred=predict(MLR,newdata=testing_set)
data=data.frame(testing_set$area,y_pred)
head(data)
