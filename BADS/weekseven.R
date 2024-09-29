#install.packages('corrgram')
library(dplyr) 
library(ggplot2) 
library(corrgram)
data <- read.csv('insurance_sample.csv') 
head(data)
data$smoker <- ifelse(data$smoker == 'yes' , 1, 0) 
data$sexismale <- ifelse(data$sex == 'male' , 1, 0) 
data$sex <- NULL

cor(data)
corrgram(data)

dataplot <- data
dataplot$smoker <- as.factor(dataplot$smoker)
dataplot$sexismale <- as.factor(dataplot$sexismale)
ggplot(data=dataplot) + geom_point(aes(age,premiums,color=smoker))+
  labs(title = "Age and Premium Plot with Smoker Information", caption = "Data from Insurance Company",
       tag = "Figure 1",x = "Age",y = "Premium")

ggplot(data=dataplot) + geom_point(aes(age,premiums,color=sexismale))+
  labs(title = "Age and Premiums Plot with Gender Information", caption = "Data from Insurance Company" ,tag = "Figure 2",x = "Age",y = "Premiums")

model <- lm(premiums ~ ., data=data)
summary(model)

datasplit <- data
datasplit$id <- 1:nrow(datasplit)
trainingdata <- datasplit %>% sample_n(700)
testdata <- anti_join(datasplit, trainingdata, by = 'id')
print(dim(trainingdata))
print(dim(testdata))

modeltrainsplit <- lm(premiums ~ ., data=trainingdata)
prediction <- predict(modeltrainsplit, newdata = testdata)
sqrt(mean((testdata$premiums - prediction)^2))

ggplot(data=testdata) + geom_point(aes(bmi,premiums,color='red'))+ geom_point(aes(bmi,prediction,color = 'blue')) + abline(modeltrainsplit)

