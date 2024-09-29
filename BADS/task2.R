#install.packages('corrgram')
library(dplyr) 
library(ggplot2) 
library(corrgram)
data <- read.csv('Transactions_Customer.csv') 
head(data)
#data$smoker <- ifelse(data$smoker == 'yes' , 1, 0) 
#data$sexismale <- ifelse(data$sex == 'male' , 1, 0) 
#data$sex <- NULL
data$greaterthan45 = ifelse(data$Estimated_Age > 45 , 1, 0)
cor(data)
corrgram(data)
summary(data)

dataplot <- data
dataplot$greaterthan45 = as.factor(dataplot$greaterthan45)
dataplot$Seen_Voucher <- as.factor(dataplot$Seen_Voucher)
dataplot$Advertisement_Channel <- as.factor(dataplot$Advertisement_Channel)
summary(dataplot)

#ggplot(data=dataplot) + geom_point(aes(Estimated_Income,Revenue,color=greaterthan45))+
#  labs(title = "Income and Revenue Plot with Greaterthan45", caption = "Data from drinks@home.uk",
#       tag = "Figure 1",x = "Estimated_Income",y = "Revenue")

ggplot(data=dataplot) + geom_point(aes(Estimated_Income,Revenue,color=Seen_Voucher))+
  labs(title = "Income and Revenue Plot with Seen_Voucher", caption = "Data from drinks@home.uk",
       tag = "Figure 1",x = "Estimated_Income",y = "Revenue")

ggplot(data=dataplot) + geom_point(aes(Estimated_Income,Revenue,color=Advertisement_Channel))+
  labs(title = "Income and Revenue Plot with Ad info", caption = "Data from drinks@home.uk",
       tag = "Figure 2",x = "Estimated_Income",y = "Revenue")

model <- lm(Revenue ~ Estimated_Income + Advertisement_Channel + Seen_Voucher, data=dataplot)
summary(model)

plot(dataplot$greaterthan45,dataplot$Revenue,col = "orange",xlab = "Greater than 45",ylab = "Revenue")
model <- lm(Revenue ~ greaterthan45, data=dataplot)
summary(model)
abline(model)

plot(dataplot$Estimated_Income,dataplot$Revenue,col = "orange",xlab = "Estimated_Income",ylab = "Revenue")
model <- lm(Revenue ~ Estimated_Income, data=dataplot)
summary(model)
abline(model)

plot(dataplot$Advertisement_Channel,dataplot$Revenue,col = "orange",xlab = "Advertisement_Channel",ylab = "Revenue")
model <- lm(Revenue ~ Advertisement_Channel, data=dataplot)
summary(model)
abline(model)

summary(dataplot$Advertisement_Channel)
summary(data[data$Advertisement_Channel==1,]$Revenue)
summary(data[data$Advertisement_Channel==2,]$Revenue)
summary(data[data$Advertisement_Channel==3,]$Revenue)
summary(data[data$Advertisement_Channel==4,]$Revenue)

plot(dataplot$Seen_Voucher,dataplot$Revenue,col = "orange",xlab = "Seen_Voucher",ylab = "Revenue")
model <- lm(Revenue ~ Seen_Voucher, data=dataplot)
summary(model)
abline(model)

plot(dataplot$Estimated_Age,dataplot$Revenue,col = "orange",xlab = "Estimated_Age",ylab = "Revenue")
model <- lm(Revenue ~ Estimated_Age, data=dataplot)
summary(model)
abline(model)

plot(dataplot$Time_On_Site,dataplot$Revenue,col = "orange",xlab = "Time_On_Site",ylab = "Revenue")
model <- lm(Revenue ~ Time_On_Site, data=dataplot)
summary(model)
abline(model)

datasplit <- data
datasplit$Seen_Voucher <- as.factor(datasplit$Seen_Voucher)
datasplit$Advertisement_Channel <- as.factor(datasplit$Advertisement_Channel)
datasplit$id <- 1:nrow(datasplit)
trainingdata <- datasplit %>% sample_n(280)
testdata <- anti_join(datasplit, trainingdata, by = 'id')
print(dim(trainingdata))
print(dim(testdata))

modeltrainsplit <- lm(Revenue ~ Estimated_Income + Advertisement_Channel + Seen_Voucher, data=trainingdata)
summary(modeltrainsplit)
prediction <- predict(modeltrainsplit, newdata = testdata)
#summary(testdata)
#summary(prediction)
#summary(testdata$Revenue - prediction )
plot(testdata$Revenue,prediction,col = "orange",xlab = "Revenue",ylab = "Prediction")
sqrt(mean((testdata$Revenue - prediction)^2))

#ggplot(data=testdata) + geom_point(aes(bmi,premiums,color='red'))+ geom_point(aes(bmi,prediction,color = 'blue'))

mean(dataplot[dataplot$greaterthan45==0,]$Revenue)
mean(dataplot[dataplot$greaterthan45==1,]$Revenue)


summary(dataplot[dataplot$Seen_Voucher==0,]$Revenue)
summary(dataplot[dataplot$Seen_Voucher==1,]$Revenue)

summary(dataplot[dataplot$Advertisement_Channel!=4,]$Revenue)
summary(dataplot[dataplot$Advertisement_Channel==4,]$Revenue)

#Start the Evaluation
model <- lm(Revenue ~ Estimated_Income + Seen_Voucher + greaterthan45, data=dataplot)
summary(model)
td = dataplot
#td$Estimated_Age <- NULL
td$greaterthan45 = ifelse(TRUE , 1, 0)
td$greaterthan45 = as.factor(td$greaterthan45)
prediction <- predict(model, newdata = td)
plot(td$Revenue,prediction,col = "orange",xlab = "Revenue",ylab = "Prediction")
mean(prediction)
mean(td$Revenue)

summary(dataplot[dataplot$greaterthan45==0,]$Revenue)
summary(dataplot[dataplot$greaterthan45==1,]$Revenue)
