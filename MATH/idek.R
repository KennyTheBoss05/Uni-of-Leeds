rm(list = ls())
ls()
library(stringr)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)
#read.csv("http://rgaykroyd.github.io/MATH3823/Datasets/
#adelaide-39
#.csv")

#Question 1
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
attach(data)
colnames(data) #names of columns
summary(data$faculty)["Length"] #Number of rows in the dataset = 60
#data
sum(data$total) #total graduates = 1008
summary(data)
sex = as.factor(sex)
faculty = as.factor(faculty)

plot(year,survive, pch=16,xlab="Year", ylab="Number Survived")
abline(h= mean(survive), lty=2)

data = data[data$sex == 'F',]
data
# Scatter plot
sp <- ggplot(data, aes(year, survive))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp

data$mortality = data$survive/data$total
# Scatter plot
sp <- ggplot(data, aes(year, mortality))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp


data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
attach(data)
data = data[data$sex == 'M',]
data

# Scatter plot
sp <- ggplot(data, aes(year, survive))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp

data$mortality = data$survive/data$total
# Scatter plot
sp <- ggplot(data, aes(year, mortality))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp

#Question 2
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
#cor(x = data[data$sex == 'M',]$year,y = data[data$sex == 'M',]$survive)
#data$total[data$total==0] = 1
#data = data[data$total!=0,]
datao = data[data$total!=0,]
summary(data$faculty)["Length"]
datao$survival = datao$survive/datao$total
# sp <- ggplot(data, aes(data$year, data$total))
# sp <- sp + geom_point(aes(color = faculty)) +
#   theme(legend.position = "top")
# sp
#attach(data)
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
glmfit = glm(data$survive~data$year+data$facultyF+data$sexF, family = poisson(link = "log"))
glmfit2 = glm(data$total~data$year+data$facultyF+data$sexF, family = poisson(link = "log"))
summary(glmfit)
anova(glmfit)
pchisq(244.75,54,lower.tail=F)
pchisq(181.30,52,lower.tail=F)

mean(datao[datao$sex == 'M',]$survival)
mean(datao[datao$sex == 'F',]$survival)

fitted_values <- predict(glmfit, type = "response")
fitted_values
fitted_values2 <- predict(glmfit2, type = "response")
fitted_values2
data$survive
#predict(glm.fit, type="response")
#data$survive - predict(glm.fit, type="response")

testdf = data.frame(YEAR = c(1941,1938),FACULTY = c("M","E"),SEX = c("M","F"))

new_values <- predict(glmfit, newdata = testdf, type = "response")
new_values

#test$sex = as.factor(test$sex)
#test$faculty = as.factor(test$faculty)
#predict(glm.fit,newdata = data,type = "response")
predict(glmfit,newdata =data)
predict(glmfit2,newdata = testdf)

data
#Prediction for year = 1941, faculty = M and Sex = M is 16.764766
round(16.764766,2) #16.76
#Prediction for year = 1938, faculty = E and Sex = F is unknown

#Question 3
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
glmfit = glm(data$survive~data$year+data$sexF, family = poisson(link = "log"))
summary(glmfit)
anova(glmfit)
pchisq(263.38,57,lower.tail=F)

rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$faculty[data$faculty=="E"] = "M"
data$faculty[data$faculty=="A"] = "Z"
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
glmfit = glm(data$survive~data$year+data$sexF+data$facultyF, family = poisson(link = "log"))
summary(glmfit)
anova(glmfit)
pchisq(261.16,55,lower.tail=F)

rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$faculty[data$faculty=="E"] = "M"
data$faculty[data$faculty=="M"] = "S"
#data$faculty[data$faculty=="A"] = "Z"
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
glmfit = glm(data$survive~data$year+data$sexF+data$facultyF, family = poisson(link = "log"))
summary(glmfit)
anova(glmfit)
pchisq(263.31,56,lower.tail=F)

rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$faculty[data$faculty=="E"] = "M"
data$faculty[data$faculty=="M"] = "S"
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
#glmfit = glm(data$survive~data$year+data$sexF + data$sexF:data$facultyF + data$year:data$facultyF, family = poisson(link = "log"))
summary(glmfit)
anova(glmfit)
pchisq(191,49,lower.tail=F)
pchisq(208.04,53,lower.tail=F)
pchisq(208.73,54,lower.tail=F)

#Question 4
datamale = data[data$sex == "M",]
#datafemale
datafemale = data[data$sex == "F",]
attach(datamale)
sex.F = as.factor(sex)
faculty.F = as.factor(faculty)
glm.fit = glm(survive ~ faculty.F ,data= datamale, family='poisson')
summary(glm.fit)
attach(datafemale)
sex.F = as.factor(sex)
faculty.F = as.factor(faculty)
glm.fit = glm(survive ~ faculty.F ,data= datafemale, family='poisson')
summary(glm.fit)
pchisq(279.39,56,lower.tail=F)
attach(data)
sex.F = as.factor(sex)
faculty.F = as.factor(faculty)
glm.fit = glm(survive ~ faculty.F*sex.F ,data= data, family='poisson')
summary(glm.fit)

#Level 5

rm(list = ls())
ls()
library(stringr)
library(ggplot2)

#Question 1
Data = read.csv("engine-39.csv",header=T, fill = TRUE)
#attach(Data)
Data
size = Data$size
wear = Data$wear
plot(size,wear,pch = 16)
summary(Data)
mean(wear)

#Question 2
#lambda = 0.001727764
plot(size,wear,pch = 16)
myfit1 = smooth.spline(size, wear, lambda = 0.01)
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted, col="blue")
#fitted(splinefit)
#wear
SSE = (fitted(myfit1) - wear)^2
SSE
sum(SSE) #4.650879
#lines(fitted(spline.fit),col = 'blue',lwd = 2)
#mysplinefit2 = splinefun(size, wear, method="natural")

#Question 3
my_listx <- list()
my_listy <- list()
for (i in -20:10) {
  # Your code here
  my_listx <- append(my_listx, i)
  myfit = smooth.spline(size, wear, lambda = 10^i)
  fit.locations = seq(0,10,0.01)
  fitted = predict(myfit, fit.locations)
  #lines(fitted, col="blue")
  SSE = (fitted(myfit) - wear)^2
  #SSE
  my_listy <- append(my_listy, sum(SSE))
  
}
my_listx
my_listy
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(0,10),xlab = "lambda (10^x)",ylab = "Sum of Squared Errors",pch = 16)


plot(size,wear,pch = 16)
myfit1 = smooth.spline(size, wear, lambda = 10^-4)
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted, col="blue")
SSE = (fitted(myfit1) - wear)^2
#SSE
print(sum(SSE))


#Question 4
#print(predict(myfit1,1))
my_listx <- list()
my_listy <- list()
for (i in -20:10) {
  # Your code here
  my_listx <- append(my_listx, i)
  myfit1 = smooth.spline(size, wear, lambda = 10^i)
  my_listy <- append(my_listy, predict(myfit1,2.6)$y)
  
}
my_listx
my_listy
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(0,10),xlab = "lambda (10^x)",ylab = "Prediction for size = 2.6L",pch = 16)

#Question 5
my_listx <- list()
my_listy <- list()
for (i in -20:10) {
  # Your code here
  my_listx <- append(my_listx, i)
  myfit = smooth.spline(size, wear, lambda = 10^i)
  fit.locations = seq(0,10,0.01)
  fitted = predict(myfit, fit.locations)
  #lines(fitted, col="blue")
  SSE = (fitted(myfit) - wear)^2
  #SSE
  my_listy <- append(my_listy, sum(SSE))
  
}
my_listx
my_listy
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(0,10),xlab = "lambda (10^x)",ylab = "Sum of Squared Errors",pch = 16)


# plot(size,wear,pch = 16)
# myfit1 = smooth.spline(size, wear, lambda = 10^-4)
# fit.locations = seq(0,10,0.01)
# fitted = predict(myfit1, fit.locations)
# lines(fitted, col="blue")
# SSE = (fitted(myfit1) - wear)^2
# #SSE
# print(sum(SSE))

plot(size,wear,pch = 16)
myfit1 = smooth.spline(size, wear, cv = TRUE)
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted, col="blue")
SSE = (fitted(myfit1) - wear)^2
#SSE
print(sum(SSE))
myfit1$cv.crit
myfit1$lambda
