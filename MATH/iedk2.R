rm(list = ls())
ls()
library(stringr)
library(ggplot2)
#read.csv("http://rgaykroyd.github.io/MATH3823/Datasets/
#adelaide-39
#.csv")

#Question 1
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data = data[data$total!=0,]
attach(data)
colnames(data) #names of columns
summary(data$faculty)["Length"] #Number of rows in the dataset = 60
#data
sum(data$total) #total graduates = 1008
#summary(data)
sexF = as.factor(sex)
facultyF = as.factor(faculty)

plot(year, survive, pch=16,xlab="Year", ylab="Number Survived")
abline(h= mean(survive), lty=2)

#Question 2
#data = data[total!=0,]
data$survived = data$survive/data$total
#data$diff = data$total - data$survive
data$survived
#data$diff
glmfit = glm(data$survived~year+facultyF+sexF, family='binomial')
summary(glmfit)
anova(glmfit)
colnames(data)
pchisq(3.9899,52,lower.tail=F)

#predict(glm.fit, type="response")
#data$survive - predict(glm.fit, type="response")

test = data.frame("year" = c(1941,1938),"faculty" = c("M","E"),"sex" = c("M","F"))
test$sex = as.factor(test$sex)
test$faculty = as.factor(test$faculty)
predict(glm.fit,newdata = data,type = "response")

data
#Prediction for year = 1941, faculty = M and Sex = M is 16.764766
round(16.764766,2) #16.76
#Prediction for year = 1938, faculty = E and Sex = F is unknown

#Question 3
glm.fit = glm(survive~faculty.F+sex.F,data = data, family='poisson')
summary(glm.fit)
#anova(glm.fit)
glm.fit = glm(survive~faculty.F*sex.F,data = data, family='poisson')
summary(glm.fit)
pchisq(223.351,54,lower.tail=F)
#anova(glm.fit)

#Question 4
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$sex = as.factor(data$sex)
data$faculty = as.factor(data$faculty)
datamale = data[data$sex == 'M',]
datafemale = data[data$sex == 'F',]
datafemale$survival = datafemale$survive/datafemale$total
glm.fit = glm(datafemale$survival ~ datafemale$faculty ,data= datafemale, family='binomial')
summary(glm.fit)

datamale$survival = datamale$survive/datamale$total
glm.fit = glm(datamale$survival ~ datamale$faculty ,data= datamale, family='poisson')
summary(glm.fit)

pchisq(279.39,56,lower.tail=F)
attach(data)
sex.F = as.factor(sex)
faculty.F = as.factor(faculty)
glm.fit = glm(survive ~ faculty.F*sex.F ,data= data, family='poisson')
summary(glm.fit)

#Level 5

#Question 1
Data = read.csv("engine-39.csv",header=T, fill = TRUE)
attach(Data)
Data
plot(size,wear,pch = 16)
summary(Data)
mean(wear)

#Question 2
#lambda = 0.001727764
spline.fit = smooth.spline(size,wear,lambda = 0.01)
fitted(spline.fit)
size
SSE = (fitted(spline.fit) - wear)^2
SSE
sum(SSE) #4.650879
lines(fitted(spline.fit),col = 'blue',lwd = 2)

#mysplinefit2 = splinefun(size, wear, method="natural")

#Question 3
#lambda = 0.001727764
spline.fit = smooth.spline(size,wear,lambda = 0.001)
fitted(spline.fit)
size
SSE = (fitted(spline.fit) - wear)^2
SSE
sum(SSE) #4.650879
lines(fitted(spline.fit),col = 'blue',lwd = 2)
