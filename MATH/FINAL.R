rm(list = ls())
ls()
library(stringr)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

#Question 1
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
summary(data)

data = data[data$sex == 'F',]

# Scatter plot
sp <- ggplot(data, aes(year, survive))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp

data$probsurvive = data$survive/data$total
# Scatter plot
sp <- ggplot(data, aes(year, probsurvive))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp


data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data = data[data$sex == 'M',]

# Scatter plot
sp <- ggplot(data, aes(year, survive))
sp <- sp + geom_point(aes(color = faculty)) +
  theme(legend.position = "top")
sp = sp + scale_x_continuous(breaks = c(1938:1947))
sp

data$probsurvive = data$survive/data$total
# Scatter plot
sp <- ggplot(data, aes(year, probsurvive))
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

sex_k = as.factor(data$sex)
faculty_k = as.factor(data$faculty)
year_k = data$year
total_k = data$total
survive_k = data$survive

#4
xk = cbind (survive_k , total_k - survive_k)                                                                                                                                                                      
modelk = glm(xk ~ sex_k+faculty_k+year_k, data = data,family = binomial("logit"))

summary(modelk) 
anova (modelk) 
survivalk = survive_k/total_k


plot(year_k, survivalk , pch=16, xlab ="year", ylim=c(0.4, 1.0),ylab="survive")
abline(h=c(0,1), lty=2)

predict_datakm <- predict(modelk, newdata= data.frame(sex_k = "M", year_k = 1941, faculty_k = "M"), type = "response")
print(predict_datakm)


predict_datakf <- predict(modelk, newdata = data.frame(sex_k = "F", year_k = 1938, faculty_k = "E"), type = "response")
print(predict_datakf)

points(1941, predict_datakm, col = "blue", pch = 19)
points(1938, predict_datakf, col = "pink", pch = 19)

#Question 3
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glm.fit = glm(y ~ data$facultyF+data$sexF, family=binomial(link='logit'))
summary(glm.fit)
anova(glm.fit)
pchisq(29.386,53,lower.tail=F) #0.9965033

fitted = predict(glm.fit,type="response")
fitted
#plot(data$survive/data$total,fitted,pch = 16)
#cor(fitted,data$survive/data$total)

rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data = data[data$total!=0,]
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glm.fit = glm(y ~ data$facultyF + data$sexF + data$facultyF:data$sexF, family=binomial(link='logit'))
summary(glm.fit)
anova(glm.fit)
pchisq(28.381,52,lower.tail=F) #0.9968971

fitted = predict(glm.fit)
fitted
plot(data$survive/data$total,fitted,pch = 16)
cor(fitted,data$survive/data$total)

#Question 4

#Male
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
#data = data[data$total!=0,]
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glmfit = glm(y ~ data$facultyF + data$sexF + data$facultyF:data$sexF, family=binomial(link='logit'))
summary(glmfit)
anova(glmfit)
pchisq(28.381,52,lower.tail=F) #0.9968971

data = data[data$sex == "M",]
fitted = predict(glmfit,newdata = data,type = "response")
fitted

#fittedgr = predict(glmfit, data.frame(year = data$year,faculty = data$faculty,sex = data$sex), type="response")
#lines(data.frame(year = data$year,faculty = data$faculty,sex = data$sex), data)

#data = data[data$sex == "M",]
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glm.fit = glm(y ~ data$facultyF, family=binomial(link='logit'))
summary(glm.fit)
anova(glm.fit)
pchisq(18.709,34,lower.tail=F)

fittedmale = predict(glm.fit)
fittedmale

plot(fitted,fittedmale,pch= 16,col = "red",xlim=c(0.6,0.8), ylim=c(0,1.3))
model = lm(fittedmale~fitted)
abline(model,col = "red")
abline(a = 0, b = 1)
fitted
fittedmale
cor(fittedmale,fitted)

#Female
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
#data = data[data$total!=0,]
data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glmfit = glm(y ~ data$facultyF + data$sexF + data$facultyF:data$sexF, family=binomial(link='logit'))
summary(glmfit)
anova(glmfit)
pchisq(28.381,52,lower.tail=F) #0.9968971

data = data[data$sex == "F",]
fitted = predict(glmfit,newdata = data,type = "response")
fitted

#fittedgr = predict(glmfit, data.frame(year = data$year,faculty = data$faculty,sex = data$sex), type="response")
#lines(data.frame(year = data$year,faculty = data$faculty,sex = data$sex), data)

data$sexF = as.factor(data$sex)
data$facultyF = as.factor(data$faculty)
y = cbind(data$survive, data$total-data$survive)
glm.fit = glm(y ~ data$facultyF, family=binomial(link='logit'))
summary(glm.fit)
anova(glm.fit)
pchisq(9.672,18,lower.tail=F)

fittedfemale = predict(glm.fit)
fittedfemale

plot(fitted,fittedfemale,pch= 16,col = "red",xlim=c(0.6,0.8), ylim=c(0,1.3))
model = lm(fittedfemale~fitted)
abline(model,col = "red")
abline(a = 0, b = 1)
fitted
fittedfemale
cor(fittedfemale,fitted)
points(0.66, 0.66, col = "red", pch = 16)
points(1941, predict_datakm, col = "red", pch = 16)

#Extra
rm(list = ls())
ls()
library(stringr)
library(ggplot2)
data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data = data[data$total!=0,]
data = data[data$sex=="M",]
mean(data[data$faculty=="A",]$survive/data[data$faculty=="A",]$total)
mean(data[data$faculty=="M",]$survive/data[data$faculty=="M",]$total)
mean(data[data$faculty=="S",]$survive/data[data$faculty=="S",]$total)
mean(data[data$faculty=="E",]$survive/data[data$faculty=="E",]$total)

data = read.csv("adelaide-39.csv",header=T, fill = TRUE)
data = data[data$sex=="F",]
mean(data[data$faculty=="A",]$survive/data[data$faculty=="A",]$total)
mean(data[data$faculty=="M",]$survive/data[data$faculty=="M",]$total)
mean(data[data$faculty=="S",]$survive/data[data$faculty=="S",]$total)
mean(data[data$faculty=="E",]$survive/data[data$faculty=="E",]$total)
