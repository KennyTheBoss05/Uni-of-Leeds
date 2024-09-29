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
plot(size,wear,xlab = "size (L)",ylab = "wear (no unit)",pch = 16)
model = lm(wear~size)
abline(model,col= "red")
summary(Data)
mean(wear)
hist(wear,xlab = "wear (no unit)", border = 129,density = 100,col = "red")
hist(size,xlab = "size (L)", border = 129,density = 100,col = "red")
boxplot(size,col = "red",xlab = "size (L)")
cor(wear,size)

#Question 2
#lambda = 0.001727764
plot(size,wear,pch = 16,xlim=c(0.5,3.5), ylim=c(1.5,6))
myfit1 = smooth.spline(size, wear, lambda = 10^-4)
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
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(0,10),xlab = "log(lambda)",ylab = "Sum of Squared Errors",pch = 16)


plot(size,wear,pch = 16)
myfit1 = smooth.spline(size, wear, lambda = 10^-4)
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted, col="blue")
SSE = (fitted(myfit1) - wear)^2
#SSE
print(sum(SSE))


#Question 4
print(predict(myfit1,2.6))
my_listx <- list()
my_listy <- list()
for (i in -20:10) {
  # Your code here
  my_listx <- append(my_listx, i)
  myfit1 = smooth.spline(size, wear, lambda = 10^i)
  my_listy <- append(my_listy, predict(myfit1,1)$y)
  
}
my_listx
my_listy
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(1,6),xlab = "log(lambda)",ylab = "Prediction for size = 1L",pch = 16)

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
  my_listy <- append(my_listy, myfit$crit)
  
}
my_listx
my_listy
plot(my_listx,my_listy,xlim=c(-20,10), ylim=c(0,0.6),xlab = "log(lambda)",ylab = "Q_ocv",pch = 16)


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
round(0.001727764,2)

#Question 6
end = FALSE
lamb = 10^-3
count = 0
chk = 0
while(end==FALSE)
{
  #my_listx <- append(my_listx, i)
  l = lamb + count*0.01
  myfit = smooth.spline(size, wear, lambda = l)
  fit.locations = seq(0,4,0.01)
  fitted = predict(myfit, size)
  #print(class(fitted))
  for (i in 1:length(fitted$y))
  {
    if (fitted$y[i+1] < fitted$y[i])
    {
      chk = 1
      break
    }
  }
  if (chk == 0)
  {
    end = TRUE
  }
  chk = 0
  count = count + 1
}
print("The lowest lambda which leads to a monotonic fitted spline is : ",l)

plot(size,wear,pch = 16)
myfit1 = smooth.spline(size, wear, lambda = 4*(10^-3))
fit.locations = seq(0,10,0.01)
fitted = predict(myfit1, fit.locations)
lines(fitted, col="blue")
SSE = (fitted(myfit1) - wear)^2
#SSE
print(sum(SSE))
