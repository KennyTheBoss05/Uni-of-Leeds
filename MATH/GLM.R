#install.packages(stringr)
library(stringr)
data = read.table("https://rgaykroyd.github.io/MATH3823/Datasets/EngineEmissions-39.csv", header=T, fill = TRUE)
colnames(data)
str_split_fixed(data$HC, ",", 3)
df = as.data.frame(str_split_fixed(data$HC, ",", 3))
colnames(df) = c("HC","CO","NOX")
HC = as.numeric(df[,1])
CO = as.numeric(df[,2])
NOX = as.numeric(df[,3])
df <- data.frame(HC, CO, NOX)
attach(df)
summary(df)
cor(df)

plot(NOX~HC, pch=16)
my.lm = lm(NOX ~ HC)
summary(my.lm)
abline(my.lm)

# Calculate the std dev of the residuals
resid.sd = sd(my.lm$residuals)

# Plot the residuals against the fitted values
plot(my.lm$fitted.values, my.lm$residuals, pch=16,
     xlab="Fitted values", ylab="Residuals",
     ylim= 0.6*c(-1, 1))

# Add zero line and lines =/- 2sd
abline(h=0, lty=2)
abline(h=2*resid.sd*c(-1, 1), lty=2, col="red")

# Look at distribution of residuals
hist(my.lm$residuals, probability=T, 
     xlim=0.6*c(-1, 1), main="")
# Compare residuals with normal distribution
qqnorm(my.lm$residuals, main=""); qqline(my.lm$residuals)

x=data.frame(x1 = c(0.4,0.8))
colnames(x) <- "babyplswork"
predict(my.lm, x)


