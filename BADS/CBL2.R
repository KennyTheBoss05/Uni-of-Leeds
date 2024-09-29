library(dplyr) 
library(ggplot2) 
library(corrgram)
data <- read.csv('Student_Performance.csv') 
head(data)
data$Extracurricular <- ifelse(data$Extracurricular == 'Yes' , 1, 0)
cor(data)

datasplit <- data
datasplit$id <- 1:nrow(datasplit)
trainingdata <- datasplit %>% sample_n(1600)
testdata <- anti_join(datasplit, trainingdata, by = 'id')
print(dim(trainingdata))
print(dim(testdata))

model <- lm(ExamScore ~ Extracurricular + HoursStudied, data=data)
model_smaller <- lm(ExamScore ~ Extracurricular + HoursStudied, data=data)
summary(model)

modeltrainsplit <- lm(ExamScore ~ Extracurricular + HoursStudied, data=trainingdata)
prediction <- predict(modeltrainsplit, newdata = testdata)
sqrt(mean((testdata$ExamScore - prediction)^2))

dataplot <- data
ggplot(data=dataplot) + geom_point(aes(HoursStudied,ExamScore,color=Extracurricular))+
  labs(title = "Student Performance Plot",
       caption = "Student_Perf",tag = "Figure",x = "HoursStudied",y = "ExamScore")
