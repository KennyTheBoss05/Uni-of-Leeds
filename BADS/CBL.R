library(dplyr) 
library(ggplot2) 
library(corrgram)
data <- read.csv('USSales.csv') 
head(data)
data$Holiday <- ifelse(data$Holiday == 'Yes' , 1, 0)
for (good in data)
  {
    print(good[['Date']])
    good[1] = as.numeric(substr(good[1], 6, 10)+substr(good[1], 3, 5)+substr(good[1], 0, 2))
    break
  }
#data$Date = as.numeric(data$Date)
#cor(data)
