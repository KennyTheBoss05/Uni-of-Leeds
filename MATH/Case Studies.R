rm(list = ls())
ls()
library(stringr)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

data = read.csv("Inter3.csv",header=T, fill = TRUE)
data <- as.data.frame(data)
summary(data)
attach(data)
# print(length(data[,1]))
# for (i in 0:length(data))
# {
#   print(AGE[i])
# }

#VITALSTATUS = as.factor(VITALSTATUS)

data = data[data['SURVIVAL_TIME'] >= 0.38*2410,]
data = data[data['AGE'] <= 0.707*104,]
data <- data[order(data$BENCHMARK_GROUP),]

# lis = c('CARBOPLATIN + ETOPOSIDE','CARBOPLATIN + PACLITAXEL','CAPECITABINE + OXALIPLATIN','CYCLOPHOSPHAMIDE + DOXORUBICIN + RITUXIMAB + VINCRISTINE','CYCLOPHOSPHAMIDE + EPIRUBICIN + FLUOROURACIL')
# lis2 = c('FLUOROURACIL + OXALIPLATIN','TRASTUZUMAB','PACLITAXEL','DOCETAXEL','RITUXIMAB')
# data = data[data['BENCHMARK_GROUP'] == lis2[5],]
# sp <- ggplot(data, aes(BENCHMARK_GROUP, AGE))
# sp <- sp + geom_point(aes(color = VITALSTATUS)) +
#   theme(legend.position = "top")
# sp


#print(data)
gg = unique(data$BENCHMARK_GROUP)[0:647]

data = data[data$BENCHMARK_GROUP %in%  gg,]
                          
summary(data)   
print(length(data[data$VITALSTATUS==1,])/length(data[,2]))
# length(data[data$VITALSTATUS==1,])
# length(data[,2])
# length(data[data$VITALSTATUS==0,])
#data[data$VITALSTATUS==1,]

#length(data[,1])
# lis = as.data.frame(table(data$BENCHMARK_GROUP))
# lis = lis[order(lis$Freq),]
# lis[length(lis)-10:length(lis),]

# lis = c('CARBOPLATIN + ETOPOSIDE','CARBOPLATIN + PACLITAXEL','CAPECITABINE + OXALIPLATIN','CYCLOPHOSPHAMIDE + DOXORUBICIN + RITUXIMAB + VINCRISTINE','CYCLOPHOSPHAMIDE + EPIRUBICIN + FLUOROURACIL')
# 
# data = data[data['BENCHMARK_GROUP'] == lis[1],]
# data
