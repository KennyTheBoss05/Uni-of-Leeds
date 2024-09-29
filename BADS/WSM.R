#install.packages("./MCDA_0.1.0.zip", repos = NULL, type = "win.binary")
#install.packages(c('Rglpk','triangle','plyr','ggplot2','glpkAPI','combinat'))
#install.packages('MCDA')
library('MCDA')
data <- read.csv('Robot_Info.csv')
performanceTable <- data[,c(1,3,4,5,6)]
weights = data[,2]
performanceTable <- data.frame(t(performanceTable))
colnames(performanceTable) <- performanceTable[1,]
performanceTable <- performanceTable[2:5,]
performanceTable <- sapply(performanceTable,as.numeric)
performanceTable
performanceTable[,4] <- performanceTable[,4]^-1
performanceTable <- performanceTable/colSums(performanceTable)[col(performanceTable)]
performanceTable
weights = as.matrix(weights)
performanceTable = as.matrix(performanceTable)
ans = performanceTable %*% weights
ans = data.frame('Ans' = ans,'Botnames' = colnames(data)[3:6])
ans

