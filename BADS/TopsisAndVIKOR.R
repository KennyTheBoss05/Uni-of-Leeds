#install.packages("./MCDA_0.1.0.zip", repos = NULL, type = "win.binary")
#install.packages(c('Rglpk','triangle','plyr','ggplot2','glpkAPI','combinat'))

#install.packages('MCDA')
library('MCDA')

data = data.frame(t(read.csv('Robot_Info.csv')))
data
pt = data[c(3,4,5,6),c(1:5)]
pt = sapply(pt,as.numeric)
row.names(pt) <- c("Archer","Bowler","Corner","Deviant")
colnames(pt) <- unlist(data[1,1:5])
weights = unlist(data[2,1:5])
weights = sapply(weights,as.numeric)
criteriaMinMax <- c("max", "max", "max","min","max")
names(criteriaMinMax) <- colnames(pt)
verall1 <- TOPSIS(pt, weights, criteriaMinMax)
verall1
verall2 = VIKOR(pt, weights, criteriaMinMax, v=0.5)
verall2 #Final answer

#Sensitivity Analysis
#Pt 1 (Changing v)
VIKOR(pt, weights, criteriaMinMax, v=0.75)
VIKOR(pt, weights, criteriaMinMax, v=0.25)
VIKOR(pt, weights, criteriaMinMax, v=1)
VIKOR(pt, weights, criteriaMinMax, v=0)
for (i in 25:0)
{
  if (VIKOR(pt, weights, criteriaMinMax, v=i*10^-2)["Deviant"]>VIKOR(pt, weights, criteriaMinMax, v=i*10^-2)["Bowler"] || VIKOR(pt, weights, criteriaMinMax, v=i*10^-2)["Deviant"] > VIKOR(pt, weights, criteriaMinMax, v=i*10^-2)["Corner"])
  {
    print(VIKOR(pt, weights, criteriaMinMax, v=i*10^-2))
    print(i*10^-2)
  }
}
VIKOR(pt, weights, criteriaMinMax, v=0.1) #Damn

#Pt 1 (Changing weights)
weights = c(0.06666667, 0.20000000, 0.13333333, 0.26666667, 0.33333333 ) #Original
VIKOR(pt, weights, criteriaMinMax, v=0.5)
weights = c(0.014880952,
            0.241071429,
            0.05952381,
            0.282738095,
            0.401785714
)
VIKOR(pt, weights, criteriaMinMax, v=0.5)
weights = c(0.035714286,
            0.214285714,
            0.071428571,
            0.321428571,
            0.357142857
)
VIKOR(pt, weights, criteriaMinMax, v=0.5)
weights = c(0.130718954,
            0.166666667,
            0.163398693,
            0.261437908,
            0.277777778
)
VIKOR(pt, weights, criteriaMinMax, v=0.5)
weights = c(0.123809524,
            0.171428571,
            0.152380952,
            0.266666667,
            0.285714286
)
VIKOR(pt, weights, criteriaMinMax, v=0.5)
