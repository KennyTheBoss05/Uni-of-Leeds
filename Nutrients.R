nutrients=read.csv("http://www1.maths.leeds.ac.uk/~john/3772/nutrients.csv")
attach(nutrients)


req = nutrients[,c("Calcium", "Iron", "Protein", "VitaminA", "VitaminC")]
rec=data.frame(Calcium	= 1000,Iron =	15,Protein	 = 60 ,VitaminA =	800 ,VitaminC =	75)


#Question 1 :

summary(req)

plot (req$Calcium,req$Protein,xlab = "Calcium",ylab ="Protein" )
#install.packages("ggplot2")
library(ggplot2)

hist(req$Calcium,xlab = "Calcium", col = "lightgreen", border = 129,density = 100)
hist(req$Iron)
hist(req$Protein)
hist(req$VitaminA)
hist(req$VitaminC)
boxplot(req$Calcium)
boxplot(req$Iron)
boxplot(req$Protein)
boxplot(req$VitaminA,col = "lightgreen", border = 129,density = 100,outline = FALSE,xlab = "Vitamin A")
boxplot(req$VitaminC)
var(req)
cov(req)
cor(req)
forvis = req[req["VitaminA"]<7000,]
forvis
summary(forvis)
pairs(forvis)

ggplot(req, aes(x = Protein, y = Iron)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")

ggplot(forvis, aes(x = VitaminA, y = VitaminC)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")

heatmap(cor(req))
cor(req)

#Question 2 : 
#install.packages("DescTools")
library("DescTools")
mu0 = c(1000, 15, 60,800,75)
xbar = colMeans(req)
rbind(xbar, mu0)
xbar/mu0
xbar
S = var(req)
S
R = cor(req)
xmm = xbar - mu0
xmm
tsq = 737 * t(xmm) %*% solve(S) %*% xmm
tsq
fstat = tsq * (737-5)/(5*736)
pf(fstat, df1 = 5, df2 = 732, lower.tail=F)
#Which is less than 0.5 percent
#So we reject the null hypothesis
#Meaning the women arent taking the right amount of nutrients

#SCI
t5 = qf(0.05, df1 = 5, df2 = 732, lower=F)*736*5/732
con = sqrt((1/737)*S[1,1]*t5)
c(xbar[1]-con,xbar[1]+con)
rec[1]
con = sqrt((1/737)*S[2,2]*t5)
c(xbar[2]-con,xbar[2]+con)
rec[2]
con = sqrt((1/737)*S[3,3]*t5)
c(xbar[3]-con,xbar[3]+con)
rec[3]
con = sqrt((1/737)*S[4,4]*t5)
c(xbar[4]-con,xbar[4]+con)
rec[4]
con = sqrt((1/737)*S[5,5]*t5)
c(xbar[5]-con,xbar[5]+con)
rec[5]


#Question 3 : 

means = colMeans(req)
matplot(means,pch = "*",
        xlab = "Nutrient", ylab = "Mean Value")
req3 = mapply("/",req,rec)
req3
colMeans(req3)
A = matrix(c(1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1),nrow=4,byrow=T)
A
req3 = req3%*%t(A)

as.matrix(means)
t(as.matrix(rec))
ans = as.matrix(means)/t(as.matrix(rec))
ans
mu0 = c(0,0,0,0)
xbar = as.matrix(colMeans(req3),nrow=4)
xbar
S = var(req3)
R = cor(req3)
R
xmm = xbar - mu0
xmm
tsq = 737 * t(xmm) %*% solve(S) %*% xmm
tsq
fstat = tsq * (737-4)/(4*736)
pf(fstat, df1 = 4, df2 = 733, lower.tail=F)
#Which is lesser than 0.5 percent
#So we reject the null hypothesis
#Meaning that everyone consumes a food with different proportions of nutrients

#SCI
t5 = qf(0.05, df1 = 4, df2 = 733, lower=F)*736*4/733
con = sqrt((1/737)*S[1,1]*t5)
c(xbar[1]-con,xbar[1]+con)
mu0[1]
con = sqrt((1/737)*S[2,2]*t5)
c(xbar[2]-con,xbar[2]+con)
mu0[2]
con = sqrt((1/737)*S[3,3]*t5)
c(xbar[3]-con,xbar[3]+con)
mu0[3]
con = sqrt((1/737)*S[4,4]*t5)
c(xbar[4]-con,xbar[4]+con)
mu0[4]

#Question 5 : 

# This file defines the local function clustering
# for use in Math 5772, Exercise Sheet 2
# updated 29 October 2015 to use multiple starts in kmeans

library(mclust)
cluster.descriptions=function(x,x.cl) {
  #  x (n by p) = data
  #  x.cl n-vector of cluster labels
  k=max(x.cl) # assumes cluster labels range from 1:k
  p=ncol(x)
  for(i in 1:k){
    cat("Cluster", i, "consists of\n")
    print(names(x.cl[x.cl==i]))
  }
  means=matrix(0,k,p)
  for(i in 1:k) means[i,]=apply(x[x.cl==i,,drop=FALSE],2,mean)
  cat("cluster means: rows=clusters; columns=variables\n")
  print(means)
  means
}

clustering=function(x,method,k=0) {
  # general clustering function
  x=as.matrix(x)
  if(k==0 &method !="mixture") return(cat("clustering failed; needs a value for k
\n"))
  if(method=="single"| method=="complete" | method=="average") {
    hc=hclust(dist(x),method=method); x.cl=cutree(hc,k)
  }
  if(method=="kmeans") x.cl=kmeans(x,k,nstart=100)$cluster
  if(method=="mixture") {
    x.mix=Mclust(x)
    x.cl=x.mix$classification
    print.Mclust(x.mix)
  }
  pairs(x,col=x.cl,pch=x.cl)
  means=cluster.descriptions(x,x.cl)
  k=max(x.cl); count=rep(0,k)
  for(i in 1:k) count[i]=sum(x.cl==i)
  list(labels=x.cl, means=means,count=count)  
}

clust = data.frame("Calcium" = req$Calcium/max(req$Calcium),"Iron" = req$Iron/max(req$Iron),"Protein" = req$Protein/max(req$Protein),"VitaminA" = req$VitaminA/max(req$VitaminA), "VitaminC" = req$VitaminC/max(req$VitaminC)) 
done = clustering(clust,"kmeans",k=2)
clust1 = clust[done$labels==1,]
clust2 = clust[done$labels==2,]
clust1
clust2

cor(clust1)
cor(clust2)

hist(clust1$VitaminC,xlab = "Cluster 1 Vitamin C", col = "lightgreen", border = 129,density = 100)
hist(clust2$VitaminC,xlab = "Cluster 2 Vitamin C", col = "lightgreen", border = 129,density = 100)

