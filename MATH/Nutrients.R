nutrients=read.csv("http://www1.maths.leeds.ac.uk/~john/3772/nutrients.csv")
attach(nutrients)

#Question 3 : 

req = nutrients[,c("Calcium", "Iron", "Protein", "VitaminA", "VitaminC")]
rec=data.frame(Calcium	= 1000,Iron =	15,Protein	 = 60 ,VitaminA =	800 ,VitaminC =	75)
means = colMeans(req)
matplot(means, pch=c("M"),
        xlab = "Nutrient", ylab = "Mean Value")
#matplot(rec, pch=c("R"),xlab = "Nutrient", ylab = "Recommended Mean Value")

as.matrix(means)
t(as.matrix(rec))
ans = as.matrix(means)/t(as.matrix(rec))
ans

cor(req)

#Question 2 : 
install.packages("DescTools")
library("DescTools")
HotellingsT2Test(data.frame(req$Calcium),mu=1000)

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
#var(req)
clust = data.frame("Calcium" = req$Calcium/max(req$Calcium),"Iron" = req$Iron/max(req$Iron),"Protein" = req$Protein/max(req$Protein),"VitaminA" = req$VitaminA/max(req$VitaminA), "VitaminC" = req$VitaminC/max(req$VitaminC)) 
clustering(clust,"kmeans",k=2)
#var(clust)
#clust2 = data.frame("Calcium" = req$Calcium/max(rec$Calcium),"Iron" = req$Iron/max(rec$Iron),"Protein" = req$Protein/max(rec$Protein),"VitaminA" = req$VitaminA/max(rec$VitaminA), "VitaminC" = req$VitaminC/max(rec$VitaminC))  
#var(clust)[3,3]/var(clust)[4,4]
#var(clust2)[4,4]/var(clust2)[3,3]

#Question 4 : List of foods that contribute to x amount of nutrient would help so that people can consume a certain food for how much ever amount of a nutrient they need

#Question 1 :

#https://rkabacoff.github.io/datavis/Univariate.html#histogram

plot (req$Calcium,req$Protein)
install.packages("ggplot2")
library(ggplot2)
data(req, package = "mosaicData")
boxplot(req$Calcium)
hist(req$Calcium)
hist(req$Iron)
hist(req$Protein)
hist(req$VitaminA)
hist(req$VitaminC)
boxplot(req$VitaminC)

cor(req)
pairs(cor(req))
?heatmap


ggplot(req, 
       aes(x = Calcium, y = Iron)) +
  geom_point()

plot(req$Calcium,req$Iron)

#ggplot(req, aes(x = Calcium, y = Iron)) +
#  geom_line()

ggplot(req, aes(x = Calcium, y = Iron)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")
