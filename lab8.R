library(dplyr)
grade<-read.csv("gradamit.csv",header=TRUE)
str(grade)
gd<-grade%>%mutate(gre=as.numeric(gre),rank=as.numeric(rank))
str(gd)
#normalize the numerical values
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
gd[,2:4] <- sapply(gd[,2:4], nor)
str(gd)

gd$admit<-factor(gd$admit)
gd$rank<-factor(gd$rank)

set.seed(100)
training.idx <- sample(1: nrow(gd), size=nrow(gd)*0.8)
train.data  <-gd[training.idx, ]
test.data <- gd[-training.idx, ]

#then, we can perform the KNN classification
library(class)
set.seed(101)
#Find the value of K for the best classifier
ac<-rep(0,30)
for(i in 1:30){
  set.seed(101)
  knn.i<-knn(train.data[,2:4],test.data[,2:4],cl=train.data$admit,k=i)
  ac
}
#Find value of k for the best classfier
ac<-rep(0, 30)
for(i in 1:30){
  set.seed(101)
  knn.i<-knn(train.data[,2:4], test.data[,2:4], cl=train.data$admit, k=i)
  ac[i]<-mean(knn.i ==test.data$admit) 
  cat("k=", i, " accuracy=", ac[i], "\n")
} 

###accuracy plot
plot(ac,type="b",xlab="K",ylab="Accuracy")
set.seed(101)
knn2<-knn(train.data[,2:4],test.data[,2:4],cl=train.data$admit,k=1)
mean(knn2==test.data$admit)
table(knn2,test.data$admit)











