library(mlbench)
library(dplyr)

data(PimaIndiansDiabetes,package="mlbench")
?PimaIndiansDiabetes
str(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)

diab<-PimaIndiansDiabetes%>%mutate(y=factor(ifelse(diabetes=="pos",1,0)))%>%select(-diabetes)

str(diab)

#####perform the normalization
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
diab[,1:8]<-sapply(diab[,1:8],nor)
str(diab)
######split the data
set.seed(100)
training.idx<-sample(1:nrow(diab),size=nrow(diab)*0.8)
train.data<-diab[training.idx,]
test.data<-diab[-training.idx,]

#######logistic regression
mlogit<-glm(y~.,data=train.data,family="binomial")
summary(mlogit)
#Five predictors: pregnant,glucose,pressure,mass,pedigree are significant at 0.05.

###make predictions for logistic regression
pred.p<-predict(mlogit,newdata=test.data,type="response")
y_pred_num<-ifelse(pred.p>0.5,1,0)
y_pred<-factor(y_pred_num,levels=c(0,1))
mean(y_pred==test.data$y)
table(y_pred,test.data$y)

#we can see that the accuracy can be computed by (87+30)/(87+30+11+26)=0.75974
###we can see that, the false positive rate is 11/(11+87)
###the false negative rate is 26/(26+30)


#####knn classification
library(class)
ac<-rep(0,30)
for (i in 1:30){
  set.seed(101)
  knn.i<-knn(train.data[,-9],test.data[,-9],cl=train.data$y,k=i)
  ac[i]<-mean(knn.i==test.data$y)
  cat("k=",i,"accuracy=",ac[i],"\n")
}
###accuracy plot
plot(ac,type="b",xlab="K",ylab="Accuracy")
#k=9 with highest accuracy=0.7727273
#use the value of k=9
set.seed(101)
knn2<-knn(train.data[,-9],test.data[,-9],cl=train.data$y,k=9)
mean(knn2==test.data$y)
table(pred_knn2=knn2,actual=test.data$y)
#Accuracy is slightly higher than that of logistic regression
# false positive rate=8/(90+8)=8.2%, smaller than that of LR
# false negative rate=27/(27+29)=48.2%  slightly bigger than that of LR

######perform the SVM

library(e1071)
m.svm<-svm(y~., data = train.data, kernel = "linear")
summary(m.svm)
# predict
pred.svm <- predict(m.svm, newdata=test.data[,1:8])

# Check accuracy:
table(predict=pred.svm, actual=test.data$y)
#         actual
#predict  0  1
#      0 85 24
#      1 13 32
#misclassification rates: 
# false positive rate=13/(85+13)=13.3%, bigger than those of LR and kNN
# false negative rate=24/(24+32)=42.9%  smaller than both LR and kNN

mean(pred.svm ==test.data$y)
#0.7597403 #same as the accuracy in LR classification but with different false positive rate

#Try radial kernel and tune its parameters 
set.seed(123) #tune requires random splitting for cross validation so set a seed
m.svm.tune1<-tune.svm(y~., data=train.data, kernel="radial", cost=10^(-1:2), 
                      gamma=c(.1,.5,1,2))
summary(m.svm.tune1)
#classification performance
best.svm <- m.svm.tune1$best.model
pred.svm.tune <- predict(best.svm, newdata=test.data[,1:8])
table(pred.svm.tune, test.data$y)
mean(pred.svm.tune ==test.data$y)












