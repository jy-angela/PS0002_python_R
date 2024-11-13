grade<-read.csv("gradamit.csv",header=TRUE,sep=",")
str(grade)

#we can see that only gpa is numeric
grade<-grade[complete.cases(grade),]

#题目中有要求rank and admit are both categorical variables 
#we then convert the variable rank and the variable admit into categorical variables
grade$admit<-factor(grade$admit)
grade$rank<-factor(grade$rank)

str(grade)

set.seed(101)
training.idx <- sample(1: nrow(grade), size=nrow(grade)*0.8)
train.data  <-grade[training.idx, ]
test.data <- grade[-training.idx, ]

mlogit<-glm(admit~.,data=train.data,family="binomial")
summary(mlogit)
exp(coef(mlogit))
#we can see that for one unit increase of gpa increase, the odds of being admitted double 

###we can then make predictions
pred.p<-predict(mlogit,newdata=test.data,type="response")
y_pred_num<-ifelse(pred.p>0.5,1,0)
y_pred <-factor(y_pred_num, levels=c(0, 1))
mean(y_pred ==test.data$admit )
table(y_pred,test.data$admit)








