library(datarium)
data(marketing,package="datarium")
summary(marketing)
head(marketing)
str(marketing)


#visualize the relationship among variables by an appropriate graph
library(psych)

pairs.panels(marketing,method="pearson",hist.col="steelblue",
               pch=21, density=TRUE,ellipses=FALSE)
#using the result, we can see that there is relatively strong linear relationship between youtube and sales, followed by 
#facebook and sales
#also, we know that the pearson correlation coefficient measure how strong the linear relationship between variables

library(corrplot)
corrplot(cor(marketing),type="upper",method="color",addCoef.col = "black",number.cex = 0.6)

###2. split the data into training and test sets
set.seed(101)
training.idx<-sample(1:nrow(marketing),size=nrow(marketing)*0.8)
train.data<-marketing[training.idx,]
test.data<-marketing[-training.idx,]


####apply the knn
set.seed(101)
library(caret)
model.knn<-train(
  sales~.,data=train.data,method="knn",
  trControl=trainControl("cv",number=4),
  preProcess=c("center","scale"),
  tuneLength=10
)

summary(model.knn)
plot(model.knn)
model$bestTune
#hence,k=5 is the best knn value that minimize the RMSE
predictions<-predict(model.knn,test.data)
RMSE(predictions,test.data$sales)
##then, we also need to visualize the results
plot(test.data$sales, predictions,main="Prediction performance of kNN regression")
abline(0,1,col="red")


#########we then perform the linear regression
lmodel<-lm(sales~.,data=train.data)
summary(lmodel)
####from the graph, we can see that let Y^=sales,
#Y^=3.399214+0.0468983X1+0.1834598X2-0.0006319X3
#we can check the significance level, we can see that newpaper is not significant 
#then, the multiple r-squared is higher, it's better fit to the line. 
#Hence, with 3 predictors in linear regression, it's able to explain about 89% of variation in sales

predictions1<-predict(lmodel,test.data)
RMSE(predictions1,test.data$sales)
plot(test.data$sales, predictions1, main="Prediction performance of linear regression")
abline(0,1, col="red")

#we can see that even though currently, linear regression is relatively accurate, we can see that knn is better than linearregression


###then, we can check the residuals
par(mfrow=c(2,2))
plot(lmodel)
#the residual plot shows that there is an outlying point 131, and a quadratic pattern so that the second order predictors are needed
#from pairs.panel plot, we can clearly see that both youtube and facebook are highly related to sales, then we add square terms of both 
#predictors in the regression
lmodel.2<-lm(sales~youtube+facebook+newspaper+I(youtube^2)+I(facebook^2),data=train.data)
summary(lmodel.2)
predictions3<-predict(lmodel.2,test.data)
RMSE(predictions3,test.data$sales)

lmodel.3<-lm(sales~youtube+facebook+newspaper+I(youtube^2)+I(facebook^2)+I(youtube*facebook),data=train.data)
summary(lmodel.3)
predictions4<-predict(lmodel.3,test.data)
RMSE(predictions4,test.data$sales)

#we can see that r-sq increases to 0.98 and the RMSE decreases much a lot compared to previous 
plot(test.data$sales, predictions4, main="Prediction performance of linear regression")
abline(0,1, col="red")
#all predicted values are very close to the true values of sales in the test set.

#we check then just the residual plots again for the improved linear regression model
par(mfrow=c(2,2))
plot(lmodel.3)

#from the residsual plots, we can see that the plot shows a horizontal line at 0, hence there is no clear quadratic pattern
#the model fitting is good enough even though the outlier still exists 
#also, we can choose this linear regression model as final model for prediction due to very small RMSE
#it can be seen from the graph that advertising on youtube and facebook have significant impacts on sales 







