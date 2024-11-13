#####lab 5
library(caret)
library(dplyr)
library(nycflights13)
library(ggplot2)

head(flights)

#prepare data
flights<-mutate(flights, gain=arr_delay-dep_delay)
flights.ua<-flights%>%filter(carrier=="UA",air_time>550)%>%select(gain,air_time)
#inspect data
flights.ua
summary(flights.ua)
#visualize the relationship
ggplot(flights.ua, aes(x=air_time, y=gain))+geom_point()+geom_smooth()
# Split the data into training and test sets
set.seed(100)
training.idx <- sample(1: nrow(flights.ua), size=nrow(flights.ua)*0.8)
train.data  <- flights.ua[training.idx, ]
test.data <- flights.ua[-training.idx, ]

############1. kNN regression ############
# Fit the model on the training set
set.seed(101)
model.knn <- train(
  gain~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 6),
  preProcess = c("center","scale"),
  tuneLength = 10
)
# Plot model error RMSE vs different values of k
plot(model.knn)
#then, we need to find the best k that minimize the RMSE
#As we known that RMSE is the square root of average difference between observed known outcome values y=(y1,y2,y3...yn) and
#the corresponding predicted values y^
#hence, the lower the RMSE, the better fit of the model

model.knn$bestTune
#make predictions on the testdata
predictions<-predict(model.knn,test.data)
head(predictions)
RMSE(predictions,test.data$gain)

##compare y and predicted y
plot(test.data$gain,predictions)

#plot the reference line x=y
abline(0,1, col="red") #intercept 为0，slope为1



####for the same question，use the linear regression method
lmodel<-lm(gain~.,data=train.data)
summary(lmodel)

#then, we make predictions on the test data
predictions<-predict(lmodel,test.data)
RMSE(predictions,test.data$gain)

#we can compare the outcome y and the predicted y
plot(test.data$gain,predictions)
abline(0,1,col="blue")