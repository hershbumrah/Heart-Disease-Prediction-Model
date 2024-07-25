library(rpart)
library(rpart.plot)
library(ggplot2)
library(klaR)
library("caret")

#read the data set
data <- read.csv("heart.csv")
set.seed(1259)
#split up the data set into two different sets
indexSet <- sample(2,nrow(data),replace = T, prob = c(0.7,0.3))
train <- data[indexSet==1,]
test <- data[indexSet==2,]

#head(data)
#summary(data)
#colnames(data)

#start of the rpart part
tree <- rpart(
  target~.,
  data=train,
  method = "class",
  control = rpart.control(cp=0.01)
)
#tree

#rpart.plot(tree)

predictions <- predict(tree, newdata = test, type = "class")
rpartTable <- table(values = test$target, predValues = predictions)
caret::confusionMatrix(rpartTable)


#start of naive bayes part
train$target = factor(train$target)
nb <- NaiveBayes(target ~ age+sex+chest.pain.type+resting.bp.s+cholesterol+
                   fasting.blood.sugar+resting.ecg+max.heart.rate+exercise.angina
                 +oldpeak,
                 data = train)

# Make predictions on the test dataset
pred <- predict(nb,test)
naiveTable = table(pred$class, test$target)
caret::confusionMatrix(naiveTable)