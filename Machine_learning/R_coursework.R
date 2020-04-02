library(dplyr)
library(ggplot2)
library(e1071)
library(class)
setwd("C:/Users/aojgb/OneDrive/Desktop/School Work/Math 501")
orchid <- read.table("orchids.txt")
View(orchid)
attach(orchid)



par(mfrow = c(1,3)) # divides the graph into 3 windows 
boxplot(X1 ~ loc, xlab = "Location", ylab = "Petal length",
        col = c("#f54242", "#d4f542", "#42f56f"))
boxplot(X2 ~ loc, xlab = "Location", ylab = "Leaf width",
        col = c("#f54242", "#d4f542", "#42f56f"))
boxplot(X3 ~ loc, xlab = "Location", ylab = "Petal width",
        col = c("#f54242", "#d4f542", "#42f56f"))

ggplot(orchid, aes(x=X1, y=X2)) +
  geom_point(aes(color= factor(loc)), shape= 1)+
  theme_minimal()+
  labs(title ="Orchids graph", x = "Petal length (mm)", y = "Leaf width (mm)")

aggregate(X1 ~ loc, FUN = mean)
# The mean of the petal length (X1) shows there is difference between the three locations
aggregate(X2 ~ loc, FUN = mean)
# The mean of the leaf width also shows there is a difference between the three locations
aggregate(X3 ~ loc, FUN = mean)
#  The mean of the petal width are very close and cannot be used to differenciate
# between the locations of the data
# From the data provided only the Petal length (X1) and Leaf width (X2) would be
# Reliable to differenciate between the data

# Creating Training data and test data
set.seed(1)
train.x <- cbind(X1, X2)

data.subset <- sample(270, 210)
train.x.sub <- train.x[data.subset, ]
test.x.sub <- train.x[-data.subset, ]
train.y <- loc[data.subset] 
test.y <- loc[-data.subset]

model.train <- orchid[data.subset,]
model.test <- orchid[-data.subset,]
set.seed(1)

# KNN method
library(caret)
set.seed(1)
model.knn <- train(loc~.-X3, 
               data = model.train, 
               method = "knn", 
               trControl = trainControl(method="repeatedcv",repeats = 3),
               preProcess = c("center", "scale"), # Normalize the data
               tuneLength = 20) # Number of possible K values to evaluate
plot(model.knn)
model.knn$bestTune
predict.knn <- model %>% predict(model.test)
mean(predict.knn == model.test$loc)

max(X1)
min(X1)
max(X2)
min(X2)

# Random forest Bagging method
set.seed(1)
library(randomForest)
library(tree)
model.train <- orchid[data.subset,]
model.test <- orchid[-data.subset,]

bag.tree <- randomForest(loc ~ . -X3, data = orchid, subset = data.subset,
                         mtry = 2, importance = TRUE)


round(importance(bag.tree), 2)

varImpPlot(bag.tree)

bag_predict <- predict(bag.tree, model.test, type = "class")


# Support vector machine
set.seed(1)
svmfit = svm(loc ~ X1 + X2, data = model.train, kernel ="linear",
             cost = 10, scale = FALSE )

tune.out = tune(svm, loc ~ X1 + X2, data = model.train,
                kernel ="linear",
                ranges = list(cost = seq(from = 0.01,to = 2, length = 40) ))
plot(tune.out$performances$cost, tune.out$performances$error)
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

ypred_linear = predict(bestmod, model.test)
mean(ypred_linear == model.test$loc)

# Support vector machine polynomial kernels
set.seed(1)
tune.out = tune(svm, loc ~ X1 + X2, data = model.train,
                kernel ="polynomial",
                ranges = list(cost = seq(from = 0.01, to = 2, length = 40),
                              degree = seq(from = 1, to = 5, length = 5) ))
ypred = predict(tune.out$best.model, model.test)
mean(ypred == model.test$loc)
# Which kernal is more suitable

# Calculating Test Errors
mean(predict.knn == model.test$loc) # KNN method

mean(bag_predict == model.test$loc) # Random forest bagging method

mean(ypred_linear == model.test$loc) # Support vector machine Linear

mean(ypred == model.test$loc) # Support vector machine Polynomial kernel
