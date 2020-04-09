library(dplyr)
library(ggplot2)
library(e1071)
library(class)
setwd("C:/Users/aojgb/OneDrive/Desktop/School Work/Math 501")
orchid <- read.table("orchids.txt")
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
train.x <- scale(cbind(X1, X2))

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
               trControl = trainControl(method  = "LOOCV"),
               preProcess = c("center", "scale"), # Normalize the data
               tuneLength = 10) # Number of possible K values to evaluate
plot(model.knn)
model.knn$bestTune
predict.knn <- model.knn %>% predict(model.test)
mean(predict.knn == model.test$loc)

pl = seq(min(model.test$X1), max(model.test$X1), by=0.1)
pw = seq(min(model.test$X2), max(model.test$X2), by=0.1)

mean(X1)
mean(X2)
mean(X3)

# generates the boundaries for the graph
lgrid <- expand.grid(X1=pl, X2=pw, X3=19.73)

knnPredGrid <- predict(model.knn, newdata=lgrid)
knnPredGrid <- model.knn %>% predict(lgrid)
knnPredGrid = as.numeric(knnPredGrid)

predict.knn <- as.numeric(predict.knn)
model.test$loc <- predict.knn

probs <- matrix(knnPredGrid, length(pl), length(pw))

contour(pl, pw, probs, labels="", xlab="Petal length (mm)", ylab="leaf width (mm)", main="K-Nearest Neighbor", axes=T)
gd <- expand.grid(x=pl, y=pw)

points(gd, pch=3, col=probs)

# add the test points to the graph

points(orchid$X1, orchid$X2, col= orchid$loc, cex= 2, pch = 20)



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

pl = seq(min(model.test$X1), max(model.test$X1), by=0.1)
pw = seq(min(model.test$X2), max(model.test$X2), by=0.1)

mean(X1)
mean(X2)
mean(X3)

# generates the boundaries for the graph
lgrid <- expand.grid(X1=pl, X2=pw, X3=19.73)

bagPredGrid <- predict(bag.tree, newdata=lgrid)
bagPredGrid <- bag.tree %>% predict(lgrid)
bagPredGrid = as.numeric(knnPredGrid)

predict.bag <- as.numeric(bag_predict)
model.test$loc <- predict.bag

probs <- matrix(bagPredGrid, length(pl), length(pw))

contour(pl, pw, probs, labels="", xlab="Petal length (mm)", ylab="leaf width (mm)", main="Random forest Bagging method", axes=T)
gd <- expand.grid(x=pl, y=pw)

points(gd, pch=3, col=probs)

# add the test points to the graph

points(orchid$X1, orchid$X2, col= orchid$loc, cex= 2, pch = 20)

######################################################################



# Support vector machine
set.seed(1)
svmfit = svm(as.factor(loc) ~ X1 + X2, data = orchid[data.subset,], kernel ="linear",
             cost = 10, scale = FALSE )

tune.out = tune(svm, loc ~ X1 + X2, data = orchid[data.subset,],
                kernel ="linear",
                ranges = list(cost = seq(from = 0.01,to = 2, length = 40) ))

plot(tune.out$performances$cost, tune.out$performances$error)

summary(tune.out)

bestmod = tune.out$best.model
bestmod
summary(bestmod)
par(mar=c(3,2,1,1))
plot(bestmod, data = orchid[data.subset,], X2~X1)

ypred_linear = predict(bestmod, model.test)

mean(ypred_linear == model.test$loc)

# Support vector machine polynomial kernels
set.seed(1)
svmfit_poly <- svm( as.factor(loc) ~ X1 + X2, 
                    data = orchid[data.subset, ], 
                    kernel = "polynomial", 
                    cost=5, degree=2, 
                    scale = FALSE)
set.seed(1)
tune.out_poly = tune(svm, loc ~ X1 + X2, data = orchid[data.subset,],
                kernel ="polynomial",
                ranges = list(cost = seq(from = 0.01,to = 3, length = 30)))

plot(tune.out_poly$performances$cost, tune.out_poly$performances$error)

summary(tune.out_poly)

bestmod_poly = tune.out_poly$best.model
bestmod_poly
summary(bestmod_poly)

par(mar=c(3,2,1,1))

plot(bestmod_poly, data = model.test, X1~X2)


ypred = predict(bestmod_poly, model.test)

mean(ypred == model.test$loc)


# Which kernal is more suitable

# Calculating Test Errors
mean(predict.knn == model.test$loc) # KNN method

mean(bag_predict == model.test$loc) # Random forest bagging method

mean(ypred_linear == model.test$loc) # Support vector machine Linear

mean(ypred == model.test$loc) # Support vector machine Polynomial kernel

