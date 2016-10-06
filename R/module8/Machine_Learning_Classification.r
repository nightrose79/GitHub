
library(dplyr)
library(magrittr)

olives <- readRDS("small_data/olives.RDS")

olives %>% head

olives %>% group_by(Region, Area) %>% summarise(count=n())

library(caret)

split <- createDataPartition(y=olives$Region,
                             p=0.80,
                             list=FALSE)

train <- olives[split,]
test <- olives[-split,]

train %>% head

# obtain missclassificaton, given a confusion matrix
missclassified <- function(t){
    x =  (sum(t)-sum(diag(t)))/sum(t)
    round(x, 3)
}

library(MASS)

# features: all except Region,Area
lda <- lda(train$Region ~ ., data=train[,3:10])

lda

train%>%names

# quite good!
plot(lda)

# training error
train_predict <- predict(lda, train[,3:10])

(t <- table(train$Region, train_predict$class))

missclassified(t)

# testing error
test_predict <- predict(lda, test[,3:10])

(t <- table(test$Region, test_predict$class))

missclassified(t)

# features: all except Region,Area
qda <- qda(train$Region ~ ., data=train[,3:10])

qda

# training error
train_predict <- predict(qda, train[,3:10])

(t <- table(train$Region, train_predict$class))

missclassified(t)

# training error
test_predict <- predict(qda, test[,3:10])

(t <- table(test$Region, test_predict$class))

missclassified(t)

library(nnet)

region2_train <- relevel(as.factor(train$Region), ref="3")
region2_test  <- relevel(as.factor(test$Region), ref="3")

mn <- multinom(region2_train ~ ., train[,c(3:10)])

mn %>% summary

## error-training (see below for alternative)
(t  <- table(region2_train, predict(mn)))

missclassified(t)

## error-testing
(t  <- table(region2_test, predict(mn, test[,c(3:10)])))

missclassified(t)





library(class)

t <- err_cv <- err_train  <- err_test <- 0

# number of K's for KNN
kk <- 50

# cross-validation error
for(i in 1:kk){
  knns <- knn.cv(olives[,-c(1,2)], cl=olives$Region, k=i, prob=TRUE)
  t <- table(olives$Region, knns)
  err_cv[i] <- missclassified(t)
}

# plot training errors
plot(err_cv,type='l')  

## test
for(i in 1:kk){
  knns <- knn(train[,-c(1,2)], test[,-c(1,2)],
              cl=train$Region, k=i,prob=TRUE)
  t <- table(test$Region,knns)
  err_test[i] <- missclassified(t)
}

## train
## training error should be =0 when K=1
for(i in 1:kk){
  knns <- knn(train[,-c(1,2)], olives[,-c(1,2)],
              cl=train$Region, k=i, prob=TRUE)
  t <- table(olives$Region,knns)
  err_train[i] <- missclassified(t)
}

## plot ALL errors
err_all <- as.data.frame(c(err_cv, err_test, err_train))
err_all$index <- as.factor(rep(c(1:kk),3))
err_all$error <- as.factor(c(rep("cv",length(err_cv)),
                             rep("test",length(err_test)),
                             rep("train",length(err_train))))

names(err_all) <- c("val","index","error")
#err_all[c(1:10,90:120,190:205,299,300),]

qplot(index,val,data=err_all,geom="line", group=error, colour=error)+
  scale_x_discrete(breaks=seq(0,50,5))+labs(x="K",y="error value")

# with ggplot
ggplot(data=err_all, aes(x=index,y=val)) +
    geom_point(size=2,aes(shape=error)) +
    geom_line(aes(group=error)) + 
    scale_x_discrete(breaks=seq(0,50,5)) +
    labs(x="K",y="error value")

# other choices: library(kernlab) which is the SVM in caret [svmLinear()]
library(e1071)

# important to specify type of outcome for classification or regression
svm <- svm(factor(train$Area) ~ ., data=train[,3:10], type="C", kernel="radial")

svm %>% summary

# training error
train_predict  <- predict(svm, train[, 3:10], decision.values = TRUE)

# round predicted values to convert them to original class labels
(t <- table(train$Area, train_predict))

missclassified(t)

# testing error
test_predict <- predict(svm, test[,3:10], decision.values = TRUE)

(t <- table(test$Area, test_predict))

missclassified(t)

library(rpart)          # trees
library(randomForest)

tree <- rpart(factor(train$Area) ~ ., data=train[,3:10])

#tree %>% summary

printcp(tree)

plot(tree)
text(tree, cex = 0.7) # cex = text size
title("Area classification tree. cp=0.01")

# training error
train_predict <- predict(tree, train[,3:10], type="class")

(t <- table(train$Area, train_predict))

missclassified(t)

# testing error
test_predict <- predict(tree, test[,3:10], type="class")

(t <- table(test$Area, test_predict))

missclassified(t)

set.seed(9991)
# using olives dataset, with subset=split
rf <- randomForest(factor(olives$Area) ~., data=olives, subset = split,
                   ntree=1000,mtry=3,
                   importance=TRUE)

# includes confusion matrix
rf

# also appears in the above output
missclassified(rf$confusion[,-10])

# all 8 features were used
varUsed(rf, count = FALSE)

rf$importance
