
library(dplyr)

wine <- readRDS("small_data/wine.RDS")
inpatient <- readRDS("small_data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014_clean.RDS")

wine %>% head

library(caret)

split <- createDataPartition(y=wine$Alcohol.content,
                             p=0.80,
                             list=FALSE)

train <- wine[split,]
test <- wine[-split,]

lm <- lm(Alcohol.content ~ ., data=train)

# shows significant predictors with asterisks *
# overall fit can be evaluated with (adjusted) R squared
lm %>% summary

# make predictions on testing data
test_pred <- predict(lm, test)
test_pred %>% head

# compute predicted MSE
mean( (test$Alcohol.content-test_pred)**2 )

library(biglm)

# lm()
inpatient %>%
    group_by(DRG.code) %>%
    do(mod = lm(Average.Total.Payments ~ Total.Discharges , data=.)) %>%
    object.size() %>%
    print(unit = "Mb")

# biglm()
inpatient %>%
    group_by(DRG.code) %>%
    do(mod = biglm(Average.Total.Payments ~ Total.Discharges, data=.)) %>%
    object.size() %>%
    print(unit = "Mb")

# create a target y
summary(wine$Alcohol.content)

train2 <- train
train2$y <- ifelse(train$Alcohol.content>13, 1, 0)
train2 <- train2[,-1]

test2 <- test
test2$y <- ifelse(test$Alcohol.content>13, 1, 0)
test2 <- test2[,-1]

# change family to run logistic regression
lr <- glm(y ~ ., data=train2, family = "binomial")

lr %>% summary

predict(lr, newdata=test2, type = "response") %>% head

# confusion matrix
(t <- table(test2$y, round(predict(lr, test2, type = "response"))))

# obtain missclassificaton, given a confusion matrix
missclassified <- function(t){
    x =  (sum(t)-sum(diag(t)))/sum(t)
    round(x, 3)
}

missclassified(t)

#https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)

# obtain training and testing splits
# glmnet() requires input matrices

y_train <- train$Alcohol.content %>% as.matrix
X_train <- train[,-1] %>% as.matrix

y_test <- test$Alcohol.content %>% as.matrix
X_test <- test[,-1] %>% as.matrix

# alpha is for the elastic-net mixing parameter; alpha is in [0, 1]
# alpha=1 (default) is for lasso, alpha=0 is the ridge
# standardize = TRUE is default
# family = gaussian (default)
# family = "binomial" for regularized logistic regression
# also:"multinomial" for multinomial model, "poisson" for Poisson regression,
# "cox" for a survival model

fit  <-  glmnet(X_train, y_train, alpha = 1, standardize = TRUE)

# each curve is a feature (X_i)
# top horizontal axis: number of non-zero coefficients at given lambda
# bottom horizontal axis: lambda (default)
# label=TRUE to label feature curves
plot(fit, xvar = "lambda", label = TRUE)

feature_number <- c(1,9)
names(train[,-1])[feature_number]

fit %>% summary

# df = number of nonzero coefficient
# %dev = the percent (of null) deviance explained
# lambda:  the value of λ
fit

# feature selection
# specify lambda with s (see fit above)
# this will return the "best" features
coef(fit, s=0.3)      # lambda = 0.3 returns 2 "best" features

# predict. can specify (multiple) lambdas with s=
predict(fit, X_test, s=c(0.5, 0.3)) %>% head

# more parameters: type.measure, nfolds
cvfit <- cv.glmnet(X_train, y_train)

?cv.glmnet

# again, features on top axis, and lambda on bottom axis
# MSE is shown, with standard deviation curves
# 2 vertical lines: lamnda.min and lamnbda.lse (see next)
plot(cvfit)

# extract lambda with minimum mean CV error
# this is one of the vertical lines in the plot above
cvfit$lambda.min

# extract lambda from regularized model within 1 s.d. of lambda.min 
# this is the other vertical line in plot above
# 1se = "one s-e" not "L s-e"
cvfit$lambda.1se

# 2 predictors seem to suffice
# compare these coefficients to lm() above
coef(cvfit, s = "lambda.1se")

predict(cvfit, X_test, s="lambda.1se") %>% head
