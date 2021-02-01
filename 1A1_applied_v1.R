rm(list = ls())

library(ISLR)
library(caret)
library(dplyr)

# A 
set.seed(1)
College <- ISLR::College

divideData <- createDataPartition(College$Apps, p=.8,list = F)
train<- College[divideData,]
test<- College[-divideData,]

#preprocessing <- train %>% preProcess(method=c("center","scale"))
#traintransformed <- preprocessing %>% predict(train)
#testtransformed <- preprocessing %>% predict(test)

# B 

linear <- lm(Apps~., data=train)
prediction <- predict(linear, test)
lmerror <- mean((test$Apps-prediction)^2)
lmerror

# C

train_matrix <- model.matrix(Apps~.,data=train)
test_matrix <- model.matrix(Apps~.,data=test)

grid=10^seq(10,-2,length=100)

ridge <- glmnet(train_matrix, train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)

crossv <- cv.glmnet(train_matrix, train$Apps, alpha=0, lambda = grid, thresh = 1e-12)
lambda_ridge <- crossv$lambda.min
lambda_ridge

ridge_prediction <- predict(ridge, s=lambda_ridge, newx = test_matrix)

mean((test$Apps-ridge_prediction)^2)


# D 

lasso <- glmnet(train_matrix, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)

crossv_2 <- cv.glmnet(train_matrix,train$Apps, alpha=1, lambda = grid, thresh = 1e-12)

lambda_lasso <- crossv_2$lambda.min
lambda_lasso

lasso_prediction <- predict(lasso, s=lambda_lasso, newx = test_matrix)

mean((test$Apps-lasso_prediction)^2)


# Coefficients 

predict(lasso,s=lambda_lasso,type="coefficients")


# E

library(pls)
set.seed(1)

pcr <- pcr(Apps~.,data=train,scale=T, validation='CV')
validationplot(pcr,val.type="MSEP")

pcr_prediction <- predict(pcr, test, ncomp = 17)
mean((test$Apps-pcr_prediction)^2)

# F

pls <- plsr(Apps~.,data=train, scale=T, validation='CV')
validationplot(pls,val.type = "MSEP")

pls_prediction <- predict(pls, test, ncomp = 10)
mean((test$Apps-pls_prediction)^2)

# G 

#R^2
test_avg <- mean(test$Apps)
linear_r2 <- 1 - mean((prediction - test$Apps)^2) / mean((test_avg - test$Apps)^2)
#Ridge model
ridge_r2 <- 1 - mean((ridge_prediction - test$Apps)^2) / mean((test_avg - test$Apps)^2)
#Lasso model
lasso_r2 <- 1 - mean((lasso_prediction - test$Apps)^2) / mean((test_avg - test$Apps)^2)
#PCR model
pcr_r2 <- 1 - mean((pcr_prediction - test$Apps)^2) / mean((test_avg - test$Apps)^2)
#PLS model
pls_r2 <- 1 - mean((pls_prediction - test$Apps)^2) / mean((test_avg - test$Apps)^2)

linear_r2
ridge_r2
lasso_r2
pcr_r2
pls_r2
