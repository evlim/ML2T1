rm(list=ls())

library(ISLR)

names(Hitters)
dim(Hitters)

Hitters =na.omit(Hitters)

# 6.6 Lab 2: Ridge Regression and the Lasso
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# 6.6.1 Ridge Regression
library(glmnet)
grid=10^seq(10,-2, length =100)
ridge.mod=glmnet(x,y,alpha=0, lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod ,s=50,type="coefficients")[1:20,]

set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,
                 thresh =1e-12)
ridge.pred=predict(ridge.mod ,s=4, newx=x[test,])
mean((ridge.pred -y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

ridge.pred=predict(ridge.mod ,s=0, newx=x[test,],exact=T, x=x[train,], y=y[train])
mean((ridge.pred -y.test)^2)
lm(y~x, subset=train)
predict (ridge.mod ,s=0,exact=T,x=x[train,], y=y[train] ,type="coefficients")[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[ train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

out=glmnet(x,y,alpha=0)
predict (out ,type="coefficients",s= bestlam)[1:20,]

# 6.6.2 The Lasso
lasso.mod=glmnet(x[train ,],y[ train],alpha=1, lambda =grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)

out=glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out ,type="coefficients",s= bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# 6.7 Lab 3: PCR and PLS Regression
# 6.7.1 Principal Components Regression
library (pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE,
            validation ="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE,
            validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")

pcr.pred=predict(pcr.fit,x[test,],ncomp =7)
mean((pcr.pred -y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# 6.7.2 Partial Least Squares
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE,
             validation ="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

pls.pred=predict (pls.fit ,x[test ,],ncomp =2)
mean((pls.pred -y.test)^2)

pls.fit=plsr(Salary~., data=Hitters, scale=TRUE , ncomp=2)
summary(pls.fit)

