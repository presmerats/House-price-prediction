####################################################################
# Machine Learning Project
# Pau Rodriguez

# Ridge & Lasso Regression
# 08/05/2018
####################################################################


# 0. loading--------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())
load("data-preprocessed.Rdata")
objects()
summary(data.new)
dim(data.new)


names(data.new)

# 1. Test and Train sets-----------------------------------

#shuffling the data
set.seed (1714)
data.new <- data.new[sample.int(nrow(data.new)),]

# 30% test, 70% train
i <- round(nrow(data.new)*0.3)
test <- data.new[1:i,]
train <- data.new[(i+1):nrow(data.new),]


# 2. Ridge regression -------------------------------------

summary(train)
names(train)


library(MASS)
model.ridge <- lm.ridge(price ~ ., data=train[,-60], lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

(lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)])

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train)[-9], pos=4, col=colors)

## So we refit our final ridge regression model using the best lambda

model.ridgereg.FINAL <- lm.ridge(price ~ ., data=train[,-60], lambda = lambda.ridge)

(beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))


# 3. Lasso regression ...................................................

## Recall that in the LASSO, the coefficients are penalized by the L1 norm. The 
# optimal value for lambda is again chosen by cross-validation

library(lars)

t <- as.numeric(train[,1])
x <- as.matrix(train[,c(2:59,61:63)])

model.lasso <- lars(x, t, type="lasso")

lambda.lasso <- c(model.lasso$lambda,0)

beta.lasso <- coef(model.lasso)

colors <- rainbow(8)

# It may help visualization if you plot using the scaled X data

beta.scale <- attr(model.lasso$beta, "scaled:scale")
beta.rescaled <- beta.lasso
for(j in 1:9) beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta.lasso)), col=colors)
text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)

## suppose we decide to choose this value
abline(v=lambda.lasso[7], lty=2)
abline(h=0, lty=7)

(beta.lasso <- beta.lasso[7,])

# 4. Ridge with glmnet...............................................

# glmnet package

library(glmnet)
t <- train$price
x <- as.matrix(train[,c(-1,-60)])
# recommended setup
x = model.matrix(price~.,train[,c(-1,-60)])[,-1]
t = train$price
# fit
ridge.mod = glmnet(x,t, alpha=0, lambda=seq(0,10,0.1))
set.seed(17)
cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1))
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam # 5.1

cv.out=cv.glmnet(x,t,alpha=0, lambda=seq(0,10,0.1), nfolds=nrow(x))
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam # 3.3

cv.out=cv.glmnet(x,t,alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam # 3.3

# NRMSE
N <- nrow(x)
t <- train$price
x <- as.matrix(train[,c(-1,-60)])

model.ridge.final <- glmnet(x,t, alpha=0, lambda=3.3  ) 
(beta.ridgereg.FINAL <- coef(model.ridge.final))

(mean.square.error6 <- sum((t - beta.ridgereg.FINAL[1] - as.matrix(x)%*%beta.ridgereg.FINAL[-1])^2)/N)
(norm.root.mse6 <- sqrt(mean.square.error6/((N-1)*var(train$price)))) # 0.06703982 good enough

# 5. Lasso with glmnet...............................................

# Lasso with the glmnet package
library(glmnet)
# recommended setup
x = model.matrix(price~.,train)[,c(-1,-60)]
t = train$price
# fit
lasso.mod = glmnet(x,t, alpha=1, lambda=seq(0,10,0.1))
plot(lasso.mod)
set.seed(17)
cv.out=cv.glmnet(x,t,alpha=1, lambda=seq(0,10,0.1))
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam # 2.7  which is close to the manual computation

N <- nrow(x)
model.lasso.final <- glmnet(x,t, alpha=1, lambda=2.7  ) 
(beta.lasso.FINAL <- coef(model.lasso.final))

(mean.square.error6 <- sum((t - beta.lasso.FINAL[1] - as.matrix(x)%*%beta.lasso.FINAL[-1])^2)/N)
(norm.root.mse6 <- sqrt(mean.square.error6/((N-1)*var(train$price)))) # 0


# this is not legal, overfitting to the test set, just for verification

t.new <- test$price
x <- as.matrix(test[,c(-1,-60)])
xdf <- test[,c(-1,-60)]
N.test <- nrow(x)


(pred.ridgereg <- sum((t.new - beta.ridgereg.FINAL[1] - x %*%beta.ridgereg.FINAL[-1])^2)/N.test)
(nrmse.ridgereg <- sqrt(pred.ridgereg/((N-1)*var(test$price))))
# 0.004288853 


(pred.lasso <- sum((t.new - predict(model.lasso.final, newx=x, s=2.7))^2)/N.test)
(nrmse.lasso <- sqrt(pred.lasso/((N-1)*var(test$price))))
# 0.00428808

