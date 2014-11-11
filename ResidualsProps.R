setwd("C:/Users/Chuan/My Study/CourseRA/JHU7 - Regression Models/1 Lecture Notes and Inclass R Codes/Week 2")

library(UsingR)
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e-(y-yhat)))
# [1] 9.485746e-13
max(abs(e-(y-coef(fit)[1]-coef(fit)[2]*x)))
# [1] 9.485746e-13

## Nonlinear Data
x <- runif(100,-3,3)
y <- x + sin(x) + rnorm(100,sd=.2)
plot(x,y)
abline(lm(y~x))
plot(x,resid(lm(y~x)))
abline(h=0)

## Heteroskedasticity
x <- runif(100,0,6)
y <- x + rnorm(100, mean = 0, sd = .001 * x)
plot(x,y)
abline(lm(y~x))

plot(x,resid(lm(y~x)))
abline(h=0)

## Residual Variation
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y~x)
summary(fit)$sigma
# [1] 31.84052
sqrt(sum(resid(fit)^2) / (n - 2))
# [1] 31.84052
