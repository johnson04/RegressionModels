rm(list=ls())
cat("\014")

# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.

data(mtcars)

fit1<-lm(mpg ~ factor(cyl) + wt, data=mtcars)
mycol=rainbow(8)
plot(mtcars$wt, mtcars$mpg, pch=19, col=mycol[mtcars$cyl])
abline(c(fit1$coeff[1],fit1$coeff[4]),col="red",lwd=3)
abline(c(fit1$coeff[1] + fit1$coeff[2] ,fit1$coeff[4]),col="blue",lwd=3)
abline(c(fit1$coeff[1] + fit1$coeff[3] ,fit1$coeff[4]),col="black",lwd=3)
fit2$coeff
# (Intercept) factor(cyl)6 factor(cyl)8           wt 
#   33.990794    -4.255582    -6.070860    -3.205613 

## As the third coefficient is the intercept value relative to that of the line associated to 4 cylinder cars. So the answer is: -6.070860

rm(list=ls())
cat("\014")

# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Compare the adjusted by weight effect of 8 cylinders as compared to 4 the unadjusted. What can be said about the effect?.

fit1<-lm(mpg ~ factor(cyl) + wt, data=mtcars)
mycol=rainbow(8)
plot(mtcars$wt, mtcars$mpg, pch=19, col=mycol[mtcars$cyl])
abline(c(fit1$coeff[1],fit1$coeff[4]),col="red",lwd=3)
abline(c(fit1$coeff[1] + fit1$coeff[2] ,fit1$coeff[4]),col="blue",lwd=3)
abline(c(fit1$coeff[1] + fit1$coeff[3] ,fit1$coeff[4]),col="black",lwd=3)
fit2$coeff
# (Intercept) factor(cyl)6 factor(cyl)8           wt 
#   33.990794    -4.255582    -6.070860    -3.205613 

mean_mpg <- aggregate(mtcars$mpg, list(mtcars$cyl), mean)
#   Group.1        x
# 1       4 26.66364
# 2       6 19.74286
# 3       8 15.10000
mean_mpg[3,2] - mean_mpg[1,2]
# [1] -11.56364
# The absolute value of this number is larger than 6.070860. Therefore, the correct answer is choice A: "Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded."

rm(list=ls())
cat("\014")

# Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder. Consider the model with an interaction between cylinders and weight and one without. Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.

data(mtcars)
mycol = rainbow(max(mtcars$cyl))
plot(mtcars$mpg ~ mtcars$wt, pch=19, col=mycol[mtcars$cyl])

fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit1$coeff
# (Intercept) factor(cyl)6 factor(cyl)8           wt 
#   33.990794    -4.255582    -6.070860    -3.205613 

fit2 <- lm(mpg ~ factor(cyl) * wt, data = mtcars)
fit2$coeff
# (Intercept)    factor(cyl)6    factor(cyl)8              wt factor(cyl)6:wt 
#   39.571196      -11.162351      -15.703167       -5.647025        2.866919 
# factor(cyl)8:wt 
#        3.454587 

abline(c(fit2$coeff[1], fit2$coeff[4]),lwd='3')
abline(c(fit2$coeff[1] + fit2$coeff[2] ,fit2$coeff[4]),col="blue",lwd=3)
abline(c(fit2$coeff[1] + fit2$coeff[3] ,fit2$coeff[4]),col="black",lwd=3)
anova(fit1,fit2)
# Analysis of Variance Table
# 
# Model 1: mpg ~ factor(cyl) + wt
# Model 2: mpg ~ factor(cyl) * wt
#
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     28 183.06                           
# 2     26 155.89  2     27.17 2.2658 0.1239

## Clearly, the p-value is larger than 0.05, meaning that the difference between the two models is due to chance. So the interaction term is not necessary.

rm(list=ls())
cat("\014")

## Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight inlcuded in the model as
## lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
## How is the wt coefficient interpretted?

lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# Call:
#     lm(formula = mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# 
# Coefficients:
#     (Intercept)   I(wt * 0.5)  factor(cyl)6  factor(cyl)8  
#          33.991        -6.411        -4.256        -6.071  

rm(list=ls())
cat("\014")

## Consider the following data set
## x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
## y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
## Give the hat diagonal for the most influential point

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

plot(x,y,type='n')
points(x,y,pch=19)

HatMatrix <- x%*%ginv(t(x)%*%x)%*%t(x)
#               [,1]          [,2]          [,3]          [,4]         [,5]
# [1,]  0.0024864289  7.043468e-04 -1.782082e-04 -0.0026052344  0.049728578
# [2,]  0.0007043468  1.995248e-04 -5.048219e-05 -0.0007380015  0.014086935
# [3,] -0.0001782082 -5.048219e-05  1.277260e-05  0.0001867233 -0.003564164
# [4,] -0.0026052344 -7.380015e-04  1.867233e-04  0.0027297166 -0.052104687
# [5,]  0.0497285779  1.408694e-02 -3.564164e-03 -0.0521046874  0.994571557

Leverage <- diag(HatMatrix)
# [1] 0.0024864289 0.0001995248 0.0000127726 0.0027297166 0.9945715571

## So the hat diagonal for the most influential point is: 0.9945715571

fit <- lm(y~x)
influence.measures(fit)
# Influence measures of
# lm(formula = y ~ x) :
#     
#     dfb.1_     dfb.x     dffit cov.r   cook.d   hat inf
# 1   1.0621 -3.78e-01    1.0679 0.341 2.93e-01 0.229   *
# 2   0.0675 -2.86e-02    0.0675 2.934 3.39e-03 0.244    
# 3  -0.0174  7.92e-03   -0.0174 3.007 2.26e-04 0.253   *
# 4  -1.2496  6.73e-01   -1.2557 0.342 3.91e-01 0.280   *
# 5   0.2043 -1.34e+02 -149.7204 0.107 2.70e+02 0.995   *

## This result also shows that the hat value for the most influential point is: 0.995

rm(list=ls())
cat("\014")

## Consider the following data set
## x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
## y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
## Give the slope dfbeta for the point with the highest hat value.

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y ~ x)
influence.measures(fit)
# Influence measures of
# lm(formula = y ~ x) :
#     
#    dfb.1_     dfb.x     dffit cov.r   cook.d   hat inf
# 1  1.0621 -3.78e-01    1.0679 0.341 2.93e-01 0.229   *
# 2  0.0675 -2.86e-02    0.0675 2.934 3.39e-03 0.244    
# 3 -0.0174  7.92e-03   -0.0174 3.007 2.26e-04 0.253   *
# 4 -1.2496  6.73e-01   -1.2557 0.342 3.91e-01 0.280   *
# 5  0.2043 -1.34e+02 -149.7204 0.107 2.70e+02 0.995   *

## Clearly, the answer is: -134

