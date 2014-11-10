# Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autoloader as the outcome (variable name use). Fit a logistic regression model with autoloader (variable auto) use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). Give the estimated odds ratio for autoloader use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator).

library(MASS)
data(shuttle)

fitMod <- glm(use ~ wind, data=shuttle, family="binomial")
summary(fitMod)
# Call:
#     glm(formula = use ~ wind, family = "binomial", data = shuttle)
# 
# Deviance Residuals: 
#     Min      1Q  Median      3Q     Max  
# -1.073  -1.073  -1.060   1.286   1.300  
# 
# Coefficients:
#     Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.25131    0.17817  -1.410    0.158
# windtail    -0.03181    0.25224  -0.126    0.900
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 350.36  on 255  degrees of freedom
# Residual deviance: 350.35  on 254  degrees of freedom
# AIC: 354.35
# 
# Number of Fisher Scoring iterations: 4

exp(fitMod$coef)
# (Intercept)    windtail 
#   0.7777778   0.9686888 

plot(shuttle$wind, shuttle$use, pch =19, col="gray", xlab="wind", ylab="use")

# If you fit a logistic regression model to a binary variable, for example use of the autolander, then fit a logistic regression model for one minus the outcome (not using the autolander) what happens to the coefficients?

newUse <- relevel(shuttle$use, "noauto")
fitMod <- glm(newUse ~ wind, data=shuttle, family="binomial")
summary(fitMod)
# Call:
#     glm(formula = newUse ~ wind, family = "binomial", data = shuttle)
# 
# Deviance Residuals: 
#     Min      1Q  Median      3Q     Max  
# -1.300  -1.286   1.060   1.073   1.073  
# 
# Coefficients:
#     Estimate Std. Error z value Pr(>|z|)
# (Intercept)  0.25131    0.17817   1.410    0.158
# windtail     0.03181    0.25224   0.126    0.900
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 350.36  on 255  degrees of freedom
# Residual deviance: 350.35  on 254  degrees of freedom
# AIC: 354.35
# 
# Number of Fisher Scoring iterations: 4

## So the answer is: reverse their signs

# Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).

rm(list=ls())
data(InsectSprays)

newSpray <- relevel(InsectSprays$spray,"B")
fitMod <- glm(count ~ newSpray, data=InsectSprays, family="poisson")
summary(fitMod)
# Call:
#     glm(formula = count ~ newSpray, family = "poisson", data = InsectSprays)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.3852  -0.8876  -0.1482   0.6063   2.6922  
# 
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    2.73003    0.07372  37.032  < 2e-16 ***
#     newSprayA   -0.05588    0.10574  -0.528    0.597    
#     newSprayC   -1.99606    0.21315  -9.364  < 2e-16 ***
#     newSprayD   -1.13740    0.14961  -7.602 2.91e-14 ***
#     newSprayE   -1.47727    0.17101  -8.638  < 2e-16 ***
#     newSprayF    0.08338    0.10215   0.816    0.414    
# ---
#     Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 409.041  on 71  degrees of freedom
# Residual deviance:  98.329  on 66  degrees of freedom
# AIC: 376.59
# 
# Number of Fisher Scoring iterations: 5

exp(fitMod$coef)
# (Intercept)   newSprayA   newSprayC   newSprayD   newSprayE   newSprayF 
#  15.3333333   0.9456522   0.1358696   0.3206522   0.2282609   1.0869565 

# Consider the data

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

# Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0. Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?

knots <- 0
splineTerms <- sapply(knots, function(knot)(x > knot)*(x-knot))
xMat <- cbind(1,x,splineTerms)
fitMod <- lm(y ~ xMat - 1)
yhat <- predict(fitMod)
plot(x,y,frame=FALSE, pch=21, bg="lightblue", cex=2)
lines(x, yhat, col = "red", lwd = 2)
summary(fitMod)
# Call:
#     lm(formula = y ~ xMat - 1)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.32158 -0.10979  0.01595  0.14065  0.26258 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# xMat  -0.18258    0.13558  -1.347    0.215    
# xMatx -1.02416    0.04805 -21.313 2.47e-08 ***
# xMat   2.03723    0.08575  23.759 1.05e-08 ***
# ---
# Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.2276 on 8 degrees of freedom
# Multiple R-squared:  0.996,    Adjusted R-squared:  0.9945 
# F-statistic:   665 on 3 and 8 DF,  p-value: 6.253e-10

## As it is relative slope, so we have that after 0, the slope is: 
## 2.03723 - 1.02416 = 1.01307

