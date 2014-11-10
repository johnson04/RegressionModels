setwd("C:/Users/Chuan/My Study/CourseRA/JHU7 - Regression Models/3 My Quizzes/week 2")

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

n = length(x)
x_mean = mean(x)
y_mean = mean(y)

beta_1 = sum((x-x_mean)*(y-y_mean)) / sum((x-x_mean)*(x-x_mean))
beta_0 = y_mean - beta_1*x_mean
y_pred = beta_0 + beta_1*x

sigma  = sqrt(sum((y-y_pred)^2)/(n-2))
ssx    = sum((x-x_mean)^2)

seBeta0 = sqrt(1/n + x_mean^2/ssx)*sigma
seBeta1 = sigma/sqrt(ssx)

tBeta0 = beta_0 / seBeta0
tBeta1 = beta_1 / seBeta1

pBeta0 = 2*pt(abs(tBeta0), df = n-2, lower.tail=FALSE)
pBeta1 = 2*pt(abs(tBeta1), df = n-2, lower.tail=FALSE)

coefTable <- rbind(c(beta0,seBeta0,tBeta0,pBeta0), c(beta1,seBeta1,tBeta1,pBeta1))

colnames(coefTable) <- c("Estimate","Std.Error","t-value","Pr(>|t|)")

rownames(coefTable) <- c("(Intercept)","x")

coefTable
#              Estimate Std.Error   t-value   Pr(>|t|)
# (Intercept) 0.1884572 0.2061290 0.9142681 0.39098029
# x           0.7224211 0.3106531 2.3254912 0.05296439

fit <- lm(y~x)
beta0 <- coef(fit)[1]
beta1 <- coef(fit)[2]
names(summary(fit))
# [1] "call"          "terms"         "residuals"     "coefficients" 
# [5] "aliased"       "sigma"         "df"            "r.squared"    
# [9] "adj.r.squared" "fstatistic"    "cov.unscaled" 
summary(fit)$coefficients
#              Estimate Std. Error   t value   Pr(>|t|)
# (Intercept) 0.1884572  0.2061290 0.9142681 0.39098029
# x           0.7224211  0.3106531 2.3254912 0.05296439

plot(x,y,frame=FALSE,xlab="x",ylab="y",pcb=21,col="black",bg="lightblue",cex=2)
abline(fit,lwd=2)
xVals <- seq(min(x),max(x),by=.01)
yVals <- beta_0 + beta_1*xVals

se1 <- sigma*sqrt(1/n + (xVals - mean(x))^2/ssx)
se2 <- sigma*sqrt(1 + 1/n + (xVals - mean(x))^2/ssx)

lines(xVals,yVals + 2*se1)
lines(xVals,yVals - 2*se1)
lines(xVals,yVals + 2*se2)
lines(xVals,yVals - 2*se2)

library(UsingR)
data(mtcars)

names(mtcars)
# [1] "mpg"  "cyl"  "disp" "hp"   "drat"
# [6] "wt"   "qsec" "vs"   "am"   "gear"
# [11] "carb"

x <- mtcars$wt
y <- mtcars$mpg

fit <- lm(y ~ x)

coef <- summary(fit)$coefficients
#              Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 37.285126   1.877627 19.857575 8.241799e-19
# x           -5.344472   0.559101 -9.559044 1.293959e-10
Beta0 <- coef[1,1]
Beta1 <- coef[2,1]

n <- length(x)

y_pred <- Beta0 + Beta1 * x
x_mean <- mean(x)

sigma  <- sqrt(sum((y-y_pred)^2)/(n-2))
ssx    <- sum((x-x_mean)^2)

se1 <- sigma*sqrt(1/n + (mean(x) - x_mean)^2/ssx)
se2 <- sigma*sqrt(1 + 1/n + (mean(x) - x_mean)^2/ssx)

## Problems #3
dt1 <- qt(0.975,df=n-2,lower.tail=TRUE)*se1
# [1] 1.099643
x0 <- mean(x)
y0 <- mean(y)
answer3 <- y0 - dt1
# [1] 18.99098

## Problems #4 Skipped

## Problems #5
x0 <- 3 # 3,000 lbs
y0 <- Beta0 + Beta1*x0
# [1] 21.25171
dt2 <- qt(0.975,df=n-2,lower.tail=TRUE)*se2
# [1] 6.316969
answer5 <- y0 + dt2
# [1] 27.56868

## Problems #6
xt <- x/2
fitt <- lm(y ~ xt)

coeft <- summary(fitt)$coefficients
#              Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  37.28513   1.877627 19.857575 8.241799e-19
# xt          -10.68894   1.118202 -9.559044 1.293959e-10
Betat0 <- coeft[1,1]
Betat1 <- coeft[2,1]

nt <- length(xt)

y_predt <- Betat0 + Betat1 * xt
x_meant <- mean(xt)

sigmat <- sqrt(sum((y-y_predt)^2)/(nt-2))
seBetat1 <- sigmat / sqrt(sum((xt-mean(xt))^2))
# [1] 1.118202 = seBeta1*2

answer6 <- Betat1 + c(-1,1)*seBetat1*qt(0.975,df=n-2)
# [1] -12.97262  -8.40527
## So the lower bound of the interval is -12.973

x <- mtcars$wt
y <- mtcars$mpg

sse1 <- sum((y-mean(y))^2)
fit  <- lm(y ~ x)
coef <- summary(fit)$coefficients
beta0 <- coef[1,1]
beta1 <- coef[2,1]
yHat <- beta0 + beta1*x
sse2 <- sum((y-yHat)^2)

ratio <- sse2/sse1
# [1] 0.2471672

