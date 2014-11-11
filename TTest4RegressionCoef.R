library(UsingR)
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
beta1 <- cor(y,x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1*mean(x)
e <- y - beta0 - beta1*x
sigma <- sqrt(sum(e^2) / (n-2))
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1/n + mean(x)^2/ssx)^.5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = 2 - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std.Error", "t.value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
#              Estimate Std.Error      t.value   P(>|t|)
# (Intercept) -259.6259  17.31886 2.523271e-19 -259.6259
# x           3721.0249  81.78588 4.549715e+01       NaN

fit <- lm(y~x)
summary(fit)$coefficients
#              Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -259.6259   17.31886 -14.99094 2.523271e-19
# x           3721.0249   81.78588  45.49715 6.751260e-40


## Getting a confidence interval
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[1,2]
# [1] -294.4870 -224.7649
sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[2,2]
# [1] 3556.398 3885.651
