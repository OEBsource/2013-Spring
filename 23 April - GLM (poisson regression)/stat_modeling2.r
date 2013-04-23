## poisson regression
d <- read.csv("http://goo.gl/ajUBF")
d$x
d$y
d$f

class(d)
class(d$y)
class(d$x)
class(d$f)

summary(d)

## visualize data
plot(d$x, d$y, pch = c(21, 19)[d$f])
legend("topleft", legend = c("C", "T"), pch=c(21, 19))

plot(d$f, d$y)

## GLM

fit <- glm(y ~ x, data=d, family = poisson)
fit

summary(fit)

## assesment of error of estimates
xx <- xx <- seq(-0.2, 3, 0.001)
plot(xx, dnorm(xx, mean=0.07566, sd=0.03560), type="l", xlab="", ylab="")
lines(xx, dnorm(xx, mean=1.29172, sd=0.36369), type="l")
xxx <- xx[xx<0]

## Pr(>|z|) in this case. when 'family = gaussian', t distribution.
segments(xxx, rep(0, length(xxx)), xxx, dnorm(xxx, mean=0.07566, sd=0.03560))
abline(v=0, lty=2)
mtext("beta2", at=0.07566)
mtext("beta1", at=1.29172)

logLik(fit)

## visualize prediction

plot(d$x, d$y, pch = c(21, 19)[d$f])
xx <- seq(min(d$x), max(d$x), length=100)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)

plot(d$x, d$y, pch = c(21, 19)[d$f])
yy <- predict(fit, newdata = data.frame(x = xx), type = "response")
lines(xx, yy, lwd = 2)


## factorial explanatory variable
fit.f <- glm(y ~ f, data = d, family = poisson)
fit.f

logLik(fit.f)

## numerical + factorial explanatory variable

fit.all <- glm(y ~ x + f, data = d, family = poisson)
logLik(fit.all)

## what is link function?
xx <- seq(5, 20, 0.1)
plot(d$x, d$y, pch = c(21, 19)[d$f], xlim=c(5, 20), ylim=c(4, 17))
yyc <- predict(fit.all, newdata = data.frame(x = xx, f="C"), type = "response")
yyt <- predict(fit.all, newdata = data.frame(x = xx, f="T"), type = "response")
lines(xx, yyc, lwd = 2)
lines(xx, yyt, lwd = 2, col="blue")

fit.lm <- glm(y ~ x + f, data = d, family = poisson(link="identity"))
xx <- seq(5, 20, 0.1)
plot(d$x, d$y, pch = c(21, 19)[d$f], xlim=c(5, 20), ylim=c(4, 17))
yylc <- predict(fit.lm, newdata = data.frame(x = xx, f="C"), type = "response")
yylt <- predict(fit.lm, newdata = data.frame(x = xx, f="T"), type = "response")
lines(xx, yylc, lwd = 2)
lines(xx, yylt, lwd = 2, col="blue")

par(mfrow=c(1,2))
plot(d$x, d$y, pch = c(21, 19)[d$f], xlim=c(5, 20), ylim=c(4, 17))
lines(xx, yyc, lwd = 2)
lines(xx, yyt, lwd = 2, col="blue")
plot(d$x, d$y, pch = c(21, 19)[d$f], xlim=c(5, 20), ylim=c(4, 17))
lines(xx, yylc, lwd = 2)
lines(xx, yylt, lwd = 2, col="blue")





