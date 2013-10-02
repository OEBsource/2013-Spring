## Modeling using Poisson distribution
#load(url('http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/distribution/data.RData'))
load(url('http://goo.gl/HvMiO'))

plant.seed <- data
length(plant.seed)

summary(plant.seed)

plant.seed <- data
length(plant.seed)

summary(plant.seed)

hist(plant.seed, breaks=seq(-0.5, 9.5, 1))

mean(plant.seed)

var(plant.seed)

sd(plant.seed)

sqrt(var(plant.seed))

## mean ~= variance, so poisson distribution OK

## plot poisson distribution

y <- 0:9
prob <- dpois(y, lambda = 3.56)
plot(y, prob, type = "b", lty = 2)
#Adding in comments

## see data and poisson distribution

hist(plant.seed, breaks = seq(-0.5, 9.5, 1))
points(y, prob * 50)
lines(y, prob * 50, lty = 2)

## log likelihood values for different lambda

logL <- function(m) sum(dpois(data, m, log = TRUE))

plot.poisson <- function(lambda) {
    y <- 0:9
    prob <- dpois(y, lambda = lambda)

    hist(plant.seed, breaks = seq(-0.5, 9.5, 1), ylim = c(0, 15),
         main = "", xlab = "", ylab = "")
    points(y, prob * 50)
    lines(y,  prob * 50, lty = 2)

    title(sprintf("lambda= %.1f\n logL= %.1f", lambda, logL(lambda)))
}

layout(matrix(1:9, byrow = T, ncol = 3))
junk <- sapply(seq(2, 5.2, 0.4), plot.poisson)

## see the shape of likelihood function

logL <- function(m) sum(dpois(data, m, log = TRUE))

lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type = "b")


## GLM

# maximum likelihood estimate and sample mean
fit.poisson <- glm(plant.seed ~ 1, family = poisson)
exp(coef(fit.poisson))

mean(plant.seed)
