# This was used to be a playground for logistic regression. Feel free to do anything here.

x1 <- c(1,2,3,4,5,6,7)
x2 <- rnorm(7)

y <- exp(2+3*x1+5*x2)/(1+exp(2+3*x1+5*x2))

logistic.fit <- glm(y ~ x1+x2, family = binomial(link="logit"))
logistic.fit

x1 <- c(1,2,3,4,5,6,7)
x2 <- rnorm(7)

y <- rep(c(exp(1)/(1+exp(1))),7)

logistic.fit <- glm(y ~ x1+x2, family = binomial(link="logit"))
logistic.fit

x1 <- c(1,2,3,4,5,6,7)
x2 <- rnorm(7)

y <- c(1,0,1,1,1,0,0)

logistic.fit <- glm(y ~ x1+x2, family = binomial(link="logit"))
logistic.fit



x1 <- c(1,2,3,444,555,666)
x2 <- seq(min(x1)-1,max(x1)+1,0.01)

y <- c(1,1,1,0,0,0)

logistic.fit <- glm(y ~ x1, family = binomial(link="logit"))
coef <- logistic.fit$coefficients
coef

logreg <- function(z){
	exp(z)/(1+exp(z))
}

plot(x1,y,pch=19,cex=1.5,col='red')
points(x2,logreg(coef[1]+coef[2]*x2),pch=19,type='l',col='blue')