library(MASS)
set.seed(666)

var1  <- 1
var2  <- 1
rho   <- 0.8
covar <- rho*var1*var2

Sigma <- matrix(data=c(var1,covar,covar,var2), nrow=2, ncol=2)
y     <- mvrnorm(n=50, mu=c(1,1.25), Sigma=Sigma)

t.test(x=y[,1], y=y[,2], paired=FALSE, var.equal=TRUE)
t.test(x=y[,1], y=y[,2], paired=TRUE,  var.equal=TRUE)

y.long <- vec(t(y))          # Turn y into a column
cond   <- rep(c("A","B"),50) # Create a predictor for the two conditions

two.sample.mod <- lm(y.long ~ cond)
summary(two.sample.mod)

subject <- matrix(data=c(seq(1,50),seq(1,50)), nrow=50, ncol=2)
subject <- vec(t(subject))
subject <- as.factor(subject)

paired.mod <- lm(y.long ~ cond + subject)
summary(paired.mod)
