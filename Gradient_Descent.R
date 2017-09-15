df <- read.csv('ex1data1.txt')
X <- df[,1]
y <- df[,2]
n <- length(X)
ones <- rep(1,n)
X <- cbind(X,ones)
lm.fit <- lm(y ~ X + ones)
summary(lm.fit)
abline(lm.fit, col = "red")