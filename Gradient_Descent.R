###: Gradient Descent on dataset from Stanford ML course ('ex1data1')

df <- read.csv('ex1data1.txt')
n <- length(X)
ones <- rep(1,n)
df <- cbind(df,ones)
colnames(df) <- c("Population","Profit","Ones")

###: Test
lm.fit <- lm(Profit ~ Population)
summary(lm.fit) # Bo: -4.21150 B1: 1.21355
plot(Population,Profit)
abline(lm.fit, col = "red")

X <- as.matrix(df[,c(1,3)])
y <- as.matrix(df[,2])
iterations <- 15000
alpha <- 0.01
theta <- matrix(c(0,0))

costFunctionJ <- function(X, y, theta) {
  m = length(y)
  error <- sum((as.matrix(X) %*% theta) - as.matrix(y)) / (2*m)
  return(error)
}

linreg <- function(X, y, theta, iterations) {
  m <- length(y)
  for(iter in 1:iterations) {
    error <- X %*% theta - y
    theta <- theta - ((alpha/m) * t(t(error) %*% X))
  }
  return(theta)
}

theta <- linreg(X, y, theta, iterations)
print(theta[,1])
###: Bo: -4.2115, B1: 1.21354
