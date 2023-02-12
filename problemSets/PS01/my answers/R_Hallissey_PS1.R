# Problem Set 1
# 12-2-2023 / 12 Frimaire CCXXXII
# Ruairí Hallissey

# Question 1

# Data
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# Kolmogorov - Smirnov Test Function without p-value
kst <- function (x) {
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  d <- (max(abs(empiricalCDF - pnorm(x))))
  return(d)
} 
kst(data)

# 2nd attempt
# I wasn't able to create function which returns the p value
# This function returns the the test stat and the result of.95 critical value divided 
# being divided by by the sqrt of the sample size, which i read was a way to carry out
# the test. Not sure though. 
kst <- function (x) {
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  d <- (max(abs(empiricalCDF - pnorm(x))))
  s <- sqrt(length(data))
  d.a <- 1.35810 / s
  cat(d, d.a)
} 
kst(data)
print("D is greater than than the .95 critical value (n > 50) divided by the sqrt 
      of the samplesize, meaning we can conclude that data is not a good fit for 
      normal distribution")

# Ultimately was not able to find a way of creating a function which returned 
# the same values as the base r function 
print(ks.test(data, "pnorm"))
print("p of 2.2e-16 is less .05, meaning we can reject the null hypthesis that the data is not normally distributed")


# Question 2
# Data
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Likelihood function
lf <- function(theta, y, X){
  n <- nrow(X) 
  k <- ncol(X)  
  beta <- theta[1 : k]  
  sigma_sqr <- theta[k + 1]^2  
  e <- y - X%*%beta
  loglik <- -.5*n*log(2*pi) -.5*n*log(sigma_sqr) - ((t(e) %*% e) / (2 * sigma_sqr))
  return(-loglik)}

# Maximum likelihood parameters
MLE <- optim(fn=lf, par=c(1, 1, 1), hessian =TRUE, y =data$y, X= cbind 
             (1 ,data$x), method = "BFGS")
# Linear Regression 
lm <- lm(y ~ x, data = data)
print(lm$coefficients)

# Comparison
cat(print("Maximum Likilehood Estimate Paramenters"), MLE$par,
    print("and Linear Regression Coefficients"),lm$coefficients)