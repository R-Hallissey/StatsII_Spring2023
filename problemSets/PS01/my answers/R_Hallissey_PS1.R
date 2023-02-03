# Problem Set 1
# 31-1-2023 / Pluviose 12th CCXXXI
# R Hallissey

# Question 1

#Data and distributions
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
#Test statistic
D <-max(abs(empiricalCDF - pnorm(data)))

?ks.test
ks.test(data, "pnorm")

# Max = maxium value
# abs = absolute positive value
# empirical CDF = observed cumulative distribution function
# pnorm gives the distribution function of (data)(which is 1000 Cauchy variables)
# So JZ's code subtracts the distribution function of the data from its empirical
# cumulative distribution function then takes the maximum absolute value from this,
# which is D, the ks t stat
# dnorm is the gives of a probability distribution


# Kolmogorov - Smirnov Test Function
kst <- function (x) {
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  d <- (max(abs(empiricalCDF - pnorm(x))))
  return(d)
} 
kst(data)

# W/palue
kst <- function (x) {
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  d <- (max(abs(empiricalCDF - pnorm(x))))
  p <- d*sqrt(length(data))
  cat(d, p)
} 
kst(data)

# 
DNorm <- function(x, mean = 0, sd = 1) {
  emp.cdf <- ecdf(x)
  n = length(x)
  df <- data.frame(emp.cdf = emp.cdf(x), pnorm = pnorm(x, mean, sd))
  vec <- (abs((df$emp.cdf - df$pnorm))) 
  res <- max(vec)* sqrt(n)
}

DnNorm <- function(n, mean = 0, sd = 1) {
  x <- sapply(10:n, rnorm, mean, sd)
  res <- sapply(x, DNorm, mean, sd)
}
  
DNorm(data, mean = 0, sd = 1)
DnNorm(n, mean = 0, sd = 1)
y <- DNorm(data, mean = 0, sd = 1)
z <- DnNorm(n, mean = 0, sd = 1)

z

sum(z)
mean(z)
max(z)


res <- D*sqrt(length(data))

1.36 / 1000
res

x <- sapply(10:n, rnorm, mean(data), sd(data))
res <- sapply(x, dnorm, mean(data), sd(data))
max.col(res)

n = 1000

p - D
D - 
res - D


x <- sapply(n, rnorm)
sapply(x, dnorm)

mean(data)


y * 
max(sqrt(z))
