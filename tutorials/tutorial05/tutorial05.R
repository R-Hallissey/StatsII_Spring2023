#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)
install.packages("nnet")
library(nnet)
install.packages("MASS")
library(MASS)


# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

?polr

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?

summary(workingMoms)

workingMoms$attitude <- factor(workingMoms$attitude,
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Disagree"))

workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                              levels = c(0,1),
                              labels = c("Non-White", "White"))
workingMoms$year <- factor(workingMoms$year, 
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

mod_1 <- polr(workingMoms$attitude ~ ., data = workingMoms, Hess = T)
summary(mod_1)

# Probabilities
(exp(mod_1$coefficients)) / (1 + (exp(mod_1$coefficients)))

# P values
ctable <- coef(summary(mod_1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2

# CI
ci <- confint(mod_1)

# Odds Transformatiom

# set reference

# Multinational


# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.


# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?