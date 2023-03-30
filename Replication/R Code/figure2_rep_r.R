rm(list=ls())
library(list)
library(tidyverse)
library(stargazer)
library(Matching)
library(ebal)
library(xtable)
library(MASS)
library(dplyr)
library(tibble)
library(ggplot2)
library(weights)
library(Hmisc)
critical <- abs(qnorm(0.025))
term_dat_cy_p <- read.csv('term_dat_cy_p.csv') 

###############################################################
#### subgroup analysis by Blair and Imai (2012)'s method ####
#### subgroup fig; by gender #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember + 
                     income +
                     pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income +pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income,pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.female.list <- as.matrix(subset(x.list, female == 1))
x.male.list <- as.matrix(subset(x.list, female == 0))
x.female.direct <- as.matrix(subset(x.direct, female == 1))
x.male.direct <- as.matrix(subset(x.direct, female == 0))

female.list <- male.list <- 
  female.direct<- male.direct <-
  female.diff <- male.diff <- rep(NA, n.draws)

set.seed(9798)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.female.list <- logistic(x.female.list %*% par.g)
  
  pred.female.direct <- logistic(x.female.direct %*% draws.direct[d,])
  
  female.list[d] <- mean(pred.female.list)
  female.direct[d] <- mean(pred.female.direct)
  female.diff[d] <- female.list[d] - female.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.male.list <- logistic(x.male.list %*% par.g)
  
  pred.male.direct <- logistic(x.male.direct %*% draws.direct[d,])
  
  male.list[d] <- mean(pred.male.list)
  male.direct[d] <- mean(pred.male.direct)
  male.diff[d] <- male.list[d] - male.direct[d]
  
}



female.list.mean <- mean(female.list)
female.list.se <- sd(female.list)
female.direct.mean <- mean(female.direct)
female.direct.se <- sd(female.direct)
female.diff.mean <- mean(female.diff)
female.diff.se <- sd(female.diff)

male.list.mean <- mean(male.list)
male.list.se <- sd(male.list)
male.direct.mean <- mean(male.direct)
male.direct.se <- sd(male.direct)
male.diff.mean <- mean(male.diff)
male.diff.se <- sd(male.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(male.list.se,
       male.direct.se,
       male.diff.se,
       female.list.se,
       female.direct.se,
       female.diff.se)
c3 = c(male.list.mean, 
       male.direct.mean,
       male.diff.mean,
       female.list.mean,
       female.direct.mean,
       female.diff.mean)
c4 = c(rep('Male',3),rep("Female",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by gender")


#### subgroup fig; by ccpmember #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember + 
                     income +
                     pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income +pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                  income, pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income,pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.ccpmember.list <- as.matrix(subset(x.list, ccpmember == 1))
x.nonccpmember.list <- as.matrix(subset(x.list, ccpmember == 0))
x.ccpmember.direct <- as.matrix(subset(x.direct, ccpmember == 1))
x.nonccpmember.direct <- as.matrix(subset(x.direct, ccpmember == 0))

ccpmember.list <- nonccpmember.list <- 
  ccpmember.direct<- nonccpmember.direct <-
  ccpmember.diff <- nonccpmember.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.ccpmember.list <- logistic(x.ccpmember.list %*% par.g)
  
  pred.ccpmember.direct <- logistic(x.ccpmember.direct %*% draws.direct[d,])
  
  ccpmember.list[d] <- mean(pred.ccpmember.list)
  ccpmember.direct[d] <- mean(pred.ccpmember.direct)
  ccpmember.diff[d] <- ccpmember.list[d] - ccpmember.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonccpmember.list <- logistic(x.nonccpmember.list %*% par.g)
  
  pred.nonccpmember.direct <- logistic(x.nonccpmember.direct %*% draws.direct[d,])
  
  nonccpmember.list[d] <- mean(pred.nonccpmember.list)
  nonccpmember.direct[d] <- mean(pred.nonccpmember.direct)
  nonccpmember.diff[d] <- nonccpmember.list[d] - nonccpmember.direct[d]
  
}



ccpmember.list.mean <- mean(ccpmember.list)
ccpmember.list.se <- sd(ccpmember.list)
ccpmember.direct.mean <- mean(ccpmember.direct)
ccpmember.direct.se <- sd(ccpmember.direct)
ccpmember.diff.mean <- mean(ccpmember.diff)
ccpmember.diff.se <- sd(ccpmember.diff)

nonccpmember.list.mean <- mean(nonccpmember.list)
nonccpmember.list.se <- sd(nonccpmember.list)
nonccpmember.direct.mean <- mean(nonccpmember.direct)
nonccpmember.direct.se <- sd(nonccpmember.direct)
nonccpmember.diff.mean <- mean(nonccpmember.diff)
nonccpmember.diff.se <- sd(nonccpmember.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonccpmember.list.se,
       nonccpmember.direct.se,
       nonccpmember.diff.se,
       ccpmember.list.se,
       ccpmember.direct.se,
       ccpmember.diff.se)
c3 = c(nonccpmember.list.mean, 
       nonccpmember.direct.mean,
       nonccpmember.diff.mean,
       ccpmember.list.mean, 
       ccpmember.direct.mean,
       ccpmember.diff.mean)
c4 = c(rep('Nonmember',3),rep("CCP member",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by party member")



#### subgroup fig; by college #####
# Fit the model 

fit.list <- ictreg(y ~ female + college + agegroup + ccpmember + 
                     income +
                     pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + college + agegroup + ccpmember + 
                    income +pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, college, agegroup, ccpmember,  
                                   income, pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, college, agegroup, ccpmember,
                                     income, pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.college.list <- as.matrix(subset(x.list, college == 1))
x.noncollege.list <- as.matrix(subset(x.list, college == 0))
x.college.direct <- as.matrix(subset(x.direct, college == 1))
x.noncollege.direct <- as.matrix(subset(x.direct, college == 0))

college.list <- noncollege.list <- 
  college.direct<- noncollege.direct <-
  college.diff <- noncollege.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.college.list <- logistic(x.college.list %*% par.g)
  
  pred.college.direct <- logistic(x.college.direct %*% draws.direct[d,])
  
  college.list[d] <- mean(pred.college.list)
  college.direct[d] <- mean(pred.college.direct)
  college.diff[d] <- college.list[d] - college.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.noncollege.list <- logistic(x.noncollege.list %*% par.g)
  
  pred.noncollege.direct <- logistic(x.noncollege.direct %*% draws.direct[d,])
  
  noncollege.list[d] <- mean(pred.noncollege.list)
  noncollege.direct[d] <- mean(pred.noncollege.direct)
  noncollege.diff[d] <- noncollege.list[d] - noncollege.direct[d]
  
}



college.list.mean <- mean(college.list)
college.list.se <- sd(college.list)
college.direct.mean <- mean(college.direct)
college.direct.se <- sd(college.direct)
college.diff.mean <- mean(college.diff)
college.diff.se <- sd(college.diff)

noncollege.list.mean <- mean(noncollege.list)
noncollege.list.se <- sd(noncollege.list)
noncollege.direct.mean <- mean(noncollege.direct)
noncollege.direct.se <- sd(noncollege.direct)
noncollege.diff.mean <- mean(noncollege.diff)
noncollege.diff.se <- sd(noncollege.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(noncollege.list.se,
       noncollege.direct.se,
       noncollege.diff.se,
       college.list.se,
       college.direct.se,
       college.diff.se)
c3 = c(noncollege.list.mean, 
       noncollege.direct.mean,
       noncollege.diff.mean,
       college.list.mean, 
       college.direct.mean,
       college.diff.mean)
c4 = c(rep('Noncollege',3),rep("College",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by educational level")

#### subgroup fig; by young #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + young+ ccpmember +  
                     income +
                     pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + young + ccpmember + 
                    income +pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, young, ccpmember,  
                                   income, pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, young, ccpmember, 
                                     income,pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.young.list <- as.matrix(subset(x.list, young == 1))
x.nonyoung.list <- as.matrix(subset(x.list, young == 0))
x.young.direct <- as.matrix(subset(x.direct, young == 1))
x.nonyoung.direct <- as.matrix(subset(x.direct, young == 0))

young.list <- nonyoung.list <- 
  young.direct<- nonyoung.direct <-
  young.diff <- nonyoung.diff <- rep(NA, n.draws)

set.seed(98985)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.young.list <- logistic(x.young.list %*% par.g)
  
  pred.young.direct <- logistic(x.young.direct %*% draws.direct[d,])
  
  young.list[d] <- mean(pred.young.list)
  young.direct[d] <- mean(pred.young.direct)
  young.diff[d] <- young.list[d] - young.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonyoung.list <- logistic(x.nonyoung.list %*% par.g)
  
  pred.nonyoung.direct <- logistic(x.nonyoung.direct %*% draws.direct[d,])
  
  nonyoung.list[d] <- mean(pred.nonyoung.list)
  nonyoung.direct[d] <- mean(pred.nonyoung.direct)
  nonyoung.diff[d] <- nonyoung.list[d] - nonyoung.direct[d]
  
}



young.list.mean <- mean(young.list)
young.list.se <- sd(young.list)
young.direct.mean <- mean(young.direct)
young.direct.se <- sd(young.direct)
young.diff.mean <- mean(young.diff)
young.diff.se <- sd(young.diff)

nonyoung.list.mean <- mean(nonyoung.list)
nonyoung.list.se <- sd(nonyoung.list)
nonyoung.direct.mean <- mean(nonyoung.direct)
nonyoung.direct.se <- sd(nonyoung.direct)
nonyoung.diff.mean <- mean(nonyoung.diff)
nonyoung.diff.se <- sd(nonyoung.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonyoung.list.se,
       nonyoung.direct.se,
       nonyoung.diff.se,
       young.list.se,
       young.direct.se,
       young.diff.se)
c3 = c(nonyoung.list.mean, 
       nonyoung.direct.mean,
       nonyoung.diff.mean,
       young.list.mean, 
       young.direct.mean,
       young.diff.mean)
c4 = c(rep('Old',3),rep("Young",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) +  
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by age")

#### subgroup fig; by high.income #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember + 
                     + high.income +
                     pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    high.income +pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   high.income, pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     high.income,pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.high.income.list <- as.matrix(subset(x.list, high.income == 1))
x.nonhigh.income.list <- as.matrix(subset(x.list, high.income == 0))
x.high.income.direct <- as.matrix(subset(x.direct, high.income == 1))
x.nonhigh.income.direct <- as.matrix(subset(x.direct, high.income == 0))

high.income.list <- nonhigh.income.list <- 
  high.income.direct<- nonhigh.income.direct <-
  high.income.diff <- nonhigh.income.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.income.list <- logistic(x.high.income.list %*% par.g)
  
  pred.high.income.direct <- logistic(x.high.income.direct %*% draws.direct[d,])
  
  high.income.list[d] <- mean(pred.high.income.list)
  high.income.direct[d] <- mean(pred.high.income.direct)
  high.income.diff[d] <- high.income.list[d] - high.income.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.income.list <- logistic(x.nonhigh.income.list %*% par.g)
  
  pred.nonhigh.income.direct <- logistic(x.nonhigh.income.direct %*% draws.direct[d,])
  
  nonhigh.income.list[d] <- mean(pred.nonhigh.income.list)
  nonhigh.income.direct[d] <- mean(pred.nonhigh.income.direct)
  nonhigh.income.diff[d] <- nonhigh.income.list[d] - nonhigh.income.direct[d]
  
}



high.income.list.mean <- mean(high.income.list)
high.income.list.se <- sd(high.income.list)
high.income.direct.mean <- mean(high.income.direct)
high.income.direct.se <- sd(high.income.direct)
high.income.diff.mean <- mean(high.income.diff)
high.income.diff.se <- sd(high.income.diff)

nonhigh.income.list.mean <- mean(nonhigh.income.list)
nonhigh.income.list.se <- sd(nonhigh.income.list)
nonhigh.income.direct.mean <- mean(nonhigh.income.direct)
nonhigh.income.direct.se <- sd(nonhigh.income.direct)
nonhigh.income.diff.mean <- mean(nonhigh.income.diff)
nonhigh.income.diff.se <- sd(nonhigh.income.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.income.list.se,
       nonhigh.income.direct.se,
       nonhigh.income.diff.se,
       high.income.list.se,
       high.income.direct.se,
       high.income.diff.se)
c3 = c(nonhigh.income.list.mean, 
       nonhigh.income.direct.mean,
       nonhigh.income.diff.mean,
       high.income.list.mean, 
       high.income.direct.mean,
       high.income.diff.mean)
c4 = c(rep('Low income',3),rep("High income",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by income level")

#### subgroup fig; by high.life #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember +  
                     income +
                     pinterest + high.life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income +pinterest + high.life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, pinterest, high.life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income,pinterest, high.life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.high.life.list <- as.matrix(subset(x.list, high.life == 1))
x.nonhigh.life.list <- as.matrix(subset(x.list, high.life == 0))
x.high.life.direct <- as.matrix(subset(x.direct, high.life == 1))
x.nonhigh.life.direct <- as.matrix(subset(x.direct, high.life == 0))

high.life.list <- nonhigh.life.list <- 
  high.life.direct<- nonhigh.life.direct <-
  high.life.diff <- nonhigh.life.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.life.list <- logistic(x.high.life.list %*% par.g)
  
  pred.high.life.direct <- logistic(x.high.life.direct %*% draws.direct[d,])
  
  high.life.list[d] <- mean(pred.high.life.list)
  high.life.direct[d] <- mean(pred.high.life.direct)
  high.life.diff[d] <- high.life.list[d] - high.life.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.life.list <- logistic(x.nonhigh.life.list %*% par.g)
  
  pred.nonhigh.life.direct <- logistic(x.nonhigh.life.direct %*% draws.direct[d,])
  
  nonhigh.life.list[d] <- mean(pred.nonhigh.life.list)
  nonhigh.life.direct[d] <- mean(pred.nonhigh.life.direct)
  nonhigh.life.diff[d] <- nonhigh.life.list[d] - nonhigh.life.direct[d]
  
}



high.life.list.mean <- mean(high.life.list)
high.life.list.se <- sd(high.life.list)
high.life.direct.mean <- mean(high.life.direct)
high.life.direct.se <- sd(high.life.direct)
high.life.diff.mean <- mean(high.life.diff)
high.life.diff.se <- sd(high.life.diff)

nonhigh.life.list.mean <- mean(nonhigh.life.list)
nonhigh.life.list.se <- sd(nonhigh.life.list)
nonhigh.life.direct.mean <- mean(nonhigh.life.direct)
nonhigh.life.direct.se <- sd(nonhigh.life.direct)
nonhigh.life.diff.mean <- mean(nonhigh.life.diff)
nonhigh.life.diff.se <- sd(nonhigh.life.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.life.list.se,
       nonhigh.life.direct.se,
       nonhigh.life.diff.se,
       high.life.list.se,
       high.life.direct.se,
       high.life.diff.se)
c3 = c(nonhigh.life.list.mean, 
       nonhigh.life.direct.mean,
       nonhigh.life.diff.mean,
       high.life.list.mean, 
       high.life.direct.mean,
       high.life.diff.mean)
c4 = c(rep('Low satisfaction',3),rep("High satisfaction",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by life satifaction")

#### subgroup fig; by high.pinterest #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember +   
                     income +
                     high.pinterest + life + self_index
                     + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income + high.pinterest + life + self_index
                    + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, high.pinterest, life, self_index, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income,high.pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list)

x.high.pinterest.list <- as.matrix(subset(x.list, high.pinterest == 1))
x.nonhigh.pinterest.list <- as.matrix(subset(x.list, high.pinterest == 0))
x.high.pinterest.direct <- as.matrix(subset(x.direct, high.pinterest == 1))
x.nonhigh.pinterest.direct <- as.matrix(subset(x.direct, high.pinterest == 0))

high.pinterest.list <- nonhigh.pinterest.list <- 
  high.pinterest.direct<- nonhigh.pinterest.direct <-
  high.pinterest.diff <- nonhigh.pinterest.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.pinterest.list <- logistic(x.high.pinterest.list %*% par.g)
  
  pred.high.pinterest.direct <- logistic(x.high.pinterest.direct %*% draws.direct[d,])
  
  high.pinterest.list[d] <- mean(pred.high.pinterest.list)
  high.pinterest.direct[d] <- mean(pred.high.pinterest.direct)
  high.pinterest.diff[d] <- high.pinterest.list[d] - high.pinterest.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.pinterest.list <- logistic(x.nonhigh.pinterest.list %*% par.g)
  
  pred.nonhigh.pinterest.direct <- logistic(x.nonhigh.pinterest.direct %*% draws.direct[d,])
  
  nonhigh.pinterest.list[d] <- mean(pred.nonhigh.pinterest.list)
  nonhigh.pinterest.direct[d] <- mean(pred.nonhigh.pinterest.direct)
  nonhigh.pinterest.diff[d] <- nonhigh.pinterest.list[d] - nonhigh.pinterest.direct[d]
  
}



high.pinterest.list.mean <- mean(high.pinterest.list)
high.pinterest.list.se <- sd(high.pinterest.list)
high.pinterest.direct.mean <- mean(high.pinterest.direct)
high.pinterest.direct.se <- sd(high.pinterest.direct)
high.pinterest.diff.mean <- mean(high.pinterest.diff)
high.pinterest.diff.se <- sd(high.pinterest.diff)

nonhigh.pinterest.list.mean <- mean(nonhigh.pinterest.list)
nonhigh.pinterest.list.se <- sd(nonhigh.pinterest.list)
nonhigh.pinterest.direct.mean <- mean(nonhigh.pinterest.direct)
nonhigh.pinterest.direct.se <- sd(nonhigh.pinterest.direct)
nonhigh.pinterest.diff.mean <- mean(nonhigh.pinterest.diff)
nonhigh.pinterest.diff.se <- sd(nonhigh.pinterest.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.pinterest.list.se,
       nonhigh.pinterest.direct.se,
       nonhigh.pinterest.diff.se,
       high.pinterest.list.se,
       high.pinterest.direct.se,
       high.pinterest.diff.se)
c3 = c(nonhigh.pinterest.list.mean, 
       nonhigh.pinterest.direct.mean,
       nonhigh.pinterest.diff.mean,
       high.pinterest.list.mean, 
       high.pinterest.direct.mean,
       high.pinterest.diff.mean)
c4 = c(rep('Low interest',3),rep("High interest",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by political interest level")


#### subgroup fig; by china #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember +  
                     income +
                     pinterest + life + self_index
                     + high.china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income +pinterest + life + self_index
                    + high.china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, pinterest, life, self_index,  high.china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income,pinterest, life, self_index, high.china, confucian_all)))

k <- ncol(x.list)

x.high.china.list <- as.matrix(subset(x.list, high.china == 1))
x.nonhigh.china.list <- as.matrix(subset(x.list, high.china == 0))
x.high.china.direct <- as.matrix(subset(x.direct, high.china == 1))
x.nonhigh.china.direct <- as.matrix(subset(x.direct, high.china == 0))

high.china.list <- nonhigh.china.list <- 
  high.china.direct<- nonhigh.china.direct <-
  high.china.diff <- nonhigh.china.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.china.list <- logistic(x.high.china.list %*% par.g)
  
  pred.high.china.direct <- logistic(x.high.china.direct %*% draws.direct[d,])
  
  high.china.list[d] <- mean(pred.high.china.list)
  high.china.direct[d] <- mean(pred.high.china.direct)
  high.china.diff[d] <- high.china.list[d] - high.china.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.china.list <- logistic(x.nonhigh.china.list %*% par.g)
  
  pred.nonhigh.china.direct <- logistic(x.nonhigh.china.direct %*% draws.direct[d,])
  
  nonhigh.china.list[d] <- mean(pred.nonhigh.china.list)
  nonhigh.china.direct[d] <- mean(pred.nonhigh.china.direct)
  nonhigh.china.diff[d] <- nonhigh.china.list[d] - nonhigh.china.direct[d]
  
}



high.china.list.mean <- mean(high.china.list)
high.china.list.se <- sd(high.china.list)
high.china.direct.mean <- mean(high.china.direct)
high.china.direct.se <- sd(high.china.direct)
high.china.diff.mean <- mean(high.china.diff)
high.china.diff.se <- sd(high.china.diff)

nonhigh.china.list.mean <- mean(nonhigh.china.list)
nonhigh.china.list.se <- sd(nonhigh.china.list)
nonhigh.china.direct.mean <- mean(nonhigh.china.direct)
nonhigh.china.direct.se <- sd(nonhigh.china.direct)
nonhigh.china.diff.mean <- mean(nonhigh.china.diff)
nonhigh.china.diff.se <- sd(nonhigh.china.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.china.list.se,
       nonhigh.china.direct.se,
       nonhigh.china.diff.se,
       high.china.list.se,
       high.china.direct.se,
       high.china.diff.se)
c3 = c(nonhigh.china.list.mean, 
       nonhigh.china.direct.mean,
       nonhigh.china.diff.mean,
       high.china.list.mean, 
       high.china.direct.mean,
       high.china.diff.mean)
c4 = c(rep('Low satisfaction',3),rep("High satisfaction",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 10))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by the feeling about China situation")

#### subgroup fig; by confucian_all #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember +  
                     income +
                     pinterest + life + self_index
                     + china + high.confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    income +pinterest + life + self_index
                    + china + high.confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, pinterest, life, self_index, china, high.confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income, pinterest, life, self_index, china, high.confucian_all)))

k <- ncol(x.list)

x.high.confucian_all.list <- as.matrix(subset(x.list, high.confucian_all == 1))
x.nonhigh.confucian_all.list <- as.matrix(subset(x.list, high.confucian_all == 0))
x.high.confucian_all.direct <- as.matrix(subset(x.direct, high.confucian_all == 1))
x.nonhigh.confucian_all.direct <- as.matrix(subset(x.direct, high.confucian_all == 0))

high.confucian_all.list <- nonhigh.confucian_all.list <- 
  high.confucian_all.direct<- nonhigh.confucian_all.direct <-
  high.confucian_all.diff <- nonhigh.confucian_all.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.confucian_all.list <- logistic(x.high.confucian_all.list %*% par.g)
  
  pred.high.confucian_all.direct <- logistic(x.high.confucian_all.direct %*% draws.direct[d,])
  
  high.confucian_all.list[d] <- mean(pred.high.confucian_all.list)
  high.confucian_all.direct[d] <- mean(pred.high.confucian_all.direct)
  high.confucian_all.diff[d] <- high.confucian_all.list[d] - high.confucian_all.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.confucian_all.list <- logistic(x.nonhigh.confucian_all.list %*% par.g)
  
  pred.nonhigh.confucian_all.direct <- logistic(x.nonhigh.confucian_all.direct %*% draws.direct[d,])
  
  nonhigh.confucian_all.list[d] <- mean(pred.nonhigh.confucian_all.list)
  nonhigh.confucian_all.direct[d] <- mean(pred.nonhigh.confucian_all.direct)
  nonhigh.confucian_all.diff[d] <- nonhigh.confucian_all.list[d] - nonhigh.confucian_all.direct[d]
  
}



high.confucian_all.list.mean <- mean(high.confucian_all.list)
high.confucian_all.list.se <- sd(high.confucian_all.list)
high.confucian_all.direct.mean <- mean(high.confucian_all.direct)
high.confucian_all.direct.se <- sd(high.confucian_all.direct)
high.confucian_all.diff.mean <- mean(high.confucian_all.diff)
high.confucian_all.diff.se <- sd(high.confucian_all.diff)

nonhigh.confucian_all.list.mean <- mean(nonhigh.confucian_all.list)
nonhigh.confucian_all.list.se <- sd(nonhigh.confucian_all.list)
nonhigh.confucian_all.direct.mean <- mean(nonhigh.confucian_all.direct)
nonhigh.confucian_all.direct.se <- sd(nonhigh.confucian_all.direct)
nonhigh.confucian_all.diff.mean <- mean(nonhigh.confucian_all.diff)
nonhigh.confucian_all.diff.se <- sd(nonhigh.confucian_all.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.confucian_all.list.se,
       nonhigh.confucian_all.direct.se,
       nonhigh.confucian_all.diff.se,
       high.confucian_all.list.se,
       high.confucian_all.direct.se,
       high.confucian_all.diff.se)
c3 = c(nonhigh.confucian_all.list.mean, 
       nonhigh.confucian_all.direct.mean,
       nonhigh.confucian_all.diff.mean,
       high.confucian_all.list.mean, 
       high.confucian_all.direct.mean,
       high.confucian_all.diff.mean)
c4 = c(rep('Low Confucianism',3),rep("High Confucianism",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 10))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by the level of Confucianism")

#### subgroup fig; by high.self #####
# Fit the model 

fit.list <- ictreg(y ~ female + education + agegroup + ccpmember + 
                     + income +
                     pinterest + life + high.self
                   + china + confucian_all,
                   treat = "treat", 
                   J = 4,
                   data = term_dat_cy_p, 
                   method = "ml",
                   fit.start = "lm")

fit.direct <- glm(termlimit_a_b ~ female + education + agegroup + ccpmember + 
                    + income +pinterest + life + high.self
                  + china + confucian_all, 
                  data = term_dat_cy_p, 
                  family = binomial("logit"))

vcov.list <- vcov(fit.list)
par.list <- coef(fit.list)

vcov.direct <- vcov(fit.direct)
par.direct <- coef(fit.direct)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list <- as.data.frame(with(term_dat_cy_p,
                             cbind(1, female, education, agegroup, ccpmember,  
                                   income, pinterest, life, high.self, china, confucian_all)))

x.direct <- as.data.frame(with(term_dat_cy_p,
                               cbind(1, female, education, agegroup, ccpmember, 
                                     income, pinterest, life, high.self, china, confucian_all)))

k <- ncol(x.list)

x.high.self.list <- as.matrix(subset(x.list, high.self == 1))
x.nonhigh.self.list <- as.matrix(subset(x.list, high.self == 0))
x.high.self.direct <- as.matrix(subset(x.direct, high.self == 1))
x.nonhigh.self.direct <- as.matrix(subset(x.direct, high.self == 0))

high.self.list <- nonhigh.self.list <- 
  high.self.direct<- nonhigh.self.direct <-
  high.self.diff <- nonhigh.self.diff <- rep(NA, n.draws)

set.seed(9799)
draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list [1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list [d, ]
  
  pred.high.self.list <- logistic(x.high.self.list %*% par.g)
  
  pred.high.self.direct <- logistic(x.high.self.direct %*% draws.direct[d,])
  
  high.self.list[d] <- mean(pred.high.self.list)
  high.self.direct[d] <- mean(pred.high.self.direct)
  high.self.diff[d] <- high.self.list[d] - high.self.direct[d]
  
}

draws.list <- mvrnorm(n = n.draws, mu = par.list[1:k], Sigma = vcov.list[1:k,1:k])
draws.direct <- mvrnorm(n = n.draws, mu = par.direct, Sigma = vcov.direct)

for (d in 1:n.draws) {
  
  par.g <- draws.list[d, ]
  
  pred.nonhigh.self.list <- logistic(x.nonhigh.self.list %*% par.g)
  
  pred.nonhigh.self.direct <- logistic(x.nonhigh.self.direct %*% draws.direct[d,])
  
  nonhigh.self.list[d] <- mean(pred.nonhigh.self.list)
  nonhigh.self.direct[d] <- mean(pred.nonhigh.self.direct)
  nonhigh.self.diff[d] <- nonhigh.self.list[d] - nonhigh.self.direct[d]
  
}



high.self.list.mean <- mean(high.self.list)
high.self.list.se <- sd(high.self.list)
high.self.direct.mean <- mean(high.self.direct)
high.self.direct.se <- sd(high.self.direct)
high.self.diff.mean <- mean(high.self.diff)
high.self.diff.se <- sd(high.self.diff)

nonhigh.self.list.mean <- mean(nonhigh.self.list)
nonhigh.self.list.se <- sd(nonhigh.self.list)
nonhigh.self.direct.mean <- mean(nonhigh.self.direct)
nonhigh.self.direct.se <- sd(nonhigh.self.direct)
nonhigh.self.diff.mean <- mean(nonhigh.self.diff)
nonhigh.self.diff.se <- sd(nonhigh.self.diff)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.self.list.se,
       nonhigh.self.direct.se,
       nonhigh.self.diff.se,
       high.self.list.se,
       high.self.direct.se,
       high.self.diff.se)
c3 = c(nonhigh.self.list.mean, 
       nonhigh.self.direct.mean,
       nonhigh.self.diff.mean,
       high.self.list.mean, 
       high.self.direct.mean,
       high.self.diff.mean)
c4 = c(rep('Low income',3),rep("High income",3))
sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name
sum_df$type <- factor(sum_df$type, levels = c('List','Direct','Diff'))

ggplot(sum_df, aes(x=type)) + 
  geom_point(aes(y=mean)) +
  geom_errorbar(aes(ymin=mean-critical*se, ymax=mean+critical*se), width=.05) +
  facet_grid(. ~ sex)+
  xlab('') + 
  ylab('Estimated Proportion') +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Support for term limits removal by self-monitoring")


#### Create the differences figure for gender and ccpmember####
# Create a ggplot data 
col_name = c("category","subgroup", "mean", "se")
c1 = c(rep("Gender", 2), 
       rep("Party member",2),
       rep("Education",2),
       rep("Age", 2),
       rep("Income level",2),
       rep("Life satisfaction",2),
       rep("Political interest", 2),
       rep("China situation", 2),
       rep("Confucianism",2),
       rep("Self-monitoring",2))

c2 = c("Female", "Male",
       "CCP Member", "Nonmember",
       "College", "Noncollege",
       "Young", "Old",
       "High income", "Low income", 
       "High satisfcation", "Low satisfaction",
       "High interest", "Low interest",
       "High satisfaction", "Low satisfaction",
       "High Confucianism","Low Confucianism",
       "High self-monitoring","Low self-monitoring")

c3 = c(female.diff.mean,
       male.diff.mean,
       
       ccpmember.diff.mean,
       nonccpmember.diff.mean,
       
       college.diff.mean,
       noncollege.diff.mean,
       
       young.diff.mean,
       nonyoung.diff.mean,
       
       high.income.diff.mean,
       nonhigh.income.diff.mean,
       
       high.life.diff.mean,
       nonhigh.life.diff.mean,
       
       high.pinterest.diff.mean,
       nonhigh.pinterest.diff.mean,
       
       high.china.diff.mean,
       nonhigh.china.diff.mean,
       
       high.confucian_all.diff.mean,
       nonhigh.confucian_all.diff.mean,
       
       high.self.diff.mean,
       nonhigh.self.diff.mean)

c4 = c(female.diff.se,
       male.diff.se,
       
       ccpmember.diff.se,
       nonccpmember.diff.se,
       
       college.diff.se,
       noncollege.diff.se,
       
       young.diff.se,
       nonyoung.diff.se,
       
       high.income.diff.se,
       nonhigh.income.diff.se,
       
       high.life.diff.se,
       nonhigh.life.diff.se,
       
       high.pinterest.diff.se,
       nonhigh.pinterest.diff.se,
       
       high.china.diff.se,
       nonhigh.china.diff.se,
       
       high.confucian_all.diff.se,
       nonhigh.confucian_all.diff.se,
       
       high.self.diff.se,
       nonhigh.self.diff.se)

sum_df = data.frame(c1,c2,c3,c4)
names(sum_df) = col_name

# generate the figure 
sum_df %>%
  mutate(lower=mean+critical*se,upper=mean-critical*se) %>% 
  mutate(category = fct_relevel(category, "Age", 
                                "Education", 
                                "Gender",
                                "Income level",
                                "Party member",
                                "Life satisfaction",
                                "Political interest",
                                "China situation", 
                                "Confucianism", "self-monitoring"))  %>% 
  ggplot(aes(x=subgroup, y=mean)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,
                position=position_dodge(width=0.5))+
  facet_wrap(~category, ncol = 1, strip.position="right",scale='free_y') +
  ylab('Difference Between List and Direct Estimates (95% CI)')+ 
  xlab('')+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black")+
  coord_flip()+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom",
        strip.text.y  = element_text(size = 7, angle = -90)) 

