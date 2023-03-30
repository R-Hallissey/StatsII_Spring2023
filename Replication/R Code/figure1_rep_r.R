#Clean the global environment
rm(list=ls())
library("readxl")
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
library(forcats)
library(weights)
library(Hmisc)
critical <- abs(qnorm(0.025))
trust_dat_cy_p = read_csv("GitHub/StatsII_Spring2023/Replication/Data/trust_dat_cy_p.csv")

#-------------------------------------------------------------------------------------------------
# make the overall trust variables in "direct" dataset binary (b for binary)
trust_dat_cy_p$centraltrust_all_b <- ifelse(trust_dat_cy_p$centraltrust_all==1 | 
                                              trust_dat_cy_p$centraltrust_all==2, 0, 1)
trust_dat_cy_p$localtrust_all_b <- ifelse(trust_dat_cy_p$localtrust_all==1 | 
                                            trust_dat_cy_p$localtrust_all==2, 0, 1)

#### Subset the dataframe (for central) ####
# The subset only contains a (control) and b (central trust)
d.list.central <- trust_dat_cy_p[c("female","education","agegroup","ccpmember", "income",
                             "life","pinterest", "self_index","treat","y",
                             "college", "young", "high.income", "high.life", "high.pinterest",
                             "high.self",
                             "china",  
                             "confucian_all",
                             "high.china", 
                             "high.confucian_all",
                             "centraltrust_all", "localtrust_all")]
d.list.central <- d.list.central[!(d.list.central$treat =="c"),]

# Make a (control) = 0, b (treated) = 1
d.list.central$treat[d.list.central$treat == "a"] = "0"
d.list.central$treat[d.list.central$treat == "b"] = "1"


#-------------------------------------------------------------------------------------------------
#### Subset the dataframe (for local) ####

# The subset only contains a (control) and b (local trust)
d.list.local <- trust_dat_cy_p[c("female","education","agegroup","ccpmember", "income",
                           "life","pinterest", "self_index","treat","y",
                           "college", "young", "high.income", "high.life", "high.pinterest",
                           "china", 
                           "confucian_all",
                           "high.china", 
                           "high.confucian_all",
                           "high.self","centraltrust_all", "localtrust_all")]
d.list.local <- d.list.local[!(d.list.local$treat =="b"),]

# Make a (control) = 0, c (treated) = 1
d.list.local$treat[d.list.local$treat == "a"] = "0"
d.list.local$treat[d.list.local$treat == "c"] = "1"


#-------------------------------------------------------------------------------------------------
#### Create the figures ####
#### subgroup analysis by Imai's method ####
#### subgroup fig; Central by gender #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + 
                             agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + 
                            agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, life, self_index,
                                           china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, 
                                             agegroup, ccpmember, income,
                                             pinterest, life, self_index,
                                             china, confucian_all)))

k <- ncol(x.list.central)

x.female.list.central <- as.matrix(subset(x.list.central, female == 1))
x.male.list.central <- as.matrix(subset(x.list.central, female == 0))
x.female.direct.central <- as.matrix(subset(x.direct.central, female == 1))
x.male.direct.central <- as.matrix(subset(x.direct.central, female == 0))

female.list.central <- male.list.central <- 
  female.direct.central <- male.direct.central<-
  female.diff.central <- male.diff.central <- rep(NA, n.draws)

set.seed(333)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.female.list.central <- logistic(x.female.list.central %*% par.g)
  
  pred.female.direct.central <- logistic(x.female.direct.central %*% draws.direct.central[d,])
  
  female.list.central[d] <- mean(pred.female.list.central)
  female.direct.central[d] <- mean(pred.female.direct.central)
  female.diff.central[d] <- female.list.central[d] - female.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.male.list.central <- logistic(x.male.list.central %*% par.g)
  
  pred.male.direct.central <- logistic(x.male.direct.central %*% draws.direct.central[d,])
  
  male.list.central[d] <- mean(pred.male.list.central)
  male.direct.central[d] <- mean(pred.male.direct.central)
  male.diff.central[d] <- male.list.central[d] - male.direct.central[d]
  
}



female.list.central.mean <- mean(female.list.central)
female.list.central.se <- sd(female.list.central)
female.direct.central.mean <- mean(female.direct.central)
female.direct.central.se <- sd(female.direct.central)
female.diff.central.mean <- mean(female.diff.central)
female.diff.central.se <- sd(female.diff.central)

male.list.central.mean <- mean(male.list.central)
male.list.central.se <- sd(male.list.central)
male.direct.central.mean <- mean(male.direct.central)
male.direct.central.se <- sd(male.direct.central)
male.diff.central.mean <- mean(male.diff.central)
male.diff.central.se <- sd(male.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(male.list.central.se,male.direct.central.se,male.diff.central.se,
       female.list.central.se,female.direct.central.se,female.diff.central.se)
c3 = c(male.list.central.mean, male.direct.central.mean,male.diff.central.mean,
       female.list.central.mean, female.direct.central.mean,female.diff.central.mean)
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
  ggtitle("Trust in central government by gender")

#### subgroup fig; Local by gender #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + 
                           agegroup + ccpmember  + income +
                           pinterest + life + self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          agegroup + ccpmember + income +
                          pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, income,
                                         pinterest, life, self_index,
                                         china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, life, self_index,
                                           china, confucian_all)))

k <- ncol(x.list.local)

x.female.list.local <- as.matrix(subset(x.list.local, female == 1))
x.male.list.local <- as.matrix(subset(x.list.local, female == 0))
x.female.direct.local <- as.matrix(subset(x.direct.local, female == 1))
x.male.direct.local <- as.matrix(subset(x.direct.local, female == 0))

female.list.local <- male.list.local <- 
  female.direct.local <- male.direct.local<-
  female.diff.local <- male.diff.local <- rep(NA, n.draws)

set.seed(59)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.female.list.local <- logistic(x.female.list.local %*% par.g)
  
  pred.female.direct.local <- logistic(x.female.direct.local %*% draws.direct.local[d,])
  
  female.list.local[d] <- mean(pred.female.list.local)
  female.direct.local[d] <- mean(pred.female.direct.local)
  female.diff.local[d] <- female.list.local[d] - female.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.male.list.local <- logistic(x.male.list.local %*% par.g)
  
  pred.male.direct.local <- logistic(x.male.direct.local %*% draws.direct.local[d,])
  
  male.list.local[d] <- mean(pred.male.list.local)
  male.direct.local[d] <- mean(pred.male.direct.local)
  male.diff.local[d] <- male.list.local[d] - male.direct.local[d]
  
}



female.list.local.mean <- mean(female.list.local)
female.list.local.se <- sd(female.list.local)
female.direct.local.mean <- mean(female.direct.local)
female.direct.local.se <- sd(female.direct.local)
female.diff.local.mean <- mean(female.diff.local)
female.diff.local.se <- sd(female.diff.local)

male.list.local.mean <- mean(male.list.local)
male.list.local.se <- sd(male.list.local)
male.direct.local.mean <- mean(male.direct.local)
male.direct.local.se <- sd(male.direct.local)
male.diff.local.mean <- mean(male.diff.local)
male.diff.local.se <- sd(male.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(male.list.local.se,male.direct.local.se,male.diff.local.se,
       female.list.local.se,female.direct.local.se,female.diff.local.se)
c3 = c(male.list.local.mean, male.direct.local.mean,male.diff.local.mean,
       female.list.local.mean, female.direct.local.mean,female.diff.local.mean)
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
  ggtitle("Trust in local governments by gender")

#### subgroup fig; Central by ccpmember #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, income,
                                             pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.ccpmember.list.central <- as.matrix(subset(x.list.central, ccpmember == 1))
x.nonccpmember.list.central <- as.matrix(subset(x.list.central, ccpmember == 0))
x.ccpmember.direct.central <- as.matrix(subset(x.direct.central, ccpmember == 1))
x.nonccpmember.direct.central <- as.matrix(subset(x.direct.central, ccpmember == 0))

ccpmember.list.central <- nonccpmember.list.central <- 
  ccpmember.direct.central <- nonccpmember.direct.central<-
  ccpmember.diff.central <- nonccpmember.diff.central <- rep(NA, n.draws)

set.seed(390)

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.ccpmember.list.central <- logistic(x.ccpmember.list.central %*% par.g)
  
  pred.ccpmember.direct.central <- logistic(x.ccpmember.direct.central %*% draws.direct.central[d,])
  
  ccpmember.list.central[d] <- mean(pred.ccpmember.list.central)
  ccpmember.direct.central[d] <- mean(pred.ccpmember.direct.central)
  ccpmember.diff.central[d] <- ccpmember.list.central[d] - ccpmember.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonccpmember.list.central <- logistic(x.nonccpmember.list.central %*% par.g)
  
  pred.nonccpmember.direct.central <- logistic(x.nonccpmember.direct.central %*% draws.direct.central[d,])
  
  nonccpmember.list.central[d] <- mean(pred.nonccpmember.list.central)
  nonccpmember.direct.central[d] <- mean(pred.nonccpmember.direct.central)
  nonccpmember.diff.central[d] <- nonccpmember.list.central[d] - nonccpmember.direct.central[d]
  
}



ccpmember.list.central.mean <- mean(ccpmember.list.central)
ccpmember.list.central.se <- sd(ccpmember.list.central)
ccpmember.direct.central.mean <- mean(ccpmember.direct.central)
ccpmember.direct.central.se <- sd(ccpmember.direct.central)
ccpmember.diff.central.mean <- mean(ccpmember.diff.central)
ccpmember.diff.central.se <- sd(ccpmember.diff.central)

nonccpmember.list.central.mean <- mean(nonccpmember.list.central)
nonccpmember.list.central.se <- sd(nonccpmember.list.central)
nonccpmember.direct.central.mean <- mean(nonccpmember.direct.central)
nonccpmember.direct.central.se <- sd(nonccpmember.direct.central)
nonccpmember.diff.central.mean <- mean(nonccpmember.diff.central)
nonccpmember.diff.central.se <- sd(nonccpmember.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonccpmember.list.central.se,
       nonccpmember.direct.central.se,
       nonccpmember.diff.central.se,
       ccpmember.list.central.se,
       ccpmember.direct.central.se,
       ccpmember.diff.central.se)
c3 = c(nonccpmember.list.central.mean, 
       nonccpmember.direct.central.mean,
       nonccpmember.diff.central.mean,
       ccpmember.list.central.mean, 
       ccpmember.direct.central.mean,
       ccpmember.diff.central.mean)
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
  ggtitle("Trust in central government by party member")

#### subgroup fig; Local by ccpmember #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education +
                           agegroup + ccpmember  + income +
                           pinterest + life + self_index 
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          agegroup + ccpmember + income +
                          pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, income,
                                         pinterest, life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.ccpmember.list.local <- as.matrix(subset(x.list.local, ccpmember == 1))
x.nonccpmember.list.local <- as.matrix(subset(x.list.local, ccpmember == 0))
x.ccpmember.direct.local <- as.matrix(subset(x.direct.local, ccpmember == 1))
x.nonccpmember.direct.local <- as.matrix(subset(x.direct.local, ccpmember == 0))

ccpmember.list.local <- nonccpmember.list.local <- 
  ccpmember.direct.local <- nonccpmember.direct.local<-
  ccpmember.diff.local <- nonccpmember.diff.local <- rep(NA, n.draws)

set.seed(8989)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.ccpmember.list.local <- logistic(x.ccpmember.list.local %*% par.g)
  
  pred.ccpmember.direct.local <- logistic(x.ccpmember.direct.local %*% draws.direct.local[d,])
  
  ccpmember.list.local[d] <- mean(pred.ccpmember.list.local)
  ccpmember.direct.local[d] <- mean(pred.ccpmember.direct.local)
  ccpmember.diff.local[d] <- ccpmember.list.local[d] - ccpmember.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonccpmember.list.local <- logistic(x.nonccpmember.list.local %*% par.g)
  
  pred.nonccpmember.direct.local <- logistic(x.nonccpmember.direct.local %*% draws.direct.local[d,])
  
  nonccpmember.list.local[d] <- mean(pred.nonccpmember.list.local)
  nonccpmember.direct.local[d] <- mean(pred.nonccpmember.direct.local)
  nonccpmember.diff.local[d] <- nonccpmember.list.local[d] - nonccpmember.direct.local[d]
  
}



ccpmember.list.local.mean <- mean(ccpmember.list.local)
ccpmember.list.local.se <- sd(ccpmember.list.local)
ccpmember.direct.local.mean <- mean(ccpmember.direct.local)
ccpmember.direct.local.se <- sd(ccpmember.direct.local)
ccpmember.diff.local.mean <- mean(ccpmember.diff.local)
ccpmember.diff.local.se <- sd(ccpmember.diff.local)

nonccpmember.list.local.mean <- mean(nonccpmember.list.local)
nonccpmember.list.local.se <- sd(nonccpmember.list.local)
nonccpmember.direct.local.mean <- mean(nonccpmember.direct.local)
nonccpmember.direct.local.se <- sd(nonccpmember.direct.local)
nonccpmember.diff.local.mean <- mean(nonccpmember.diff.local)
nonccpmember.diff.local.se <- sd(nonccpmember.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonccpmember.list.local.se,nonccpmember.direct.local.se,nonccpmember.diff.local.se,
       ccpmember.list.local.se,ccpmember.direct.local.se,ccpmember.diff.local.se)
c3 = c(nonccpmember.list.local.mean, nonccpmember.direct.local.mean,nonccpmember.diff.local.mean,
       ccpmember.list.local.mean, ccpmember.direct.local.mean,ccpmember.diff.local.mean)
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
  ggtitle("Trust in local governments by party member")

#### subgroup fig; Central by college #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)


fit.list.central <- ictreg(y ~ female + college + agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + college + agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, college, agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, college, agegroup, ccpmember, income,
                                             pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.college.list.central <- as.matrix(subset(x.list.central, college == 1))
x.noncollege.list.central <- as.matrix(subset(x.list.central, college == 0))
x.college.direct.central <- as.matrix(subset(x.direct.central, college == 1))
x.noncollege.direct.central <- as.matrix(subset(x.direct.central, college == 0))

college.list.central <- noncollege.list.central <- 
  college.direct.central <- noncollege.direct.central<-
  college.diff.central <- noncollege.diff.central <- rep(NA, n.draws)

set.seed(777)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.college.list.central <- logistic(x.college.list.central %*% par.g)
  
  pred.college.direct.central <- logistic(x.college.direct.central %*% draws.direct.central[d,])
  
  college.list.central[d] <- mean(pred.college.list.central)
  college.direct.central[d] <- mean(pred.college.direct.central)
  college.diff.central[d] <- college.list.central[d] - college.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.noncollege.list.central <- logistic(x.noncollege.list.central %*% par.g)
  
  pred.noncollege.direct.central <- logistic(x.noncollege.direct.central %*% draws.direct.central[d,])
  
  noncollege.list.central[d] <- mean(pred.noncollege.list.central)
  noncollege.direct.central[d] <- mean(pred.noncollege.direct.central)
  noncollege.diff.central[d] <- noncollege.list.central[d] - noncollege.direct.central[d]
  
}



college.list.central.mean <- mean(college.list.central)
college.list.central.se <- sd(college.list.central)
college.direct.central.mean <- mean(college.direct.central)
college.direct.central.se <- sd(college.direct.central)
college.diff.central.mean <- mean(college.diff.central)
college.diff.central.se <- sd(college.diff.central)

noncollege.list.central.mean <- mean(noncollege.list.central)
noncollege.list.central.se <- sd(noncollege.list.central)
noncollege.direct.central.mean <- mean(noncollege.direct.central)
noncollege.direct.central.se <- sd(noncollege.direct.central)
noncollege.diff.central.mean <- mean(noncollege.diff.central)
noncollege.diff.central.se <- sd(noncollege.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(noncollege.list.central.se,
       noncollege.direct.central.se,
       noncollege.diff.central.se,
       college.list.central.se,
       college.direct.central.se,
       college.diff.central.se)
c3 = c(noncollege.list.central.mean, 
       noncollege.direct.central.mean,
       noncollege.diff.central.mean,
       college.list.central.mean, 
       college.direct.central.mean,
       college.diff.central.mean)
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
  ggtitle("Trust in central government by educational level")

#### subgroup fig; Local by college #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)


fit.list.local <- ictreg(y ~ female + college + agegroup + ccpmember  + income +
                           pinterest + life + self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + college + agegroup + ccpmember + income +
                          pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, college, agegroup, ccpmember, income,
                                         pinterest, life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, college, agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.college.list.local <- as.matrix(subset(x.list.local, college == 1))
x.noncollege.list.local <- as.matrix(subset(x.list.local, college == 0))
x.college.direct.local <- as.matrix(subset(x.direct.local, college == 1))
x.noncollege.direct.local <- as.matrix(subset(x.direct.local, college == 0))

college.list.local <- noncollege.list.local <- 
  college.direct.local <- noncollege.direct.local<-
  college.diff.local <- noncollege.diff.local <- rep(NA, n.draws)

set.seed(333)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.college.list.local <- logistic(x.college.list.local %*% par.g)
  
  pred.college.direct.local <- logistic(x.college.direct.local %*% draws.direct.local[d,])
  
  college.list.local[d] <- mean(pred.college.list.local)
  college.direct.local[d] <- mean(pred.college.direct.local)
  college.diff.local[d] <- college.list.local[d] - college.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.noncollege.list.local <- logistic(x.noncollege.list.local %*% par.g)
  
  pred.noncollege.direct.local <- logistic(x.noncollege.direct.local %*% draws.direct.local[d,])
  
  noncollege.list.local[d] <- mean(pred.noncollege.list.local)
  noncollege.direct.local[d] <- mean(pred.noncollege.direct.local)
  noncollege.diff.local[d] <- noncollege.list.local[d] - noncollege.direct.local[d]
  
}



college.list.local.mean <- mean(college.list.local)
college.list.local.se <- sd(college.list.local)
college.direct.local.mean <- mean(college.direct.local)
college.direct.local.se <- sd(college.direct.local)
college.diff.local.mean <- mean(college.diff.local)
college.diff.local.se <- sd(college.diff.local)

noncollege.list.local.mean <- mean(noncollege.list.local)
noncollege.list.local.se <- sd(noncollege.list.local)
noncollege.direct.local.mean <- mean(noncollege.direct.local)
noncollege.direct.local.se <- sd(noncollege.direct.local)
noncollege.diff.local.mean <- mean(noncollege.diff.local)
noncollege.diff.local.se <- sd(noncollege.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(noncollege.list.local.se,
       noncollege.direct.local.se,
       noncollege.diff.local.se,
       college.list.local.se,
       college.direct.local.se,
       college.diff.local.se)
c3 = c(noncollege.list.local.mean, 
       noncollege.direct.local.mean,
       noncollege.diff.local.mean,
       college.list.local.mean, 
       college.direct.local.mean,
       college.diff.local.mean)
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
  ggtitle("Trust in local governments by educational level")

#### subgroup fig; Central by young #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + young + ccpmember  + income +
                             pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + young + ccpmember + income +
                            pinterest + life + self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, young, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, young, ccpmember, income,
                                             pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.young.list.central <- as.matrix(subset(x.list.central, young == 1))
x.nonyoung.list.central <- as.matrix(subset(x.list.central, young == 0))
x.young.direct.central <- as.matrix(subset(x.direct.central, young == 1))
x.nonyoung.direct.central <- as.matrix(subset(x.direct.central, young == 0))

young.list.central <- nonyoung.list.central <- 
  young.direct.central <- nonyoung.direct.central<-
  young.diff.central <- nonyoung.diff.central <- rep(NA, n.draws)

set.seed(314)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.young.list.central <- logistic(x.young.list.central %*% par.g)
  
  pred.young.direct.central <- logistic(x.young.direct.central %*% draws.direct.central[d,])
  
  young.list.central[d] <- mean(pred.young.list.central)
  young.direct.central[d] <- mean(pred.young.direct.central)
  young.diff.central[d] <- young.list.central[d] - young.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonyoung.list.central <- logistic(x.nonyoung.list.central %*% par.g)
  
  pred.nonyoung.direct.central <- logistic(x.nonyoung.direct.central %*% draws.direct.central[d,])
  
  nonyoung.list.central[d] <- mean(pred.nonyoung.list.central)
  nonyoung.direct.central[d] <- mean(pred.nonyoung.direct.central)
  nonyoung.diff.central[d] <- nonyoung.list.central[d] - nonyoung.direct.central[d]
  
}



young.list.central.mean <- mean(young.list.central)
young.list.central.se <- sd(young.list.central)
young.direct.central.mean <- mean(young.direct.central)
young.direct.central.se <- sd(young.direct.central)
young.diff.central.mean <- mean(young.diff.central)
young.diff.central.se <- sd(young.diff.central)

nonyoung.list.central.mean <- mean(nonyoung.list.central)
nonyoung.list.central.se <- sd(nonyoung.list.central)
nonyoung.direct.central.mean <- mean(nonyoung.direct.central)
nonyoung.direct.central.se <- sd(nonyoung.direct.central)
nonyoung.diff.central.mean <- mean(nonyoung.diff.central)
nonyoung.diff.central.se <- sd(nonyoung.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonyoung.list.central.se,
       nonyoung.direct.central.se,
       nonyoung.diff.central.se,
       young.list.central.se,
       young.direct.central.se,
       young.diff.central.se)
c3 = c(nonyoung.list.central.mean, 
       nonyoung.direct.central.mean,
       nonyoung.diff.central.mean,
       young.list.central.mean, 
       young.direct.central.mean,
       young.diff.central.mean)
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
  ggtitle("Trust in central government by age")

#### subgroup fig; Local by young #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + 
                           young + ccpmember  + income +
                           pinterest + life + self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          young + ccpmember + income +
                          pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         young, ccpmember, income,
                                         pinterest, life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           young, ccpmember, income,
                                           pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.young.list.local <- as.matrix(subset(x.list.local, young == 1))
x.nonyoung.list.local <- as.matrix(subset(x.list.local, young == 0))
x.young.direct.local <- as.matrix(subset(x.direct.local, young == 1))
x.nonyoung.direct.local <- as.matrix(subset(x.direct.local, young == 0))

young.list.local <- nonyoung.list.local <- 
  young.direct.local <- nonyoung.direct.local<-
  young.diff.local <- nonyoung.diff.local <- rep(NA, n.draws)

set.seed(3144)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.young.list.local <- logistic(x.young.list.local %*% par.g)
  
  pred.young.direct.local <- logistic(x.young.direct.local %*% draws.direct.local[d,])
  
  young.list.local[d] <- mean(pred.young.list.local)
  young.direct.local[d] <- mean(pred.young.direct.local)
  young.diff.local[d] <- young.list.local[d] - young.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonyoung.list.local <- logistic(x.nonyoung.list.local %*% par.g)
  
  pred.nonyoung.direct.local <- logistic(x.nonyoung.direct.local %*% draws.direct.local[d,])
  
  nonyoung.list.local[d] <- mean(pred.nonyoung.list.local)
  nonyoung.direct.local[d] <- mean(pred.nonyoung.direct.local)
  nonyoung.diff.local[d] <- nonyoung.list.local[d] - nonyoung.direct.local[d]
  
}



young.list.local.mean <- mean(young.list.local)
young.list.local.se <- sd(young.list.local)
young.direct.local.mean <- mean(young.direct.local)
young.direct.local.se <- sd(young.direct.local)
young.diff.local.mean <- mean(young.diff.local)
young.diff.local.se <- sd(young.diff.local)

nonyoung.list.local.mean <- mean(nonyoung.list.local)
nonyoung.list.local.se <- sd(nonyoung.list.local)
nonyoung.direct.local.mean <- mean(nonyoung.direct.local)
nonyoung.direct.local.se <- sd(nonyoung.direct.local)
nonyoung.diff.local.mean <- mean(nonyoung.diff.local)
nonyoung.diff.local.se <- sd(nonyoung.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonyoung.list.local.se,
       nonyoung.direct.local.se,
       nonyoung.diff.local.se,
       young.list.local.se,
       young.direct.local.se,
       young.diff.local.se)
c3 = c(nonyoung.list.local.mean, 
       nonyoung.direct.local.mean,
       nonyoung.diff.local.mean,
       young.list.local.mean, 
       young.direct.local.mean,
       young.diff.local.mean)
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
  ggtitle("Trust in local governments by age")

#### subgroup fig; Central by high.income#####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)
 
fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember + high.income +
                             pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + high.income +
                            pinterest + life + self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, high.income,
                                           pinterest, life, self_index,  china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, high.income,
                                             pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.high.income.list.central <- as.matrix(subset(x.list.central, high.income == 1))
x.nonhigh.income.list.central <- as.matrix(subset(x.list.central, high.income == 0))
x.high.income.direct.central <- as.matrix(subset(x.direct.central, high.income == 1))
x.nonhigh.income.direct.central <- as.matrix(subset(x.direct.central, high.income == 0))

high.income.list.central <- nonhigh.income.list.central <- 
  high.income.direct.central <- nonhigh.income.direct.central<-
  high.income.diff.central <- nonhigh.income.diff.central <- rep(NA, n.draws)

set.seed(134)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.income.list.central <- logistic(x.high.income.list.central %*% par.g)
  
  pred.high.income.direct.central <- logistic(x.high.income.direct.central %*% draws.direct.central[d,])
  
  high.income.list.central[d] <- mean(pred.high.income.list.central)
  high.income.direct.central[d] <- mean(pred.high.income.direct.central)
  high.income.diff.central[d] <- high.income.list.central[d] - high.income.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonhigh.income.list.central <- logistic(x.nonhigh.income.list.central %*% par.g)
  
  pred.nonhigh.income.direct.central <- logistic(x.nonhigh.income.direct.central %*% draws.direct.central[d,])
  
  nonhigh.income.list.central[d] <- mean(pred.nonhigh.income.list.central)
  nonhigh.income.direct.central[d] <- mean(pred.nonhigh.income.direct.central)
  nonhigh.income.diff.central[d] <- nonhigh.income.list.central[d] - nonhigh.income.direct.central[d]
  
}



high.income.list.central.mean <- mean(high.income.list.central)
high.income.list.central.se <- sd(high.income.list.central)
high.income.direct.central.mean <- mean(high.income.direct.central)
high.income.direct.central.se <- sd(high.income.direct.central)
high.income.diff.central.mean <- mean(high.income.diff.central)
high.income.diff.central.se <- sd(high.income.diff.central)

nonhigh.income.list.central.mean <- mean(nonhigh.income.list.central)
nonhigh.income.list.central.se <- sd(nonhigh.income.list.central)
nonhigh.income.direct.central.mean <- mean(nonhigh.income.direct.central)
nonhigh.income.direct.central.se <- sd(nonhigh.income.direct.central)
nonhigh.income.diff.central.mean <- mean(nonhigh.income.diff.central)
nonhigh.income.diff.central.se <- sd(nonhigh.income.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.income.list.central.se,
       nonhigh.income.direct.central.se,
       nonhigh.income.diff.central.se,
       high.income.list.central.se,
       high.income.direct.central.se,
       high.income.diff.central.se)
c3 = c(nonhigh.income.list.central.mean, 
       nonhigh.income.direct.central.mean,
       nonhigh.income.diff.central.mean,
       high.income.list.central.mean, 
       high.income.direct.central.mean,
       high.income.diff.central.mean)
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
  ggtitle("Trust in central government by income level")

#### subgroup fig; Local by high.income#####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education +
                           agegroup + ccpmember + high.income +
                           pinterest + life + self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          agegroup + ccpmember + high.income +
                          pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, high.income,
                                         pinterest, life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, high.income,
                                           pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.high.income.list.local <- as.matrix(subset(x.list.local, high.income == 1))
x.nonhigh.income.list.local <- as.matrix(subset(x.list.local, high.income == 0))
x.high.income.direct.local <- as.matrix(subset(x.direct.local, high.income == 1))
x.nonhigh.income.direct.local <- as.matrix(subset(x.direct.local, high.income == 0))

high.income.list.local <- nonhigh.income.list.local <- 
  high.income.direct.local <- nonhigh.income.direct.local<-
  high.income.diff.local <- nonhigh.income.diff.local <- rep(NA, n.draws)

set.seed(136)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.high.income.list.local <- logistic(x.high.income.list.local %*% par.g)
  
  pred.high.income.direct.local <- logistic(x.high.income.direct.local %*% draws.direct.local[d,])
  
  high.income.list.local[d] <- mean(pred.high.income.list.local)
  high.income.direct.local[d] <- mean(pred.high.income.direct.local)
  high.income.diff.local[d] <- high.income.list.local[d] - high.income.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonhigh.income.list.local <- logistic(x.nonhigh.income.list.local %*% par.g)
  
  pred.nonhigh.income.direct.local <- logistic(x.nonhigh.income.direct.local %*% draws.direct.local[d,])
  
  nonhigh.income.list.local[d] <- mean(pred.nonhigh.income.list.local)
  nonhigh.income.direct.local[d] <- mean(pred.nonhigh.income.direct.local)
  nonhigh.income.diff.local[d] <- nonhigh.income.list.local[d] - nonhigh.income.direct.local[d]
  
}



high.income.list.local.mean <- mean(high.income.list.local)
high.income.list.local.se <- sd(high.income.list.local)
high.income.direct.local.mean <- mean(high.income.direct.local)
high.income.direct.local.se <- sd(high.income.direct.local)
high.income.diff.local.mean <- mean(high.income.diff.local)
high.income.diff.local.se <- sd(high.income.diff.local)

nonhigh.income.list.local.mean <- mean(nonhigh.income.list.local)
nonhigh.income.list.local.se <- sd(nonhigh.income.list.local)
nonhigh.income.direct.local.mean <- mean(nonhigh.income.direct.local)
nonhigh.income.direct.local.se <- sd(nonhigh.income.direct.local)
nonhigh.income.diff.local.mean <- mean(nonhigh.income.diff.local)
nonhigh.income.diff.local.se <- sd(nonhigh.income.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.income.list.local.se,
       nonhigh.income.direct.local.se,
       nonhigh.income.diff.local.se,
       high.income.list.local.se,
       high.income.direct.local.se,
       high.income.diff.local.se)
c3 = c(nonhigh.income.list.local.mean, 
       nonhigh.income.direct.local.mean,
       nonhigh.income.diff.local.mean,
       high.income.list.local.mean, 
       high.income.direct.local.mean,
       high.income.diff.local.mean)
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
  ggtitle("Trust in local governments by income level")

#### subgroup fig; Central by high.life #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             pinterest + high.life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + income +
                            pinterest + high.life+ self_index
                            + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           pinterest, high.life, self_index, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, income,
                                             pinterest, high.life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.high.life.list.central <- as.matrix(subset(x.list.central, high.life == 1))
x.nonhigh.life.list.central <- as.matrix(subset(x.list.central, high.life == 0))
x.high.life.direct.central <- as.matrix(subset(x.direct.central, high.life == 1))
x.nonhigh.life.direct.central <- as.matrix(subset(x.direct.central, high.life == 0))

high.life.list.central <- nonhigh.life.list.central <- 
  high.life.direct.central <- nonhigh.life.direct.central<-
  high.life.diff.central <- nonhigh.life.diff.central <- rep(NA, n.draws)

set.seed(349)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.life.list.central <- logistic(x.high.life.list.central %*% par.g)
  
  pred.high.life.direct.central <- logistic(x.high.life.direct.central %*% draws.direct.central[d,])
  
  high.life.list.central[d] <- mean(pred.high.life.list.central)
  high.life.direct.central[d] <- mean(pred.high.life.direct.central)
  high.life.diff.central[d] <- high.life.list.central[d] - high.life.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonhigh.life.list.central <- logistic(x.nonhigh.life.list.central %*% par.g)
  
  pred.nonhigh.life.direct.central <- logistic(x.nonhigh.life.direct.central %*% draws.direct.central[d,])
  
  nonhigh.life.list.central[d] <- mean(pred.nonhigh.life.list.central)
  nonhigh.life.direct.central[d] <- mean(pred.nonhigh.life.direct.central)
  nonhigh.life.diff.central[d] <- nonhigh.life.list.central[d] - nonhigh.life.direct.central[d]
  
}



high.life.list.central.mean <- mean(high.life.list.central)
high.life.list.central.se <- sd(high.life.list.central)
high.life.direct.central.mean <- mean(high.life.direct.central)
high.life.direct.central.se <- sd(high.life.direct.central)
high.life.diff.central.mean <- mean(high.life.diff.central)
high.life.diff.central.se <- sd(high.life.diff.central)

nonhigh.life.list.central.mean <- mean(nonhigh.life.list.central)
nonhigh.life.list.central.se <- sd(nonhigh.life.list.central)
nonhigh.life.direct.central.mean <- mean(nonhigh.life.direct.central)
nonhigh.life.direct.central.se <- sd(nonhigh.life.direct.central)
nonhigh.life.diff.central.mean <- mean(nonhigh.life.diff.central)
nonhigh.life.diff.central.se <- sd(nonhigh.life.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.life.list.central.se,
       nonhigh.life.direct.central.se,
       nonhigh.life.diff.central.se,
       high.life.list.central.se,
       high.life.direct.central.se,
       high.life.diff.central.se)
c3 = c(nonhigh.life.list.central.mean, 
       nonhigh.life.direct.central.mean,
       nonhigh.life.diff.central.mean,
       high.life.list.central.mean, 
       high.life.direct.central.mean,
       high.life.diff.central.mean)
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
  ggtitle("Trust in central government by life satisfication")

#### subgroup fig; local by high.life #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)
 
fit.list.local <- ictreg(y ~ female + education +
                           agegroup + ccpmember  + income +
                           pinterest + high.life + self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education+ 
                          agegroup + ccpmember + income +
                          pinterest + high.life+ self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, income,
                                         pinterest, high.life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, high.life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.high.life.list.local <- as.matrix(subset(x.list.local, high.life == 1))
x.nonhigh.life.list.local <- as.matrix(subset(x.list.local, high.life == 0))
x.high.life.direct.local <- as.matrix(subset(x.direct.local, high.life == 1))
x.nonhigh.life.direct.local <- as.matrix(subset(x.direct.local, high.life == 0))

high.life.list.local <- nonhigh.life.list.local <- 
  high.life.direct.local <- nonhigh.life.direct.local<-
  high.life.diff.local <- nonhigh.life.diff.local <- rep(NA, n.draws)

set.seed(30235)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.high.life.list.local <- logistic(x.high.life.list.local %*% par.g)
  
  pred.high.life.direct.local <- logistic(x.high.life.direct.local %*% draws.direct.local[d,])
  
  high.life.list.local[d] <- mean(pred.high.life.list.local)
  high.life.direct.local[d] <- mean(pred.high.life.direct.local)
  high.life.diff.local[d] <- high.life.list.local[d] - high.life.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonhigh.life.list.local <- logistic(x.nonhigh.life.list.local %*% par.g)
  
  pred.nonhigh.life.direct.local <- logistic(x.nonhigh.life.direct.local %*% draws.direct.local[d,])
  
  nonhigh.life.list.local[d] <- mean(pred.nonhigh.life.list.local)
  nonhigh.life.direct.local[d] <- mean(pred.nonhigh.life.direct.local)
  nonhigh.life.diff.local[d] <- nonhigh.life.list.local[d] - nonhigh.life.direct.local[d]
  
}



high.life.list.local.mean <- mean(high.life.list.local)
high.life.list.local.se <- sd(high.life.list.local)
high.life.direct.local.mean <- mean(high.life.direct.local)
high.life.direct.local.se <- sd(high.life.direct.local)
high.life.diff.local.mean <- mean(high.life.diff.local)
high.life.diff.local.se <- sd(high.life.diff.local)

nonhigh.life.list.local.mean <- mean(nonhigh.life.list.local)
nonhigh.life.list.local.se <- sd(nonhigh.life.list.local)
nonhigh.life.direct.local.mean <- mean(nonhigh.life.direct.local)
nonhigh.life.direct.local.se <- sd(nonhigh.life.direct.local)
nonhigh.life.diff.local.mean <- mean(nonhigh.life.diff.local)
nonhigh.life.diff.local.se <- sd(nonhigh.life.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.life.list.local.se,
       nonhigh.life.direct.local.se,
       nonhigh.life.diff.local.se,
       high.life.list.local.se,
       high.life.direct.local.se,
       high.life.diff.local.se)
c3 = c(nonhigh.life.list.local.mean, 
       nonhigh.life.direct.local.mean,
       nonhigh.life.diff.local.mean,
       high.life.list.local.mean, 
       high.life.direct.local.mean,
       high.life.diff.local.mean)
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
  ggtitle("Trust in local governments by life satisfication")

#### subgroup fig; Central by high.pinterest #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             high.pinterest + life + self_index
                             + china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + income +
                            high.pinterest + life + self_index
                          + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           high.pinterest, life, self_index, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, income,
                                             high.pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.central)

x.high.pinterest.list.central <- as.matrix(subset(x.list.central, high.pinterest == 1))
x.nonhigh.pinterest.list.central <- as.matrix(subset(x.list.central, high.pinterest == 0))
x.high.pinterest.direct.central <- as.matrix(subset(x.direct.central, high.pinterest == 1))
x.nonhigh.pinterest.direct.central <- as.matrix(subset(x.direct.central, high.pinterest == 0))

high.pinterest.list.central <- nonhigh.pinterest.list.central <- 
  high.pinterest.direct.central <- nonhigh.pinterest.direct.central<-
  high.pinterest.diff.central <- nonhigh.pinterest.diff.central <- rep(NA, n.draws)

set.seed(989)

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.pinterest.list.central <- logistic(x.high.pinterest.list.central %*% par.g)
  
  pred.high.pinterest.direct.central <- logistic(x.high.pinterest.direct.central %*% draws.direct.central[d,])
  
  high.pinterest.list.central[d] <- mean(pred.high.pinterest.list.central)
  high.pinterest.direct.central[d] <- mean(pred.high.pinterest.direct.central)
  high.pinterest.diff.central[d] <- high.pinterest.list.central[d] - high.pinterest.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonhigh.pinterest.list.central <- logistic(x.nonhigh.pinterest.list.central %*% par.g)
  
  pred.nonhigh.pinterest.direct.central <- logistic(x.nonhigh.pinterest.direct.central %*% draws.direct.central[d,])
  
  nonhigh.pinterest.list.central[d] <- mean(pred.nonhigh.pinterest.list.central)
  nonhigh.pinterest.direct.central[d] <- mean(pred.nonhigh.pinterest.direct.central)
  nonhigh.pinterest.diff.central[d] <- nonhigh.pinterest.list.central[d] - nonhigh.pinterest.direct.central[d]
  
}



high.pinterest.list.central.mean <- mean(high.pinterest.list.central)
high.pinterest.list.central.se <- sd(high.pinterest.list.central)
high.pinterest.direct.central.mean <- mean(high.pinterest.direct.central)
high.pinterest.direct.central.se <- sd(high.pinterest.direct.central)
high.pinterest.diff.central.mean <- mean(high.pinterest.diff.central)
high.pinterest.diff.central.se <- sd(high.pinterest.diff.central)

nonhigh.pinterest.list.central.mean <- mean(nonhigh.pinterest.list.central)
nonhigh.pinterest.list.central.se <- sd(nonhigh.pinterest.list.central)
nonhigh.pinterest.direct.central.mean <- mean(nonhigh.pinterest.direct.central)
nonhigh.pinterest.direct.central.se <- sd(nonhigh.pinterest.direct.central)
nonhigh.pinterest.diff.central.mean <- mean(nonhigh.pinterest.diff.central)
nonhigh.pinterest.diff.central.se <- sd(nonhigh.pinterest.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.pinterest.list.central.se,
       nonhigh.pinterest.direct.central.se,
       nonhigh.pinterest.diff.central.se,
       high.pinterest.list.central.se,
       high.pinterest.direct.central.se,
       high.pinterest.diff.central.se)
c3 = c(nonhigh.pinterest.list.central.mean, 
       nonhigh.pinterest.direct.central.mean,
       nonhigh.pinterest.diff.central.mean,
       high.pinterest.list.central.mean, 
       high.pinterest.direct.central.mean,
       high.pinterest.diff.central.mean)
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
  ggtitle("Trust in central government by political interest level ")

#### subgroup fig; Local by high.pinterest #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + 
                           agegroup + ccpmember  + income +
                           high.pinterest + life +self_index
                           + china + confucian_all,
                         treat = "treat", 
                         J = 4 ,
                         data = d.list.local, 
                         method = "ml",
                         fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          agegroup + ccpmember + income +
                          high.pinterest + life + self_index
                          + china + confucian_all, 
                        data = trust_dat_cy_p, 
                        family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, income,
                                         high.pinterest, life, self_index, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           high.pinterest, life, self_index, china, confucian_all)))

k <- ncol(x.list.local)

x.high.pinterest.list.local <- as.matrix(subset(x.list.local, high.pinterest == 1))
x.nonhigh.pinterest.list.local <- as.matrix(subset(x.list.local, high.pinterest == 0))
x.high.pinterest.direct.local <- as.matrix(subset(x.direct.local, high.pinterest == 1))
x.nonhigh.pinterest.direct.local <- as.matrix(subset(x.direct.local, high.pinterest == 0))

high.pinterest.list.local <- nonhigh.pinterest.list.local <- 
  high.pinterest.direct.local <- nonhigh.pinterest.direct.local<-
  high.pinterest.diff.local <- nonhigh.pinterest.diff.local <- rep(NA, n.draws)

set.seed(909)

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.high.pinterest.list.local <- logistic(x.high.pinterest.list.local %*% par.g)
  
  pred.high.pinterest.direct.local <- logistic(x.high.pinterest.direct.local %*% draws.direct.local[d,])
  
  high.pinterest.list.local[d] <- mean(pred.high.pinterest.list.local)
  high.pinterest.direct.local[d] <- mean(pred.high.pinterest.direct.local)
  high.pinterest.diff.local[d] <- high.pinterest.list.local[d] - high.pinterest.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonhigh.pinterest.list.local <- logistic(x.nonhigh.pinterest.list.local %*% par.g)
  
  pred.nonhigh.pinterest.direct.local <- logistic(x.nonhigh.pinterest.direct.local %*% draws.direct.local[d,])
  
  nonhigh.pinterest.list.local[d] <- mean(pred.nonhigh.pinterest.list.local)
  nonhigh.pinterest.direct.local[d] <- mean(pred.nonhigh.pinterest.direct.local)
  nonhigh.pinterest.diff.local[d] <- nonhigh.pinterest.list.local[d] - nonhigh.pinterest.direct.local[d]
  
}



high.pinterest.list.local.mean <- mean(high.pinterest.list.local)
high.pinterest.list.local.se <- sd(high.pinterest.list.local)
high.pinterest.direct.local.mean <- mean(high.pinterest.direct.local)
high.pinterest.direct.local.se <- sd(high.pinterest.direct.local)
high.pinterest.diff.local.mean <- mean(high.pinterest.diff.local)
high.pinterest.diff.local.se <- sd(high.pinterest.diff.local)

nonhigh.pinterest.list.local.mean <- mean(nonhigh.pinterest.list.local)
nonhigh.pinterest.list.local.se <- sd(nonhigh.pinterest.list.local)
nonhigh.pinterest.direct.local.mean <- mean(nonhigh.pinterest.direct.local)
nonhigh.pinterest.direct.local.se <- sd(nonhigh.pinterest.direct.local)
nonhigh.pinterest.diff.local.mean <- mean(nonhigh.pinterest.diff.local)
nonhigh.pinterest.diff.local.se <- sd(nonhigh.pinterest.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.pinterest.list.local.se,
       nonhigh.pinterest.direct.local.se,
       nonhigh.pinterest.diff.local.se,
       high.pinterest.list.local.se,
       high.pinterest.direct.local.se,
       high.pinterest.diff.local.se)
c3 = c(nonhigh.pinterest.list.local.mean, 
       nonhigh.pinterest.direct.local.mean,
       nonhigh.pinterest.diff.local.mean,
       high.pinterest.list.local.mean, 
       high.pinterest.direct.local.mean,
       high.pinterest.diff.local.mean)
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
  ggtitle("Trust in local governments by political interest level")

#### subgroup fig; Central by high.self #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             pinterest + life + high.self + china + confucian_all,
                          treat = "treat", 
                          J = 4 ,
                          data = d.list.central, 
                          method = "ml",
                         fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + income +
                            pinterest + life + high.self + china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           pinterest, life, high.self, china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                            pinterest, life, high.self, china, confucian_all)))

k <- ncol(x.list.central)

x.high.self.list.central <- as.matrix(subset(x.list.central, high.self == 1))
x.nonhigh.self.list.central <- as.matrix(subset(x.list.central, high.self == 0))
x.high.self.direct.central <- as.matrix(subset(x.direct.central, high.self == 1))
x.nonhigh.self.direct.central <- as.matrix(subset(x.direct.central, high.self == 0))

high.self.list.central <- nonhigh.self.list.central <- 
  high.self.direct.central <- nonhigh.self.direct.central<-
  high.self.diff.central <- nonhigh.self.diff.central <- rep(NA, n.draws)

set.seed(111)

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.self.list.central <- logistic(x.high.self.list.central %*% par.g)
  
  pred.high.self.direct.central <- logistic(x.high.self.direct.central %*% draws.direct.central[d,])
  
  high.self.list.central[d] <- mean(pred.high.self.list.central)
  high.self.direct.central[d] <- mean(pred.high.self.direct.central)
  high.self.diff.central[d] <- high.self.list.central[d] - high.self.direct.central[d]
    
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonhigh.self.list.central <- logistic(x.nonhigh.self.list.central %*% par.g)
  
  pred.nonhigh.self.direct.central <- logistic(x.nonhigh.self.direct.central %*% draws.direct.central[d,])
  
  nonhigh.self.list.central[d] <- mean(pred.nonhigh.self.list.central)
  nonhigh.self.direct.central[d] <- mean(pred.nonhigh.self.direct.central)
  nonhigh.self.diff.central[d] <- nonhigh.self.list.central[d] - nonhigh.self.direct.central[d]
  
}



high.self.list.central.mean <- mean(high.self.list.central)
high.self.list.central.se <- sd(high.self.list.central)
high.self.direct.central.mean <- mean(high.self.direct.central)
high.self.direct.central.se <- sd(high.self.direct.central)
high.self.diff.central.mean <- mean(high.self.diff.central)
high.self.diff.central.se <- sd(high.self.diff.central)

nonhigh.self.list.central.mean <- mean(nonhigh.self.list.central)
nonhigh.self.list.central.se <- sd(nonhigh.self.list.central)
nonhigh.self.direct.central.mean <- mean(nonhigh.self.direct.central)
nonhigh.self.direct.central.se <- sd(nonhigh.self.direct.central)
nonhigh.self.diff.central.mean <- mean(nonhigh.self.diff.central)
nonhigh.self.diff.central.se <- sd(nonhigh.self.diff.central)

#Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.self.list.central.se,
       nonhigh.self.direct.central.se,
       nonhigh.self.diff.central.se,
      high.self.list.central.se,
      high.self.direct.central.se,
       high.self.diff.central.se)
c3 = c(nonhigh.self.list.central.mean, 
       nonhigh.self.direct.central.mean,
      nonhigh.self.diff.central.mean,
       high.self.list.central.mean, 
      high.self.direct.central.mean,
     high.self.diff.central.mean)
c4 = c(rep('lower self-monitoring level',3),rep("Higher self-monitoring level",3))
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
  ggtitle("Trust in central government by self-monitoring level")

#### subgroup fig; Local by high.self #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + 
                           agegroup + ccpmember  + income +
                           pinterest + life + high.self + china + confucian_all,
                         treat = "treat", 
                        J = 4 ,
                        data = d.list.local, 
                         method = "ml",
                       fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                          agegroup + ccpmember + income +
                         pinterest + life + high.self + china + confucian_all, 
                        data = trust_dat_cy_p, 
                       family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                   cbind(1, female, education, 
                                         agegroup, ccpmember, income,
                                         pinterest, life, high.self, china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                     cbind(1, female, education,
                                           agegroup, ccpmember, income,
                                         pinterest, life, high.self, china, confucian_all)))

k <- ncol(x.list.local)

x.high.self.list.local <- as.matrix(subset(x.list.local, high.self == 1))
x.nonhigh.self.list.local <- as.matrix(subset(x.list.local, high.self == 0))
x.high.self.direct.local <- as.matrix(subset(x.direct.local, high.self == 1))
x.nonhigh.self.direct.local <- as.matrix(subset(x.direct.local, high.self == 0))

high.self.list.local <- nonhigh.self.list.local <- 
 high.self.direct.local <- nonhigh.self.direct.local<-
  high.self.diff.local <- nonhigh.self.diff.local <- rep(NA, n.draws)

set.seed(111)

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
 pred.high.self.list.local <- logistic(x.high.self.list.local %*% par.g)
  
 pred.high.self.direct.local <- logistic(x.high.self.direct.local %*% draws.direct.local[d,])
  
 high.self.list.local[d] <- mean(pred.high.self.list.local)
  high.self.direct.local[d] <- mean(pred.high.self.direct.local)
 high.self.diff.local[d] <- high.self.list.local[d] - high.self.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
pred.nonhigh.self.list.local <- logistic(x.nonhigh.self.list.local %*% par.g)
  
pred.nonhigh.self.direct.local <- logistic(x.nonhigh.self.direct.local %*% draws.direct.local[d,])
  
 nonhigh.self.list.local[d] <- mean(pred.nonhigh.self.list.local)
 nonhigh.self.direct.local[d] <- mean(pred.nonhigh.self.direct.local)
 nonhigh.self.diff.local[d] <- nonhigh.self.list.local[d] - nonhigh.self.direct.local[d]
  
}



high.self.list.local.mean <- mean(high.self.list.local)
high.self.list.local.se <- sd(high.self.list.local)
high.self.direct.local.mean <- mean(high.self.direct.local)
high.self.direct.local.se <- sd(high.self.direct.local)
high.self.diff.local.mean <- mean(high.self.diff.local)
high.self.diff.local.se <- sd(high.self.diff.local)

nonhigh.self.list.local.mean <- mean(nonhigh.self.list.local)
nonhigh.self.list.local.se <- sd(nonhigh.self.list.local)
nonhigh.self.direct.local.mean <- mean(nonhigh.self.direct.local)
nonhigh.self.direct.local.se <- sd(nonhigh.self.direct.local)
nonhigh.self.diff.local.mean <- mean(nonhigh.self.diff.local)
nonhigh.self.diff.local.se <- sd(nonhigh.self.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.self.list.local.se,
       nonhigh.self.direct.local.se,
       nonhigh.self.diff.local.se,
       high.self.list.local.se,
    high.self.direct.local.se,
     high.self.diff.local.se)
c3 = c(nonhigh.self.list.local.mean, 
     nonhigh.self.direct.local.mean,
     nonhigh.self.diff.local.mean,
     high.self.list.local.mean, 
    high.self.direct.local.mean,
    high.self.diff.local.mean)
c4 = c(rep('lower self-monitoring level',3),rep("Higher self-monitoring level",3))
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
 ggtitle("Trust in local governments by self-monitoring level")

#### subgroup fig; Central by china.situation #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + 
                             agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + high.china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + 
                            agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + high.china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, life, self_index,
                                           high.china, confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, 
                                             agegroup, ccpmember, income,
                                             pinterest, life, self_index,
                                             high.china, confucian_all)))

k <- ncol(x.list.central)

x.high.china.list.central <- as.matrix(subset(x.list.central, high.china == 1))
x.non.high.china.list.central <- as.matrix(subset(x.list.central, high.china == 0))
x.high.china.direct.central <- as.matrix(subset(x.direct.central, high.china == 1))
x.non.high.china.direct.central <- as.matrix(subset(x.direct.central, high.china == 0))

high.china.list.central <- non.high.china.list.central <- 
  high.china.direct.central <- non.high.china.direct.central<-
  high.china.diff.central <- non.high.china.diff.central <- rep(NA, n.draws)

set.seed(89956)
draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.china.list.central <- logistic(x.high.china.list.central %*% par.g)
  
  pred.high.china.direct.central <- logistic(x.high.china.direct.central %*% draws.direct.central[d,])
  
  high.china.list.central[d] <- mean(pred.high.china.list.central)
  high.china.direct.central[d] <- mean(pred.high.china.direct.central)
  high.china.diff.central[d] <- high.china.list.central[d] - high.china.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.non.high.china.list.central <- logistic(x.non.high.china.list.central %*% par.g)
  
  pred.non.high.china.direct.central <- logistic(x.non.high.china.direct.central %*% draws.direct.central[d,])
  
  non.high.china.list.central[d] <- mean(pred.non.high.china.list.central)
  non.high.china.direct.central[d] <- mean(pred.non.high.china.direct.central)
  non.high.china.diff.central[d] <- non.high.china.list.central[d] - non.high.china.direct.central[d]
  
}



high.china.list.central.mean <- mean(high.china.list.central)
high.china.list.central.se <- sd(high.china.list.central)
high.china.direct.central.mean <- mean(high.china.direct.central)
high.china.direct.central.se <- sd(high.china.direct.central)
high.china.diff.central.mean <- mean(high.china.diff.central)
high.china.diff.central.se <- sd(high.china.diff.central)

non.high.china.list.central.mean <- mean(non.high.china.list.central)
non.high.china.list.central.se <- sd(non.high.china.list.central)
non.high.china.direct.central.mean <- mean(non.high.china.direct.central)
non.high.china.direct.central.se <- sd(non.high.china.direct.central)
non.high.china.diff.central.mean <- mean(non.high.china.diff.central)
non.high.china.diff.central.se <- sd(non.high.china.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(non.high.china.list.central.se,
       non.high.china.direct.central.se,
       non.high.china.diff.central.se,
       high.china.list.central.se,
       high.china.direct.central.se,
       high.china.diff.central.se)

c3 = c(non.high.china.list.central.mean, 
       non.high.china.direct.central.mean,
       non.high.china.diff.central.mean,
       high.china.list.central.mean, 
       high.china.direct.central.mean,
       high.china.diff.central.mean)

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
  ggtitle("Trust in central government by the feeling about China situation")+
  theme(plot.title = element_text(size = 10))

#### subgroup fig; local by china.situation #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + 
                             agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + high.china + confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.local, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + 
                            agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + high.china + confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                     cbind(1, female, education, 
                                           agegroup, ccpmember, income,
                                           pinterest, life, self_index,
                                           high.china, confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, 
                                             agegroup, ccpmember, income,
                                             pinterest, life, self_index,
                                             high.china, confucian_all)))

k <- ncol(x.list.local)

x.high.china.list.local <- as.matrix(subset(x.list.local, high.china == 1))
x.non.high.china.list.local <- as.matrix(subset(x.list.local, high.china == 0))
x.high.china.direct.local <- as.matrix(subset(x.direct.local, high.china == 1))
x.non.high.china.direct.local <- as.matrix(subset(x.direct.local, high.china == 0))

high.china.list.local <- non.high.china.list.local <- 
  high.china.direct.local <- non.high.china.direct.local<-
  high.china.diff.local <- non.high.china.diff.local <- rep(NA, n.draws)

set.seed(9856)
draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.high.china.list.local <- logistic(x.high.china.list.local %*% par.g)
  
  pred.high.china.direct.local <- logistic(x.high.china.direct.local %*% draws.direct.local[d,])
  
  high.china.list.local[d] <- mean(pred.high.china.list.local)
  high.china.direct.local[d] <- mean(pred.high.china.direct.local)
  high.china.diff.local[d] <- high.china.list.local[d] - high.china.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.non.high.china.list.local <- logistic(x.non.high.china.list.local %*% par.g)
  
  pred.non.high.china.direct.local <- logistic(x.non.high.china.direct.local %*% draws.direct.local[d,])
  
  non.high.china.list.local[d] <- mean(pred.non.high.china.list.local)
  non.high.china.direct.local[d] <- mean(pred.non.high.china.direct.local)
  non.high.china.diff.local[d] <- non.high.china.list.local[d] - non.high.china.direct.local[d]
  
}



high.china.list.local.mean <- mean(high.china.list.local)
high.china.list.local.se <- sd(high.china.list.local)
high.china.direct.local.mean <- mean(high.china.direct.local)
high.china.direct.local.se <- sd(high.china.direct.local)
high.china.diff.local.mean <- mean(high.china.diff.local)
high.china.diff.local.se <- sd(high.china.diff.local)

non.high.china.list.local.mean <- mean(non.high.china.list.local)
non.high.china.list.local.se <- sd(non.high.china.list.local)
non.high.china.direct.local.mean <- mean(non.high.china.direct.local)
non.high.china.direct.local.se <- sd(non.high.china.direct.local)
non.high.china.diff.local.mean <- mean(non.high.china.diff.local)
non.high.china.diff.local.se <- sd(non.high.china.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(non.high.china.list.local.se,non.high.china.direct.local.se,non.high.china.diff.local.se,
       high.china.list.local.se,high.china.direct.local.se,high.china.diff.local.se)
c3 = c(non.high.china.list.local.mean, non.high.china.direct.local.mean,non.high.china.diff.local.mean,
       high.china.list.local.mean, high.china.direct.local.mean,high.china.diff.local.mean)
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
  ggtitle("Trust in local governments by the feeling about China situation")+
  theme(plot.title = element_text(size = 10))

#### subgroup fig; Central by high.confucian_all #####
# Fit the model 
d.list.central$treat <- as.numeric(d.list.central$treat)

fit.list.central <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + china + high.confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.central, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.central <- glm(centraltrust_all_b ~ female + education + agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + china + high.confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.central <- vcov(fit.list.central)
par.list.central <- coef(fit.list.central)

vcov.direct.central <- vcov(fit.direct.central)
par.direct.central <- coef(fit.direct.central)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.central <- as.data.frame(with(d.list.central,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, high.confucian_all)))

x.direct.central <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, income,
                                             pinterest, life, self_index, china, high.confucian_all)))

k <- ncol(x.list.central)

x.high.confucian_all.list.central <- as.matrix(subset(x.list.central, high.confucian_all == 1))
x.nonhigh.confucian_all.list.central <- as.matrix(subset(x.list.central, high.confucian_all == 0))
x.high.confucian_all.direct.central <- as.matrix(subset(x.direct.central, high.confucian_all == 1))
x.nonhigh.confucian_all.direct.central <- as.matrix(subset(x.direct.central, high.confucian_all == 0))

high.confucian_all.list.central <- nonhigh.confucian_all.list.central <- 
  high.confucian_all.direct.central <- nonhigh.confucian_all.direct.central<-
  high.confucian_all.diff.central <- nonhigh.confucian_all.diff.central <- rep(NA, n.draws)

set.seed(390)

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.high.confucian_all.list.central <- logistic(x.high.confucian_all.list.central %*% par.g)
  
  pred.high.confucian_all.direct.central <- logistic(x.high.confucian_all.direct.central %*% draws.direct.central[d,])
  
  high.confucian_all.list.central[d] <- mean(pred.high.confucian_all.list.central)
  high.confucian_all.direct.central[d] <- mean(pred.high.confucian_all.direct.central)
  high.confucian_all.diff.central[d] <- high.confucian_all.list.central[d] - high.confucian_all.direct.central[d]
  
}

draws.list.central <- mvrnorm(n = n.draws, mu = par.list.central[1:k], Sigma = vcov.list.central[1:k,1:k])
draws.direct.central <- mvrnorm(n = n.draws, mu = par.direct.central, Sigma = vcov.direct.central)

for (d in 1:n.draws) {
  
  par.g <- draws.list.central[d, ]
  
  pred.nonhigh.confucian_all.list.central <- logistic(x.nonhigh.confucian_all.list.central %*% par.g)
  
  pred.nonhigh.confucian_all.direct.central <- logistic(x.nonhigh.confucian_all.direct.central %*% draws.direct.central[d,])
  
  nonhigh.confucian_all.list.central[d] <- mean(pred.nonhigh.confucian_all.list.central)
  nonhigh.confucian_all.direct.central[d] <- mean(pred.nonhigh.confucian_all.direct.central)
  nonhigh.confucian_all.diff.central[d] <- nonhigh.confucian_all.list.central[d] - nonhigh.confucian_all.direct.central[d]
  
}



high.confucian_all.list.central.mean <- mean(high.confucian_all.list.central)
high.confucian_all.list.central.se <- sd(high.confucian_all.list.central)
high.confucian_all.direct.central.mean <- mean(high.confucian_all.direct.central)
high.confucian_all.direct.central.se <- sd(high.confucian_all.direct.central)
high.confucian_all.diff.central.mean <- mean(high.confucian_all.diff.central)
high.confucian_all.diff.central.se <- sd(high.confucian_all.diff.central)

nonhigh.confucian_all.list.central.mean <- mean(nonhigh.confucian_all.list.central)
nonhigh.confucian_all.list.central.se <- sd(nonhigh.confucian_all.list.central)
nonhigh.confucian_all.direct.central.mean <- mean(nonhigh.confucian_all.direct.central)
nonhigh.confucian_all.direct.central.se <- sd(nonhigh.confucian_all.direct.central)
nonhigh.confucian_all.diff.central.mean <- mean(nonhigh.confucian_all.diff.central)
nonhigh.confucian_all.diff.central.se <- sd(nonhigh.confucian_all.diff.central)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.confucian_all.list.central.se,
       nonhigh.confucian_all.direct.central.se,
       nonhigh.confucian_all.diff.central.se,
       high.confucian_all.list.central.se,
       high.confucian_all.direct.central.se,
       high.confucian_all.diff.central.se)
c3 = c(nonhigh.confucian_all.list.central.mean, 
       nonhigh.confucian_all.direct.central.mean,
       nonhigh.confucian_all.diff.central.mean,
       high.confucian_all.list.central.mean, 
       high.confucian_all.direct.central.mean,
       high.confucian_all.diff.central.mean)
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
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Trust in central government by the level of Confucianism")+
  theme(plot.title = element_text(size = 12))


#### subgroup fig; local by high.confucian_all #####
# Fit the model 
d.list.local$treat <- as.numeric(d.list.local$treat)

fit.list.local <- ictreg(y ~ female + education + agegroup + ccpmember  + income +
                             pinterest + life + self_index
                             + china + high.confucian_all,
                           treat = "treat", 
                           J = 4 ,
                           data = d.list.local, 
                           method = "ml",
                           fit.start = "lm")

fit.direct.local <- glm(localtrust_all_b ~ female + education + agegroup + ccpmember + income +
                            pinterest + life + self_index
                            + china + high.confucian_all, 
                          data = trust_dat_cy_p, 
                          family = binomial("logit"))

vcov.list.local <- vcov(fit.list.local)
par.list.local <- coef(fit.list.local)

vcov.direct.local <- vcov(fit.direct.local)
par.direct.local <- coef(fit.direct.local)

n.draws <- 10000

logistic <- function(x) exp(x)/(1+exp(x))

x.list.local <- as.data.frame(with(d.list.local,
                                     cbind(1, female, education, agegroup, ccpmember, income,
                                           pinterest, life, self_index, china, high.confucian_all)))

x.direct.local <- as.data.frame(with(trust_dat_cy_p,
                                       cbind(1, female, education, agegroup, ccpmember, income,
                                             pinterest, life, self_index, china, high.confucian_all)))

k <- ncol(x.list.local)

x.high.confucian_all.list.local <- as.matrix(subset(x.list.local, high.confucian_all == 1))
x.nonhigh.confucian_all.list.local <- as.matrix(subset(x.list.local, high.confucian_all == 0))
x.high.confucian_all.direct.local <- as.matrix(subset(x.direct.local, high.confucian_all == 1))
x.nonhigh.confucian_all.direct.local <- as.matrix(subset(x.direct.local, high.confucian_all == 0))

high.confucian_all.list.local <- nonhigh.confucian_all.list.local <- 
  high.confucian_all.direct.local <- nonhigh.confucian_all.direct.local<-
  high.confucian_all.diff.local <- nonhigh.confucian_all.diff.local <- rep(NA, n.draws)

set.seed(390)

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.high.confucian_all.list.local <- logistic(x.high.confucian_all.list.local %*% par.g)
  
  pred.high.confucian_all.direct.local <- logistic(x.high.confucian_all.direct.local %*% draws.direct.local[d,])
  
  high.confucian_all.list.local[d] <- mean(pred.high.confucian_all.list.local)
  high.confucian_all.direct.local[d] <- mean(pred.high.confucian_all.direct.local)
  high.confucian_all.diff.local[d] <- high.confucian_all.list.local[d] - high.confucian_all.direct.local[d]
  
}

draws.list.local <- mvrnorm(n = n.draws, mu = par.list.local[1:k], Sigma = vcov.list.local[1:k,1:k])
draws.direct.local <- mvrnorm(n = n.draws, mu = par.direct.local, Sigma = vcov.direct.local)

for (d in 1:n.draws) {
  
  par.g <- draws.list.local[d, ]
  
  pred.nonhigh.confucian_all.list.local <- logistic(x.nonhigh.confucian_all.list.local %*% par.g)
  
  pred.nonhigh.confucian_all.direct.local <- logistic(x.nonhigh.confucian_all.direct.local %*% draws.direct.local[d,])
  
  nonhigh.confucian_all.list.local[d] <- mean(pred.nonhigh.confucian_all.list.local)
  nonhigh.confucian_all.direct.local[d] <- mean(pred.nonhigh.confucian_all.direct.local)
  nonhigh.confucian_all.diff.local[d] <- nonhigh.confucian_all.list.local[d] - nonhigh.confucian_all.direct.local[d]
  
}



high.confucian_all.list.local.mean <- mean(high.confucian_all.list.local)
high.confucian_all.list.local.se <- sd(high.confucian_all.list.local)
high.confucian_all.direct.local.mean <- mean(high.confucian_all.direct.local)
high.confucian_all.direct.local.se <- sd(high.confucian_all.direct.local)
high.confucian_all.diff.local.mean <- mean(high.confucian_all.diff.local)
high.confucian_all.diff.local.se <- sd(high.confucian_all.diff.local)

nonhigh.confucian_all.list.local.mean <- mean(nonhigh.confucian_all.list.local)
nonhigh.confucian_all.list.local.se <- sd(nonhigh.confucian_all.list.local)
nonhigh.confucian_all.direct.local.mean <- mean(nonhigh.confucian_all.direct.local)
nonhigh.confucian_all.direct.local.se <- sd(nonhigh.confucian_all.direct.local)
nonhigh.confucian_all.diff.local.mean <- mean(nonhigh.confucian_all.diff.local)
nonhigh.confucian_all.diff.local.se <- sd(nonhigh.confucian_all.diff.local)

# Generate the figure #
col_name = c('type','se','mean','sex')
c1 = c('List','Direct','Diff','List','Direct','Diff')
c2 = c(nonhigh.confucian_all.list.local.se,
       nonhigh.confucian_all.direct.local.se,
       nonhigh.confucian_all.diff.local.se,
       high.confucian_all.list.local.se,
       high.confucian_all.direct.local.se,
       high.confucian_all.diff.local.se)
c3 = c(nonhigh.confucian_all.list.local.mean, 
       nonhigh.confucian_all.direct.local.mean,
       nonhigh.confucian_all.diff.local.mean,
       high.confucian_all.list.local.mean, 
       high.confucian_all.direct.local.mean,
       high.confucian_all.diff.local.mean)
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
  theme(plot.title = element_text(size = 13))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks=seq(-0.5,1,0.25))+
  ggtitle("Trust in local government by the level of Confucianism")+
  theme(plot.title = element_text(size = 12))

#### Create the differences figure for gender and ccpmember####
# Create a ggplot data 
col_name = c("category","subgroup", "Type", "mean", "se")
c1 = c(rep("Gender", 4), 
       rep("Party member",4),
       rep("Education",4),
       rep("Age", 4),
       rep("Income level",4),
       rep("Life satisfaction",4),
       rep("Political interest", 4),
       rep("China situation", 4),
       rep("Confucianism", 4),
       rep("Self-monitoring", 4))

c2 = c("Female", "Male", "Female", "Male",
       "CCP Member", "Nonmember","CCP Member", "Nonmember",
       "College", "Noncollege","College", "Noncollege",
       "Young", "Old", "Young", "Old",
       "High income", "Low income", "High income", "Low income",
       "High satisfcation", "Low satisfaction","High satisfcation", "Low satisfaction",
       "High interest", "Low interest","High interest", "Low interest",
       "High satisfaction", "Low satisfaction","High satisfaction", "Low satisfaction",
       "High Confucianism", "Low Confucianism", "High Confucianism", "Low Confucianism",
       "High self-monitoring", "Low self-monitoring", "High self-monitoring", "Low self-monitoring")

c3 = c("Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local",
       "Central", "Central", "Local", "Local")

c4 = c(female.diff.central.mean,
       male.diff.central.mean,
       female.diff.local.mean,
       male.diff.local.mean,
       
       ccpmember.diff.central.mean,
       nonccpmember.diff.central.mean,
       ccpmember.diff.local.mean,
       nonccpmember.diff.local.mean,
       
       college.diff.central.mean,
       noncollege.diff.central.mean,
       college.diff.local.mean,
       noncollege.diff.local.mean,
       
       young.diff.central.mean,
       nonyoung.diff.central.mean,
       young.diff.local.mean,
       nonyoung.diff.local.mean,
       
       high.income.diff.central.mean,
       nonhigh.income.diff.central.mean,
       high.income.diff.local.mean,
       nonhigh.income.diff.local.mean,
       
       high.life.diff.central.mean,
       nonhigh.life.diff.central.mean,
       high.life.diff.local.mean,
       nonhigh.life.diff.local.mean,
       
       high.pinterest.diff.central.mean,
       nonhigh.pinterest.diff.central.mean,
       high.pinterest.diff.local.mean,
       nonhigh.pinterest.diff.local.mean,
       
       high.china.diff.central.mean,
       non.high.china.diff.central.mean,
       high.china.diff.local.mean,
       non.high.china.diff.local.mean,
       
       high.confucian_all.diff.central.mean,
       nonhigh.confucian_all.diff.central.mean,
       high.confucian_all.diff.local.mean,
       nonhigh.confucian_all.diff.local.mean,
       
       high.self.diff.central.mean,
       nonhigh.self.diff.central.mean,
       high.self.diff.local.mean,
       nonhigh.self.diff.local.mean)

c5 = c(female.diff.central.se,
       male.diff.central.se,
       female.diff.local.se,
       male.diff.local.se,
       
       ccpmember.diff.central.se,
       nonccpmember.diff.central.se,
       ccpmember.diff.local.se,
       nonccpmember.diff.local.se,
       
       college.diff.central.se,
       noncollege.diff.central.se,
       college.diff.local.se,
       noncollege.diff.local.se,
       
       young.diff.central.se,
       nonyoung.diff.central.se,
       young.diff.local.se,
       nonyoung.diff.local.se,
       
       high.income.diff.central.se,
       nonhigh.income.diff.central.se,
       high.income.diff.local.se,
       nonhigh.income.diff.local.se,
       
       high.life.diff.central.se,
       nonhigh.life.diff.central.se,
       high.life.diff.local.se,
       nonhigh.life.diff.local.se,
       
       high.pinterest.diff.central.se,
       nonhigh.pinterest.diff.central.se,
       high.pinterest.diff.local.se,
       nonhigh.pinterest.diff.local.se,
       
       high.china.diff.central.se,
       non.high.china.diff.central.se,
       high.china.diff.local.se,
       non.high.china.diff.local.se,
       
       high.confucian_all.diff.central.se,
       nonhigh.confucian_all.diff.central.se,
       high.confucian_all.diff.local.se,
       nonhigh.confucian_all.diff.local.se,
       
       high.self.diff.central.se,
       nonhigh.self.diff.central.se,
       high.self.diff.local.se,
       nonhigh.self.diff.local.se)

sum_df = data.frame(c1,c2,c3,c4,c5)
names(sum_df) = col_name


sum_df %>%
  mutate(lower=mean+critical*se,upper=mean-critical*se) %>% 
  ggplot(aes(x=subgroup, y=mean, color=Type)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,
                position=position_dodge(width=0.5))+
  facet_wrap(~category, ncol = 1, strip.position="right",scale='free_y') +
  ylab('Differenece between direct and list estimates (95% CI)')+
  xlab('')+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black")+
  coord_flip()+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom",
        strip.text.y  = element_text(size = 7, angle = -90)) 

# reorder the variables
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
  ggplot(aes(x=subgroup, y=mean, shape=Type)) + # I changed "color" to "shape"（20220607)
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
        strip.text.y  = element_text(size = 6, angle = -90)) 

