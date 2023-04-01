#################
# Problem Set 2 # 
# R Hallissey ###
# 17-2-2023 / 29 Pluviose XXCCCI 

# Packages
library(stargazer)

############
# Problem 1
###########

# Data
# @ "https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"

# Problem 1 
# Additive Model
levels(climateSupport$choice)
levels(climateSupport$sanctions)
levels(climateSupport$countries)
logis_1 <- glm(climateSupport$choice ~ climateSupport$countries + climateSupport$sanctions,
               family = "binomial")
summary(logis_1)
stargazer(logis_1, title = "Additive Model")


# Null for the Model
"Null hypthesis for the additive model: There is no reletionship between support for 
climate agrements and any componant the sanctions and numbers of
particpating countries variables"

# Null model
null_logis_1 <- glm(climateSupport$choice ~ 1, family = "binomial")

# Likelihood Ratio Test
anova(null_logis_1, logis_1, test = 'LRT')
stargazer(anova(null_logis_1, logis_1, test = 'LRT'))

"P_value <.001 (p = 2.2e-16) suggests there is a significant difference between the two models
and we can reject the null hypothesis that there is no reletionship between support for climate agreements
 and any category of sanctions or no. of particpating countries"

# Analysis 
# Coverting log odds to probs for interpretation
(exp(logis_1$coefficients)) / (1 + (exp(logis_1$coefficients)))


"Short Analysis: Each coefficient has a significant reletionship with climate support, with the exception of 
the category for 160-190 countries particpating. Converting the log odds to probabilities,
coefficients for 80 countries partipating and 20% sanctions increased the liklihood of support
for the agreement when compared to 20 countries and no sanctions.

Of the other significant categories, the probabilities for each is bellow the intercept,
meaning their effect was to reduce the liklihood of support when compared to the baseline of 
20 countries and no sanctions"

# A. 
# Trying to do probability for Y1 = odds of it occurring over the odds of it not, not sure
# though. Formula adds the intercept and all coefficients except one for 160 countries and 
# and 5% / 15% sanctions in the two formulas respectively. My thinking was the 
# probability of it not occurring would be equal to the cumulative probability of 
# the other events occurring, so basically I left out the parameters I was interested for
# for each calulation. 

# Probability of support when sanctions 5% and countries 160
1 / 1+exp(-(-.005665 + -0.458452 + -0.181086 + -0.150207)) # 3.2154
# Probability of support when sanctions 15% and countries 160
1 / 1+exp(-(-.005665 + -0.458452 + -0.276332 + -0.150207)) 

3.436 - 3.215 # = 0.221

"Holding particpating counrtires at 160, changing sanctions from 5% to 15% increases 
the odds of support slightly from 3.2153 to 3.436."

# B.
# Trying to do probability for Y1 = odds of it occurring over the odds of it not, not sure
# though. Formula adds intercept and all coefficients except one for 80 countries.
# My thinking was the probability of it not occurring would be equal to the cumulative probability
# of the other events occurring
1 / 1+exp(-(-.005665 + -0.009950 + -0.276332 + -0.181086 + -0.150207)) # = 2.864
"When sanctions are none and particpating countries are 80, individuals are 
2.8 times more likely to support climate aggrement compared other combinations" #???

# C.
"The effect on repspondents of knowing that the agrement invlolves both sanctions and the no. of supporting
countries may be greater than the sum effect each"

# Interactive Model
logis_2 <- glm(climateSupport$choice ~ climateSupport$countries + climateSupport$sanctions
               + climateSupport$countries*climateSupport$sanctions, 
               family = binomial(link = "logit"))
summary(logis_2)

# Likelihood Ratio Test for difference between two models
anova(logis_2, logis_1, test = 'LRT')
"P value > .0001, meaning there is not a significant difference between between the models"

stargazer(logis_1, logis_2, title = "Additive and Interactive Model")

