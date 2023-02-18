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
logis_1 <- glm(climateSupport$choice ~ climateSupport$countries + climateSupport$sanctions,
    family = "binomial")
summary(logis_1)
stargazer(logis_1, title = "Additive Model")

# Null for the Model
"Global null hypthesis: There is no reletionship between support for climate agrements and 
any componant of sanctions or the numbers of
particpating countries particpating"

# Null model
null_logis_1 <- glm(climateSupport$choice ~ 1, family = "binomial")

# Likelihood Ratio Null hypothesis test
anova(null_logis_1, logis_1, test = 'LRT')

"P_value <.001 (p = 2.2e-16) suggests there is a significant difference between the two models
and we can reject the null hypothesis that there is no reletionship between any category of sanctions,
no. of particpating countries, and support for climate agrements"

# Analysis 
"Each coefficient had a significant reletionship with climate support, with the exception of exception
of the category for 160-190 countries particpating. For each category, the odds ratios were bellow zero
meaning that each case moving from the reference category to the coefficent categories lowered the probability
of support for climate agrements"

# A. 
# Logistic equation 
# Yi = β0 + β1X+ εi
# Sanctions 5 to 15 is first and second sanction output
"Holding other variables in the model constant, when sanctions are 5% sanctions, 
individuals 3.7 times less likely (OR: -.27.p < .001) so support climate agrements
than when there are no sanction. When sanctions are at 15%, individuals 
are 5.5 (OR: -0.18.p < .001) less likely than when there are none. Increasing sanctions 
thus increases the odds of opposition to climate agrements."

# B.
logis_prob <- predict(logis_1, type="response")

logis_pred <- with(climateSupport, data.frame(sanctions_None = as.factor(c(0 , 1 , 0 , 1 ))))

pred_logis <- cbind(logis_pred, predict(logis_1, newdata = logis_pred,
                           type = "response"))
summary(pred_logis)

# C.
"The effect on repspondents of knowing that the agrement invlolves both sanctions and the no. of supporting
countries may be greater than the effect of knowledge of either"

# Interactive Model
logis_2 <- glm(climateSupport$choice ~ climateSupport$countries + climateSupport$sanctions
               + climateSupport$countries*climateSupport$sanctions, 
               family = binomial(link = "logit"))
summary(logis_2)

anova(logis_2, logis_1, test = 'LRT')
"P value > .0001, meaning there is no significant difference between differences between the models"

stargazer(logis_1, logis_2, title = "Additive and Interactive Model")