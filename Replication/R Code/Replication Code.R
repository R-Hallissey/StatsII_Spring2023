# Replication code for; Campaign Finance Laws and Party Competition 
# Authors: Joshua D. Potter and Margit Tavits
# Published: April 2013
##########################
# Replication and additons by Ruairí Hallissey 
# 28-3-2023


##########################
# Codebook 
##########################

# cnty \ country name 
# year \ year of current election 
# rulelaw \ from World Bank
# polity \ Polity IV score
# thresh \ legal vote threshold 
# postenp \ ENP after current election
# preenp \ ENP prior to current election
# fundparity4 \ Metric presented in paper
# fundparity 3 \ Alternate drops direct funding
# directelig \ Direct funding eligibility
# demin \ Year first democratic
# demyear \ Number of democratic years
# fed \ Whether or not federal
# pres \ Whether or not presidential
# avemag \ Average district magnitude
# smd \ Whether or not SMD system
# fract \ Ethnolinguistic fractionalization
# donorlimit \ Whether limits on donations
# eligmedia \ Free media eligibility
# partyspend \ Whether limits on spending


##########################
#### Replication Code
##########################

# Packages
library(stargazer)
library(arm)
library(haven) # For dta file


# Data
data <- read_dta("~/GitHub/StatsII_Spring2023/Replication/Data/potter_tavits_data.dta")

# drop outliers
campaigns <- subset(data, postenp < 9.2)

#  Post-1974 subset for endogeneity test
later1974<-subset(campaigns, demin>1973)

# OLS Model 1 in Table 2
full<-lm(postenp ~ fundparity4
         + demyears
         + fed 
         + pres 
         + log(avemag) 
         + fract 
         + log(avemag):fract, 
         data=campaigns)
summary(full)

# Regression Model 2 in Table 2
post1974<-lm(postenp ~ fundparity4
             + demyears
             + fed 
             + pres 
             + log(avemag) 
             + fract 
             + log(avemag):fract, 
             data=later1974)	
summary(post1974)

#  texreg for table in the paper
rep.list <- list(full, post1974)
stargazer(rep.list)

# construct the endogeneity plot in Figure 1
plot(campaigns$fundparity4 
     ~ campaigns$preenp, 
     pch=20, col="grey20", cex=1.5, 
     xlab="Previous ENP", 
     ylab="Current Fund Parity Value")
display(lm(fundparity4 ~ preenp, data=campaigns))
abline(a=0.82, b=-0.04, lwd=2)	

# model to ensure that all fund parity metric components are exerting similarly-signed influences (this model is mentioned in footnote 44)
components<-lm(postenp ~ directelig 
               + partyspend
               + donorlimit
               + eligmedia
               + demyears
               + fed 
               + pres 
               + log(avemag) 
               + fract 
               + log(avemag):fract, 
               data=campaigns)
summary(components)

# model to ensure that differences between legal rules and empirical practice in a country are not driving  results 
# (mentioned in footnote 45)
rules.practice<-lm(postenp ~ fundparity4
                   + rulelaw
                   + fundparity4*rulelaw
                   + demyears
                   + fed 
                   + pres 
                   + log(avemag) 
                   + fract 
                   + log(avemag):fract, 
                   data=campaigns)
summary(rules.practice)

# model to ensure that including legal threshold (eliminates a large number of observations) does not undercut results (mentioned in footnote 56) 
threshold<-lm(postenp ~ fundparity4
              + thresh
              + demyears
              + fed 
              + pres 
              + log(avemag) 
              + fract 
              + log(avemag):fract, 
              data=campaigns)
summary(threshold)

# Additions: 
# Recoding pres to a binary variable and running interactive model
# Interactive w/out recode using full data set
int_mod <- lm(postenp ~ fundparity4
              + demyears
              + fed 
              + pres 
              + pres * demyears
              + log(avemag) 
              + fract
              + log(avemag):fract, 
              data=campaigns)

summary(int_mod)	

anova(int_mod, full)


# Recoding Pres to binary variable for interpretability
# Countries excluded due to recode: South Africa, Indonesia, Moldova, 
# Indonesia, Estonia

campaigns$pres_cat <- factor(campaigns$pres, levels=c('Non-Pres', 'Pres'))
campaigns$pres_cat[campaigns$pres < 1] <- "Non-Pres"
campaigns$pres_cat[campaigns$pres > 1] <- "Pres"


# Interactive model with categorical presidency variable
int_mod2 <- lm(postenp ~ fundparity4
               + demyears
               + fed 
               + pres_cat
               + log(avemag) 
               + fract
               + demyears:pres_cat
               + log(avemag):fract, 
               data=campaigns)
summary(int_mod2)	



# Altering the full additive model to allow for anova comparison 
full_w_cat <- lm(postenp ~ fundparity4
                 + demyears
                 + fed 
                 + pres_cat
                 + log(avemag) 
                 + fract
                 + log(avemag):fract, 
                 data=campaigns)

summary(full_w_cat)

# F test 
# # p > .05. Significant difference variance explained
anova(int_mod2, full_w_cat)


# Same operations w/ post-1974 data
# Interaction between years as democracy and presidential system is not significant 
# in post 74 mode
p_74_int_mod <- lm(postenp ~ fundparity4
                   + demyears
                   + fed 
                   + pres 
                   + pres * demyears
                   + log(avemag) 
                   + fract
                   + log(avemag):fract, 
                   data=later1974)

summary(p_74_int_mod)

anova(p_74_int_mod, post1974)

# Recdoing presidential system to binary factor for post 74 data
later1974$pres_cat <- factor(later1974$pres, levels=c('Non-Pres', 'Pres'))
later1974$pres_cat[later1974$pres == 0] <- "Non-Pres"
later1974$pres_cat[later1974$pres == 2] <- "Pres"

# Interactive model with categorical presidency variable for post 74 data
p_74_int_mod2 <- lm(postenp ~ fundparity4
                    + demyears
                    + fed 
                    + pres_cat
                    + demyears * pres_cat
                    + log(avemag) 
                    + fract
                    + log(avemag):fract, 
                    data=later1974)
summary(p_74_int_mod2)

# Adding categorical pres to 74 additive model to allow for anova comparison 
p_74_w_cat <- lm(postenp ~ fundparity4
                 + demyears
                 + fed 
                 + pres_cat
                 + log(avemag) 
                 + fract
                 + log(avemag):fract, 
                 data=later1974)

# Model does not pass .05 threshold, meaning variance explained could random
summary(p_74_w_cat)

# Amount of variance explained by each model is not significantly different
anova(p_74_int_mod2, p_74_w_cat)

# Some descriptive stats
mean(campaigns$postenp) # Highest Greece = 9 / Lowest Lowest 1.7

# Average years as a democracy by regime type in full data set
aggregate(campaigns$demyears, by = list(campaigns$pres_cat), FUN = mean)

# Average years as a democracy by regime type in full post 1974 set
aggregate(later1974$demyears, by = list(later1974$pres_cat), FUN = mean)


# For Presentation
setwd("GitHub/StatsII_Spring2023/Replication/R Code")

stargazer(full, post1974, type = "latex", out ="P+T_Main_Models.text")
stargazer(full, full_w_cat, out = "Full_and_Full_cat.txt")

stargazer(full_w_cat, int_mod2,out = "int_mod_comparison.txt")
stargazer(anova(full_w_cat, int_mod2), out = "anova_int_mod_comp.txt")


stargazer(p_74_w_cat, p_74_int_mod2, out = "post_74_comparisons.txt")
stargazer(anova(p_74_w_cat, p_74_int_mod2), out = "post_74_anova.txt")