####################
# Problem Set 3 ###
###################

# 20 - 3 - 23 

library(readr)
library(MASS)
library(nnet)
library(ggplot2)
library(stargazer)

# Problem 1
data <- read_csv("~/GitHub/StatsII_Spring2023/datasets/gdpChange.csv")

#  Q 1.1
# Recoding and ordering categorical GDP Difference
data$GDP_CAT <- data$GDPWdiff
data$GDP_CAT[data$GDPWdiff == 0] <- "No Change"
data$GDP_CAT[data$GDPWdiff< 0] <- "Negative"
data$GDP_CAT[data$GDPWdiff> 0] <- "Positive"

data$GDP_CAT <- factor(data$GDP_CAT, levels=c('No Change', 'Negative', 'Positive'))
levels(data$GDP_CAT)


# Multinomial regression with 'No Change' as reference
mod_1 <- multinom(GDP_CAT ~ REG + OIL, data = data)
summary(mod_1)
stargazer(mod_1)

# Odds ratios
exp(coef(mod_1))
# Probabilities
(exp(coef(mod_1))) / (1 + (exp(coef(mod_1))))
stargazer((exp(coef(mod_1))) / (1 + (exp(coef(mod_1)))))

#Significance test
z <- summary(mod_1)$coefficients/summary(mod_1)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)
stargazer((p <- (1 - pnorm(abs(z), 0, 1)) * 2))

# Predicted probabilities
pp <- data.frame(fitted(mod_1))
head(data.frame(Change = data$GDP_CAT,
                No_Change = pp$No.Change,
                Negative = pp$Negative,
                Positive = pp$Positive))


# Analysis
print('The cuttoff for the log odds of no change to negative gdp difference is 3.80 (SE = 0.27),
meaning this is average effect on log odds of negative gdp difference when country is both a non-democracy
and not a major oil exporter.The cuttoff for log odds for no change to positive gdp difference is 4.533 (SE = 2.6)
which is the averge effect of being a non-democratic non-major oil exporter on log odds of positive gdp difference.

Holding oil exporter status constant, being moving from non- to democracy increases the log odds of no change
to negative GDP growth by an avaerge of 3.80 (SE = 0.76). 
The average effect of being a democracy on the log odds of no change to positive economic 
growth is 4.533 (SE = 76). With no change as the reference and holding regime type constant, moving from non- to major oil
exporter increases the log of negative GDP differences by  the average effect of being 
4.78 (SE = 6.88). The average of effect of being a major oil exporter on the log odds no change to positive economic growth is 4.57 (SE = 6.88)
However, at the 95% confidence level, only for the reletionship between regime type on positive growth we can reject the null hypothesis that
this reletionship is random (z = 2.03, p = 0.02)

Coverting to probabilities, when holding whether a country is a major 
oil exporter constant, democracies have 79.8% greater of expiriencing 
negative economic growth compared to no change over non-democracies, and an 85.5% chance they 
expirienceing positive economic growth. Democracies are thus 5.7% more likely than non-democracies to expirence positive 
economic growth compared to no change, than they are negative economic growth compared to no change. 
When holding regime type constant, major oil exporters are 99.1% more likely than non-oil exporters to expirience negative 
growth compared to no change, and 98.9% more likely to expirience positive 
growth. Thus, when controlling for regime type, major oil exporters are 0.2% 
more likely to expirience negative growth.')

# Q1.2
# Reordering
data$GDP_CAT <- factor(data$GDP_CAT, levels=c('Negative', 'No Change', 'Positive'))

# Ordered Logistic Regression
mod_2 <- polr(GDP_CAT ~ REG + OIL, data = data, Hess = TRUE)
summary(mod_2)
stargazer(mod_2)


# Odds ratios
exp(coef(mod_2))
# Probabilities
(exp(coef(mod_2))) / (1 + (exp(coef(mod_2))))
stargazer((exp(coef(mod_2))) / (1 + (exp(coef(mod_2)))))

# Significance test
ctable <- coef(summary(mod_2))
p2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p2))
stargazer((ctable <- cbind(ctable, "p value" = p2)))

# Analysis 
print('The cuttoff for the log odds of negative to no change in gdp difference 
when a country is both a non-major oil exporter and a non-democracy and is -.7312 (SE = 0.04). The 
average effect of a country being a non-major oil exporter and a non-democracy on the log logs odds no change to positive 
is 0.71 ( SE = 0.04)

Holding oil export status contant, moving from democracy to non-democracy increases the log of
GDP difference average  of 0.39 (SE = 0.04) from negative to positive.
Holding regime type contant, moving from non- to major oil exporter reduces the log of the ordered GDP difference -0.19. 
However, in the ordered logistic, neither of these reletionships are significant, 
meaning we cannot reject the null hypothesis that either association is random.

Coverting to probabilities, holding whether oil exporter status constant, demcracies are
59% more likely than non-democracies to have postive economic growth. Holding 
regime type constant, countries which are major oil exporters are 45% less like to have positive 
economic growth than non-major oil exporters.')

#Predicted probabilities
pp2 <- data.frame(fitted(mod_2))
head(data.frame(Change = data$GDP_CAT,
                Negative = pp2$Negative,
                No_Change = pp2$No.Change,
                Positive = pp2$Positive))


# Question 2
#Data
data2 <- read_csv("~/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv")

levels(data2$PAN.governor.06)

# Model
mod_3 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
               PAN.governor.06, data= data2 , family = poisson)

# Test score and p value
summary(mod_3)
stargazer(mod_3)



# Exponentiation for counts 
exp(coef(mod_3))

# Analysis
print('When controlling for the other information the model, a district going from non-competitive to competitive
reduces log count of districts visted by an average -0.08, suggeting competition is associated with a reduction in candidate vists. 
A p value <0.05 means that cannot reject the null hypothesis that 
this reletionship is by chance (z = 0.477, p = 0.63)')


# Fitted Model
coeff <- mod_3$coefficients
coeff
fm <- exp(coeff[1] + coeff[2] + coeff[4])
fm
stargazer(fm)

print('Hypothetical competitive district with average poverty and a PAN govenor
      would get an average of 0 visits from the winning candidate.')

