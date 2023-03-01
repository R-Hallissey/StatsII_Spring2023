#######################
# Stats 2: tutorial 4 #
#######################

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

library(dplyr)
library(ggplot2)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.


na.omit(graduation$nsibs_cut)
graduation <- na.omit(graduation)
plot(graduation$nsibs_cut, xlab = "No. Graduated Siblings", ylab = "No. of Students")
graduation <- graduation[-which(graduation$nsibs < 0),]

plot(graduation$nonwhite, graduation$hsgrad, xlab = "No = Non-White - Yes = White,",
     ylab = "Yes = Graduate - No = Non-Graduate")

# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")

summary(mod_1)

# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <- #your model
glm(graduation$hsgrad ~ graduation$intact + graduation$nonwhite + graduation$asvab,
    family ="binomial")

summary(mod_2)


# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2. 

fvs_1 <- fitted.values(mod_1)
fvs_2 <- fitted.values(mod_2)
df <- as.data.frame()

predicted_data <- data.frame(graduation$hsgrad, fvs_1, fvs_2)

?data.frame


# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.
predicted_data %>% 
  arrange(mod_1_hat) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, mod_1_hat)) +
  geom_point(aes(colour = hsgrad), alpha = 0.5) +
  scale_y_continuous(limits )



# 3. c) Do the same for mod_2. Compare the results.
predicted_data %>% 
  arrange(fvs_2) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, fvs_2)) +
  geom_point(aes(colour = graduation.hsgrad), alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1))

# 4. Calculate McFadden's Pseudo R squared for both models. 
#    Which model explains more variance?
#    What are the p values?
summary(mod_2)

# Psuedo r square ratio of (minmum) likelihoods for null and main model 

mod_null <- glm(graduation$hsgrad ~ 1, family = "binomial")
1- logLik(mod_1)/logLik(mod_null)
