# Pluvious 5th - Tutorial One - Data Analysis Template

# Packages Tidydata
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyselect)

install.packages("matricks")
library(lme4)  

#
# library(emmeans)
# library(effects)
# DATA WRANGLING #

# Code for manually specifying contents of columns
data <- read_csv("data/tutorial1_data.csv", 
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_double(),
                   `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_double(),
                   `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_double()))

# Remove superfluous columns
data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`))

# Removes uncessary ornamentation 
names(data) <- sub(" \\[.*", "", names(data)) 

# Assigning new column names. Tip: You need to include names you wish to keep
colnames(data) <- c('Country', 'Business Ease','Tax Revenue','GDP per capita')

# Removing na
data <- drop_na(data)

# Converting column values
data$`GDP per capita` <- as.integer(data$`GDP per capita`)
data$`Business Ease` <- as.integer(data$`Business Ease`)
data$`GDP per capita` <- as.integer(data$`GDP per capita`)
data$`Tax Revenue`<- as.integer(data$`Tax Revenue`)

# scatter plot of business ease and gdp
qplot(x = `Business Ease`, y = `GDP per capita`, geom = 'point', data = data)
# Regression
summary(lm(data$`GDP per capita` ~ data$`Business Ease`))

qplot(x = `Business Ease`, y = `GDP per capita`, geom = 'point', data = data)

       
# ggplot best fit line 
data %>% 
  ggplot(aes(`Business Ease`, `GDP per capita`)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Fancier ggplot
data %>%
  ggplot(aes(`Business Ease`, `GDP per capita`,
         alpha = `Business Ease`)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ylim(0, 150000) + 
  labs(title = "GDP Per Capita and Ease of Doing Business",
       subtitle = "World Bank - Europe and Central Asia",
       alpha = "Ease of doing business") +
  theme(legend.position = c(.85, .75),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit (0.5, "cm"))


