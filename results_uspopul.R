################################################################################
#
#  American Populism Project - Results 
#  Author: Hannah Cake 
#  Last updated: 4/29/2024
#
################################################################################

# Clear environment 
rm(list = ls())

# Load packages 
library(quanteda) 
library(haven) 
library(tidyverse) 
library(stats) 
library(stargazer) 
library(jtools) 
library(broom) 
library(ggstance) 
library(devtools) 
library(preText) 

### Load data 
camp_txt <- read.csv("camp_txt.csv")
View(camp_txt)

### Checking Assumptions 

hist(camp_txt$dict_gender)
hist(camp_txt$dict_populist)

mean(camp_txt$dict_populist)
mean(camp_txt$dict_gender)

var(camp_txt$dict_populist)
var(camp_txt$dict_gender)

#drop outlier
drop_row <- function(data, row_index) {
  data <- data[-row_index, ]
  return(data)
}
camp_txt <- drop_row(camp_txt, 57)
View(camp_txt)

### Create Interaction Term 

# Make Party Numeric 
camp_txt$party_numeric <- ifelse(camp_txt$party == "Republican Party", 1, 0)

# Create Interaction Term with populism and party
camp_txt$pop_party_int <- camp_txt$party_numeric * camp_txt$dict_populist

# Creating State fixed effects 
library(glmnet)
camp_txt$state_fe <- as.factor(camp_txt$state)

# Model 1
model1 <- glm(dict_gender ~ dict_populist + 
              party_numeric + rep_gender, 
              data = camp_txt, family = poisson)
summary(model1)
coefficients <- coef(model1)
p_values <- summary(model1)$coefficients[, 4]
print(coefficients)
print(p_values)

# Model 2 with interaction 
model2 <- glm(dict_gender ~ pop_party_int + dict_populist + 
                party_numeric + rep_gender, 
              data = camp_txt, family = poisson)
summary(model2)
coefficients <- coef(model2)
p_values <- summary(model2)$coefficients[, 4]
print(coefficients)
print(p_values)

# Model 3 with State fixed effects and interaction 
model3 <- glm(dict_gender ~ pop_party_int + dict_populist + 
                party_numeric + rep_gender + state_fe, 
              data = camp_txt, family = poisson)
summary(model3)
coefficients <- coef(model3)
p_values <- summary(model3)$coefficients[, 4]
print(coefficients)
print(p_values)


### Tables 
stargazer(model1, model2,model3, type = "text")
stargazer(model1, model2, model3, title = "Regression Results", 
          type = "html", out="C:results.html")






