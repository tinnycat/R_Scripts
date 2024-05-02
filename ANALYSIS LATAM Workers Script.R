################################################################################
#  
#  Project: Worker's Rights in Latin America - ANALYSIS
#  Author: Hannah Cake
#  Last Updated: 4/30/2024
#
################################################################################

# Clear environment 
rm(list=ls())

# Load packages
library(MASS)
library(haven)
library(stargazer)
library(ggplot2)
library(officer)
library(magrittr)
library(brant)
library(DescTools)
library(plm)
library(gplots)

# Load Data (But save a version with NAs included!)
LATAM_work_dirty <- read.csv("LATAM_work.csv")
# Remove NA
LATAM_work <- na.omit(LATAM_work_dirty)
View(LATAM_work)

# Create an interaction term for Populism and Left/Right
LATAM_work$popxriglef <- LATAM_work$v2xpa_popul * LATAM_work$v2pariglef_ord
print(LATAM_work$popxriglef)

# Convert uri_p to factor
LATAM_work$uri_p_fact <- as.factor(LATAM_work$uri_p)

# Convert wri_p to factor 
LATAM_work$wri_p_fact <- as.factor(LATAM_work$wri_p)

### Create State and Year Fixed Effects 

# Check for heterogeneity across country years
# Note: plotmeans draw a 95% confidence interval around the means
plotmeans(wri_p ~ year, main = "Heterogeineity across years", data = LATAM_work)
plotmeans(uri_p ~ year, main = "Heterogeineity across years", data = LATAM_work)
plotmeans(wri_p ~ COWcode, main = "Heterogeineity across years", data = LATAM_work)
plotmeans(uri_p ~ COWcode, main = "Heterogeineity across years", data = LATAM_work)

### Test fitted vs Random Effects Models 

# Set fixed effects variable 
panel_set <- pdata.frame(LATAM_work, index = c("country_name"))

# WORKER Estimate panel data model with country fixed effects
fixed_w <- plm(wri_p ~ v2xpa_popul + v2pariglef_ord + v2x_api + e_gdppc + wri_l, 
                   data = panel_set, model="within")
summary(fixed_w) 
# WORKER Estimated random effects using panel setting 
random_w <- plm(wri_p ~ v2xpa_popul + v2pariglef_ord + v2x_api + e_gdppc + wri_l, 
                    data = panel_set, model="random")
summary(random_w)
# WORKER Huasman Test to determine whether to used fixed or random. 
phtest(fixed_w, random_w)
# Use fixed effects! 

# UNION Estimate panel data model with country fixed effects
fixed_u <- plm(uri_p ~ v2xpa_popul + v2pariglef_ord + v2x_api + e_gdppc + uri_l, 
                   data = panel_set, model="within")
summary(fixed_u) 
# UNION Estimated random effects using panel setting 
random_u <- plm(uri_p ~ v2xpa_popul + v2pariglef_ord + v2x_api + e_gdppc + uri_l, 
                    data = panel_set, model="random")
summary(random_u)
# UNION Huasman Test to determine whether to used fixed or random. 
phtest(fixed_u, random_u)
# Use fixed effects! 

### Fit Ordered Logistic Regression Models

######### Union Rights Models 

# Model 1 Union rights practices (NO interaction / FE)
model1 <- polr(formula = uri_p_fact ~ v2xpa_popul + v2pariglef_ord 
               + v2x_api + e_gdppc + uri_l + year, 
               data = LATAM_work, Hess = TRUE)
summary(model1)
(ctable <- coef(summary(model1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model1, "McFadden")
# Output 
stargazer(model1, title = "Regression Results", 
          type = "text")

# Model 2 Union rights index with interaction 
model2 <- polr(formula = uri_p_fact ~ popxriglef + v2xpa_popul + v2pariglef_ord + 
                 v2x_api + e_gdppc + uri_l + year, 
               data = LATAM_work, Hess = TRUE, )
summary(model2)
(ctable <- coef(summary(model2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model2, "McFadden")
# Output 
stargazer(model2, title = "Regression Results", 
          type = "text")

# Model 3 Union rights index with interaction and country fixed effects 
model3 <- polr(formula = uri_p_fact ~ popxriglef + v2xpa_popul + v2pariglef_ord + 
                 v2x_api + e_gdppc + uri_l + year + factor(country_name), 
               data = LATAM_work, Hess = TRUE, )
summary(model3)
(ctable <- coef(summary(model2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model3, "McFadden")
# Output 
stargazer(model3, title = "Regression Results", 
          type = "text")

# Output Union Models
stargazer(model1, model2, model3, title = "Union Regression Results", 
          type = "html", out = "C:results_union.html") 

######### Workers' Rights Models 

# Model 4 Workers' Right practices (NO interaction / FE)  
model4 <- polr(formula = wri_p_fact ~ v2xpa_popul + v2pariglef_ord +
                 v2x_api + e_gdppc + wri_l + year,  
               data = LATAM_work, Hess = TRUE)
summary(model4)
(ctable <- coef(summary(model4)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model4, "McFadden")
# Output 
stargazer(model4, title = "Regression Results", 
          type = "text")

# Model 5 Workers Right Index with Interaction 
model5 <- polr(formula = wri_p_fact ~ popxriglef + v2xpa_popul + v2pariglef_ord +
                v2x_api + e_gdppc + wri_l + year,  
               data = LATAM_work, Hess = TRUE)
summary(model5)
(ctable <- coef(summary(model5)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model5, "McFadden")
# Output 
stargazer(model5, title = "Regression Results", 
          type = "text")

# Model 6 Workers Right Index with Interaction and Country Fixed Effects
model6 <- polr(formula = wri_p_fact ~ popxriglef + v2xpa_popul + v2pariglef_ord +
                 v2x_api + e_gdppc + wri_l + year + factor(country_name),  
               data = LATAM_work, Hess = TRUE)
summary(model6)
(ctable <- coef(summary(model6)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# Pseudo-R-squared
PseudoR2(model6, "McFadden")
# Output 
stargazer(model6, title = "Regression Results", 
          type = "text")


### Output Workers' Rights Models
stargazer(model4, model5, model6, title = "Worker Regression Results", 
          type = "html", out = "C:results_worker.html") 


