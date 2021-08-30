## Advanced Stats and Modelling supporting tools ====

library(tidyverse) 
library(tidymodels)
library(openxlsx)

# Load Data 
# set working directory to the base directory where both the data and the .r file are for ease
# setwd(" ")
# Read in Data from spreadsheet and convert it to a tibble to make it 'tidy' and easy to read and use
sals <- read.xlsx("wk3-2.xlsx") %>% as_tibble()

# Check Data 
sals

# Scatter Plot with differences in wages over time between male and female
# EDA
sals %>% 
  ggplot(aes(Experience, Salary)) +
  geom_point(aes(colour = factor(Male)))

# Create an interaction variable
 sals_1 <- sals %>% mutate(
   experience_male = Experience*Male, # create a binary variable that accounts for both teh differences in sex and experience -- this was apparent in the EDA above
   Male = as_factor(Male) #convert to a factor for ease in future 
 )

 sals_1
 
 # create a lm predicting salaries against the other variables
 
 ### Build a model ====
 
 lm_mod <- linear_reg() %>% 
   set_engine("lm")
 
 lm_mod
 ### fit the model ====
 
 lm_fit <- lm_mod %>% 
   fit(Salary ~ Experience + Male + experience_male, data = sals_1)
 
 lm_fit %>% tidy() # the individual variable performance
 lm_fit %>% glance()  # The summary stats for the overall model
 
### Create Plots for Checking the assumptions (residuals) ====

# Add residual info to the DF 
sals_2 <- lm_fit$fit %>% augment(sals_1) #pull the fit diagnostics for each observation to enable them to be plotted
sals_2 
lm_fit$fit 
# Plot residuals  
sals_2 %>% ggplot(aes(.fitted)) +
   geom_point(aes(y = .resid), colour = "blue")

sals_2 %>% ggplot(aes(Salary), fill = Male) +
  geom_point(aes(y = .fitted))

# Cooks Distance 
sals_2 %>% ggplot(aes(Obs)) +
  geom_col(aes(y = .cooksd)) +
  geom_hline(yintercept = 4/nrow(sals_2), col = "steelblue", lty = 2) #add line of influence

# Studentised Residuals
# points outside +/- 2 are outside 2 SD and are not meeting the assumptions of a linear model 
sals_2 %>% ggplot(aes(.fitted)) +
  geom_point(aes(y = .std.resid)) +
  geom_hline(yintercept = 2) +
  geom_hline(yintercept = -2)

# Check the shape of the residuals against experience // this should be done for each variable if possible 
sals_2 %>% ggplot(aes(Experience)) +
  geom_point(aes(y = .resid)) +
  geom_hline(aes(yintercept = 0))

### Create a model with a log^10 y variable ====

# create a log10 salary variable
sals_3 <- sals_1 %>% mutate(
  log_sal = log10(Salary), 
  exp_2 = Experience^2
) 
sals_3

### Build model ====

# use the engineered features including the 'log' of the target variable

lm_fit_1 <- lm_mod %>% 
  fit(log_sal ~ Experience + Male + experience_male + exp_2, data = sals_3)

### Check the model ====

# overall model performance R2 etc
lm_fit_1 %>% glance() %>% tidy() # each observation performance
lm_fit_1 %>% glance() # overall performance of the model - note the R2 and we haven't removed a single outlier...
lm_fit_1$fit %>% summary() #note the male variable and how it is now much more significant 
results_1 <- lm_fit_1 %>% tidy() # store the results for later usage 
results_1

### interpret results ====
# backwards convert log10 for interpretations and interpret results as percentage increases per unit of increase not inclusive of the intercept. 

results_1 %>% select(term, estimate) %>% pivot_wider(names_from = term, values_from = estimate) %>% transmute(
  intercept = 10^`(Intercept)`,
  experience = (10^Experience - 1)*100,
  male = (10^Male - 1)*100,
  exp_male = (10^experience_male  -1) *100,
  exp_2_1 = (10^exp_2 - 1) * 100
)

#Inspect plots of the results 
par(mfrow = c(2,2)) #split the plotting area into 2 x 2 to make it easy to plot the basic plots
lm_fit_1$fit%>% plot() #plot the basic plots in the

lm_fit_1 
 # Percentage scales are a multiplicative relationship by using a log transformation it turns it into an additive relationship.  

# Plot the fitted model predictions for both males and females with the features engineered
# Features altered for this model were
# - Log10 for the target variable (Salaries) to account for the leveling off as experience continues to increase // salaries can't go up forever in a linear fashion, they must plateau at some point in time (around 13 yrs for females and 17 yrs for males in our model)
# - experienced squared to account for the 'inverted U' found in the residuals in previous models
# - experience variable to account for both the male and female differences identified in the original and simple EDA

# Plot the predicted model for both males and females -- note this was done using only a linear model with 'careful' feature engineering... A simple model that is easily explainable and understood by all that appears to perform well. 

# add the model predictions to the results for plotting with a confidence interval (.95)
sals_3 <- augment(lm_fit_1, sals_3, interval = "confidence")
sals_3
# Plot the model predictions
sals_3 %>% ggplot(aes(x = Experience, y = .pred), colour = Male) +
  geom_point(aes(colour = Male)) 
