## Advanced Stats and Modelling supporting tools ====

library(tidyverse) 
library(tidymodels)
library(openxlsx)

# Load Data 

sals <- read.xlsx("/Users/james/Downloads/wk3-2.xlsx")
sals

# Scatter Plot with differences in wages over time between male and female
# EDA
sals %>% 
  ggplot(aes(Experience, Salary)) +
  geom_point(aes(colour = factor(Male)))

# Create an interaction variable
 sals_1 <- sals %>% mutate(
   experience_male = Experience*Male
 ) %>% tibble()

 sals_1
 
 # create a lm predicting salaries against the other variables
 
 ### Build a model ====
 
 lm_mod <- linear_reg() %>% 
   set_engine("lm")
 
 lm_mod
 ### fit the model ====
 
 lm_fit <- lm_mod %>% 
   fit(Salary ~ Experience + Male + experience_male, data = sals_1)
 
 lm_fit %>% summary()
 lm_fit$fit %>% glance()
 lm_fit$fit %>% summary() %>% tidy() 
### Create Plots for Checking the assumptions (residuals) ====

# Add residual info to the DF 
sals_2 <- lm_fit$fit %>% augment(sals_1)
sals_2 
# Plot residuals  
sals_2 %>% ggplot(aes(.fitted)) +
   geom_point(aes(y = .resid), colour = "blue")
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

lm_fit_1 <- lm_mod %>% 
  fit(log_sal ~ Experience + Male + experience_male + exp_2, data = sals_3)

### Check the model ====
# overall model performance R2 etc
lm_fit_1$fit %>% glance() %>% tidy()
results_1 <- lm_fit_1$fit %>% tidy()
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
par(mfrow = c(2,2))
lm_fit_1$fit%>% plot()

lm_fit_1 
 # Percentage scales are a multiplicative relationship by using a log transformation it turns it into an additive relationship.  
sals_3 <- sals_3 %>% mutate(
  Male = as_factor(Male)
)
res_1 <- tidy(lm_fit_1)
res_1

sals_3 <- augment(lm_fit_1, sals_3, interval = "confidence")
sals_3 %>% ggplot(aes(x = Experience, y = .pred), colour = Male) +
  geom_point(aes(colour = Male)) 
