# GLM - Logistic Basics
#### James M
#### Sep 2021
#### 0.1.0
#### This will cover the basics of logistic regression modelling for binary data within a GLM framework

## load Packages ====
 
library(tidyverse)
library(tidymodels)
library(openxlsx)
## Load Data ====
 
df <- read.xlsx('bwght.xlsx') %>% tibble()
write_csv(df,"bwght.csv")
# check data
df 
df <- df %>% 
  mutate(
    bwght = as_factor(bwght)
  )

## visualise data ====
 
df %>% ggplot(aes(x = gage, y = bwght), fill = bwght) +
  geom_point(aes(colour = bwght))

## Log Odds // Logit Function ====
# The intent is to offer some background on log-odds to now how the logit function can be derived

# Count the number of positives and the number of negatives
df %>% group_by(bwght) %>% count()
# total number of observations
df %>% nrow()

# we can see that we have 17 '1's' and 7 '0's' 

17/7 # the odds of a positive outcome '1'

p <- 17/24 # the probability of a positive outcome
n <- 7/24 # the probability of a negative outcome

# This is the log of the odds
log(17/7)  

# This is the log of the probabilities - this is known as the logit function
log(p/(1-p))
# They both return the same result - .8873032

# This is particularly useful when solving for probabilities of binary outcomes win/lose, yes/no - consider decision tree applications
# The log of the odds makes things easier to interpret in addition to making things symmetrical

## Build a GLM Logistical model using Tidy Models Principles ====
### Parsnip to build the model ====
glm_log <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

### Recipe for pre-processing data ====

binary <- c("non-normal","normal")

glm_rec <- recipe(bwght ~ gage, data = df)

encoded <- glm_rec %>% prep() %>% bake(new_data = NULL)
# This gives a table with gage on the left and the number of 'failures' and 'successes' on the right. As the gage increases the success rate appears to increase as expected. 

encoded %>% table()
# this offers a way to view the success and failure cumulatively as totals
table(encoded$bwght, df$bwght)

### Create a Workflow ====

mod_wf <- workflow() %>% 
  add_recipe(glm_rec) %>% 
  add_model(glm_log)

### Fit the model ====

glm_log_fit <- fit(mod_wf, data = df)

### Assess the model ====

glm_log_fit %>% tidy()
glm_log_fit %>% pluck("fit")

### Make Predictions ====
# Create data using the existing df

new_data <- df %>% select(-bwght)

# conduct predictions of the binary variable then add the probabilities of these to the original df 
df_preds <- df %>% 
  bind_cols(predict(glm_log_fit, new_data = new_data),
            predict(glm_log_fit, new_data = new_data, type = "prob")
            )

# Determine how the model performs with accuracy and Kappa statistics https://yardstick.tidymodels.org/reference/kap.html 
df_preds %>% metrics(bwght, .pred_class)

# This is the long hand calc for the probability that at 40wks (gage) normal weight will be achieved. 

exp(-48.9085 + (1.3127 *40)) / (1 + (exp(-48.9085 + (1.3127 *40))))

df_preds %>% roc_auc(bwght, .pred_0) #gives an answer of .908


# Plot the roc curve for visual assessment of how well the model discriminates
df_preds %>% roc_curve(bwght, .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  geom_text(aes(x = .5,
                y = .25,
                label = "ROC_AUC = 0.908"), stat = "unique")

## New Model with Health Data ====
 
### Import Data ====

df_health <- read.csv("health.csv") %>% as_tibble()
df_health

df_health <- df_health %>% 
  filter(car != 9) %>% # remove response variables that don't contribute to the model - did not answers.. 
  mutate(
    female = if_else(gender == 2,1,0) %>% as_factor(), # binary outcome coding for logistic modelling
    acc_car = if_else(car == 2,0,1) %>% as_factor(),
    marstat = as_factor(marstat)# binary outcome coding for logistic modelling
  )

### Build a Model ====
glm_log_hlth <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

### Recipe for Model ====

glm_rec_hlth <- recipe(acc_car ~ female + age + marstat, data = df_health) %>% 
  step_dummy(marstat) 
  prep() %>% 
  bake(df_health)

glm_rec_hlth

### Workflow for Health Model ====

mod_wf_hlth <- workflow() %>% 
  add_recipe(glm_rec_hlth) %>% 
  add_model(glm_log_hlth)

### Fit the Model ====

glm_log_hlth_fit <- fit(mod_wf_hlth, data = df_health)

### Assess the Model ====

glm_log_hlth_fit %>% tidy()
glm_log_hlth_fit %>% pluck("fit")
glm_log_hlth_fit %>% anova()

### Make Predictions on original Data ====
# Note this data wasn't split into train and test sets which isn't best practice
# this is something that would need to be remedied if the model was to be generalised
glm_hlth_preds <- bind_cols(df_health, glm_log_hlth_fit %>% predict(df_health),
                            df_health, glm_log_hlth_fit %>% predict(df_health,
                                                                    type = "prob"))

glm_hlth_preds

### Assess the model ====

conf_mat(glm_hlth_preds, truth = acc_car...6, estimate = .pred_class)
accuracy(glm_hlth_preds, truth = acc_car...6, estimate = .pred_class)
f_meas(glm_hlth_preds, truth = acc_car...6, estimate = .pred_class)

### ROC ====

glm_hlth_preds %>% roc_auc(acc_car...6, .pred_0) # ROC value of .771

p1 <- glm_hlth_preds %>% roc_curve(acc_car...6, .pred_0)
p1 %>% autoplot()

## Future Work ====
 
# group variables in the health data set based on marital status to determine
# the best fit for the model as - consider the data pre-process in this instance.


