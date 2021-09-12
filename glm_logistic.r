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
# The intent is to offer some background on log-odds to not how the logit function can be derived

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


