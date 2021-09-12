# GLM Basics
#### James
#### Aug 2021
#### 1.0.0
#### This is an R script to cover the basics of GLM with a focus on when it is appropriate to use GLM compared with basic linear regression. 


## Load Packages ====

library(tidymodels)
library(tidyverse)
library(openxlsx)
## Background ====

# On occasion linear regression isn't appropriate for modelling a problem. This is where Maximum Likelihood estimation is appropriate as a precursor to the use of a GLM.
### Key Points ====

# Linear regression assumes that variance is constant, possibly from a normal distribution. There are many Data types where variance (randomness) isn't constant. The Ordinary Least Squares (OLS) method for linear regression isn't appropriate - in the instances below the Maximum Likelihood Estimate (MLE) is appropriate.  

# Three examples where Linear Regression isn't appropriate are:

# - The response variable is a proportion (multinomial) ranging between 0 and 1 // for proportions of a number of counts the binomial distribution may be appropriate. Consider binary data, yes/no responses, coin tosses, present/ absent etc

# - The response is a count. Counts are non-negative and discrete and as the count approaches 0 the variance must approach 0. Consider Poisson distributions for count data.

# - The response is positive continuous. Positive continuous data are often right skewed and as the the response approaches 0 the variance approaches 0. Consider *Gamma and Inverse Gaussian* distributions in this instance.

# There are three components of a GLM:
# - The random component -- the probability distribution of the response variable
# - The systematic component -- the explanatory variables
# - The link function -- specifies the link between the random and systemic components. This is different for the different distributions. 

#### What is Maximum Likelihood Estimates (MLE)? ==== 

# In ordinary linear regression the OLS method is used to estimate the parameters of the model. OLS is appropriate if the Data is approximately normally distributed. MLE can be applied to data that isn't normally distributed and can be applied across more kinds fo data in conjunction with Generalised Linear Models (GLM). The family of distributions used for GLMs is known as the exponential distribution model (EDM) which includes, binomial, normal, Poisson and Gamma distributions among others. 

# The Idea of MLE is to choose those estimates for the unknown parameters that maximise the probability density of the observed data. 

### A Binary Example - Coin Tosses ====
# Generate a set of random 'coin toss' data (1,0) 

# input number of coin tosses 
n <- 60
# input the chance of a head
w <- .30

coin_toss <- rbinom(n, 1, w) %>% as_tibble_col(column_name = "heads")
n <- nrow(coin_toss)
y <- sum(coin_toss$heads)

# this will run 30 coin tosses 10 times
rol_av <- tibble(p_hat = integer(), av = integer())
ave_p <- tibble(liklie = integer(),log_liklie = integer(), P_estimate = integer(), sd_p = integer())
for (i in 1:10){
  #This function replicates a series of coin tosses as a set of binomial data before calculating the p-hat for each set of coin tosses in addition to a a rolling average for p-hat across each set of coin tosses. 
  coin_toss <- rbinom(n, 1, w) %>% as_tibble_col(column_name = "heads")
  n <- nrow(coin_toss)
  y <- sum(coin_toss$heads)
  p_hat = y/n
  row = tibble(p_hat)
  rol_av <- rol_av %>% 
    add_row(row) %>% 
    mutate(
      av = cumsum(p_hat)/seq_along(p_hat)
    )
  log_likelihood <- tibble("P_estimate" = (seq(0.05,.95, by = .05)))
  liklie <- dbinom(y,n,log_likelihood$P_estimate)
  log_liklie <- liklie %>% log10()
  log_likelihood <- log_likelihood %>% add_column(liklie, log_liklie)
  log_likelihood <- log_likelihood %>% arrange(desc(liklie))
  # get the most likely p for each trail
  ave_p <- add_row(ave_p, log_likelihood[1,])
  ave_p <- ave_p %>% 
    mutate(
      av_p = cumsum(ave_p$P_estimate)/ seq_along(ave_p$P_estimate),
      sd_p = sd(ave_p$P_estimate)
    )
  rol_av <- rol_av %>% 
    mutate(
      P_est = ave_p$P_estimate,
      P_est_av = ave_p$av_p
    )
}
rol_av 
ave_p
#create a tibble to build the other estimates around
log_likelihood <- tibble("P_estimate" = (seq(0.05,.95, by = .05)))

# create the likelihood as a column (vector)
liklie <- dbinom(y,n,log_likelihood$P_estimate)

#create the log(likelihood)10 as a column (vector)
log_liklie <- liklie %>% log10()

# Add the columns to the original tibble
log_likelihood <- log_likelihood %>% add_column(liklie, log_liklie)

log_likelihood %>% arrange(desc(liklie), log_liklie)

# Plot the likelihood
log_likelihood %>% ggplot(aes(P_estimate)) +
  geom_line(aes(y = liklie))

# Plot the log likelihood
log_likelihood %>% ggplot(aes(P_estimate)) +
  geom_line(aes(y = log_liklie))

#### Interpretation of the Results ==== 

#GLM - MLE will give a different answer to OLS in occasions where there are either small data sets or there are a number of different features - the details are in the n vs the n-1 that is in the estimation of the variance methods. 

# A rule of thumb is when the data is normally distributed then use OLS - MLE and ultimately GLM should be used for non-normally distributed data.  

