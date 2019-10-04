## Copyright(c) 2018, Elicit. All Rights Reserved.

# Setup -------------------------------------------------------------------

library(rstan)
library(tidyverse)

# Create data -------------------------------------------------------------

old_coupon_margin = 100
new_coupon_margin = 95

# Visitors and converters for 5 days of testing with the old and new coupon
# Visitors and converters are cumulative over the 5 days
old_coupon <- data_frame(
  campaign = "Old Coupon",
  day = 1:5,
  visitors = c(484, 951, 1434, 1935, 2433),
  converters = c(16, 36, 54, 77, 89))

new_coupon <- data_frame(
  campaign = "New Coupon",
  day = 1:5,
  visitors = c(468, 1017, 1508, 2020, 2567),
  converters = c(26, 52, 73, 106, 136))

experiments <- old_coupon %>%
  bind_rows(new_coupon)

# Create Stan code -------------------------------------------------------------

# Stan code for creating a binomial regression for two campaigns

conversion_rate_code <- "
data {
  int converters[2];
  int visitors[2];
  matrix[2, 2] campaign;
}
parameters {
  vector<lower=-5, upper=5>[2] logit_rate;
}
model {
  converters ~ binomial_logit(visitors, campaign * logit_rate); // likelihood
  logit_rate ~ normal(-3.178054, 0.1640332);
}
generated quantities  {
  vector[2] conversion_rate;
  conversion_rate = inv_logit(logit_rate);
}
"

# Run for each day from 1 through 5   ------------------------------------------------------

current_day = 1  # change this value for different days and run from here to the end

# include only one day's worth at time
experiment <- experiments %>%
  filter(day == current_day)

# Create the campaign matrix because campaign is categorical
campaign_matrix <- model.matrix(~ campaign + converters + visitors + 0, data = experiment)

# compile the code and sample at the same time
fit <- stan(model_code = conversion_rate_code, 
            data = list(converters = campaign_matrix[,"converters"], 
                        visitors = campaign_matrix[, "visitors"],
                        campaign = campaign_matrix[,1:2]))

# Show the results of the fit
print(fit, digits = 3)

# Pull the posterior draws into a data frame
posterior_draws <- rstan::extract(fit) %>%
  as.data.frame

# Fix up the campaign names for our probabilities and not the logit values
names(posterior_draws)[3:4] <-str_sub( colnames(campaign_matrix)[1:2], 9)

# Show your new data
posterior_draws %>%
  
  # first select only the coefficients
  select(3:4) %>%
  
  # Turn them into a 'long' or 'tidy' format to make comparative graphing easier
  gather(campaign, rate) %>%
  
  # and plot them
  ggplot(aes(rate, fill = campaign)) +  
  geom_histogram(binwidth = 0.001, position = "identity", alpha = 0.5) 


# The probability of either coupon being larger
posterior_draws %>%
  summarise(prob_new = sum(`New Coupon` > `Old Coupon`) / n(),
            prob_old = sum(`Old Coupon` >= `New Coupon`) / n())


# Review losses
# First understand the differences in percentages
posterior_draws %>%
  
  # first select only the coefficients
  mutate(Choose_New_Loss_pct = `Old Coupon` - `New Coupon`,
         Choose_New_Loss = Choose_New_Loss_pct * 100) %>%
  
  
  # and plot them
  ggplot(aes(Choose_New_Loss_pct, fill = Choose_New_Loss_pct > 0)) +  
  geom_histogram(binwidth = 0.001, alpha = 0.5) +
  theme_minimal()

# First understand the differences then dollars
posterior_draws %>%
  
  mutate(Choose_New_Loss_pct = `Old Coupon` - `New Coupon`,
         Choose_New_Loss = Choose_New_Loss_pct * 100) %>%
  
  # and plot them
  ggplot(aes(Choose_New_Loss, fill = Choose_New_Loss > 0)) +  
  geom_histogram(binwidth = 0.1, alpha = 0.5) +
  theme_minimal()

# and then calculate expected loss
posterior_draws %>%
  
  # First calculate the chance of loss
  # Dividing by 4000 because there are 4000 samples and they each 
  # have an equal chance of occurring
  mutate(Choose_New_Loss_pct = (`Old Coupon` - `New Coupon`) / 4000,
         
         # then multiply by the margin
         Choose_New_Loss = Choose_New_Loss_pct * 100) %>%
  
  # pick only those samples where the chance is > 0
  filter(Choose_New_Loss_pct > 0) %>%
  
  # Sum up to the total
  summarise(Choose_New_Loss_per_person = sum(Choose_New_Loss)) %>%
  
  # And multiply for 180 days by 1000 people per day
  mutate(Choose_New_Loss = Choose_New_Loss_per_person * 180 * 1000)

# create a loss function
loss_function <- function(chosen_draws,
                          rejected_draws,
                          rejected_margin,
                          future_period = 180,
                          daily_mean = 1000) {
  
  # Sum over the entire distribution of posterior draws
  sum(
    
    # Multiply the conversion rate difference times the margin and total possible visitors
    # Only use positive values since we are looking at maximum possible losses
    pmax((rejected_draws - chosen_draws) * rejected_margin * future_period * daily_mean, 0) / 
      
      # Each draw has equal weight
      length(chosen_draws)
  )
}


# And look at the values
posterior_draws %>%
  summarise(choosing_new_loss = loss_function(`New Coupon`, `Old Coupon`, old_coupon_margin),
            choosing_old_loss = loss_function(`Old Coupon`, `New Coupon`, new_coupon_margin),
            choosing_more = (current_day + 1) * max(loss_function(`New Coupon`, `Old Coupon`, old_coupon_margin, 1, 500),
                                                    loss_function(`Old Coupon`, `New Coupon`, new_coupon_margin, 1, 500))
  )



