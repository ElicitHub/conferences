## Copyright(c) 2018, Elicit. All Rights Reserved.

## ---- Setup -------------------------------------------------------------------

library(rstan)
library(tidyverse)
options(scipen = 9)    # Display without Scientific Notation

## ---- Generate data -----------------------------------------------------------

# Create a history of clicks and conversions for each brand

brand_conversions <- data_frame(  # dplyr's function for creating data frames
  
  # The brands
  brand = c("Bianchi", "Cannondale", "Cervelo", "Colnago", "Fuji", "Giant","GT",
            "Kona","Merida","Pinarello","Santa Cruz","Schwinn","Scott","Specialized","Trek","Wilier"),
  
  # All clicks in each brand's history
  clicks = c(1963,1937,2061,2011,2007,1979,1985,1995,2000,2005,2018,2025,2055,2069,3,3),
  
  # All conversions in each brand's history
  conversions = c(58,60,65,67,67,74,75,81,82,85,88,93,99,112, 1,3)) %>%
  
  #  calculate their conversion rates and 
  #  reorder the brands by their conversion rates 
  mutate(conversion_rate = conversions / clicks) %>%
  arrange(conversion_rate)

## ---- Display data -----------------------------------------------------------


# and display the table
brand_conversions 

# and display a graph of the data
brand_conversions %>%
  ggplot(aes(conversion_rate, brand)) +
  geom_point() +
  theme_minimal() +
  xlab("Conversion Rate") + ylab("Brand")


## ---- Standard Linear Regression ----------------------------------------------

# This is a binomial distribution because a customer can 
# either convert or not convert

glm_fit <- glm(
  cbind(conversions, clicks - conversions) ~ brand + 0,  # Using 0 because brand is categorical
  family = binomial(link = logit),
  data = brand_conversions
) 

# Generate summarise and confidence intervals

glm_fit_summary <- summary(glm_fit)
glm_fit_confint <- as.data.frame(confint(glm_fit))

## ---- Review Results  -----------------------------------------------------------

glm_fit_summary
glm_fit_confint

## ---- Coefficients  --------

#  and bring them into a data frame for ease of use

coeefficient_df <- as.data.frame(glm_fit_summary$coefficients) %>% 
  
  # Then the 95 confidence interval
  bind_cols(glm_fit_confint) %>%
  select(Estimate, `2.5 %`, `97.5 %`) %>%
  
  # Then convert the log back to the rate
  mutate_all(plogis) %>%
  
  # We get an NA for the upper for Wilier so we will just put the Estimate in for the upper
  mutate(`97.5 %` = ifelse(is.na(`97.5 %`), 1, `97.5 %`)) %>%
  
  # Bring in the brands
  mutate(Brand = glm_fit$data$brand)


# Let's graph it

coeefficient_df %>%
  
  # Plot the estimate, errorbar, and flip for readability
  ggplot(aes(Brand, Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  coord_flip() +
  theme_minimal()

## ---- Direct Binomial in Stan  ---- No data from the R environment


stan_code_model_only <- "
parameters {
  real rate;
}
model {
   40 ~ binomial(1001, rate);
}"

model_only_fit <- stan(model_code = stan_code_model_only)
model_only_fit

plot(model_only_fit, plotfun = "stan_trace")
plot(model_only_fit, plotfun = "stan_hist")

## ---- Binomial without priors  -----------------------------------------------------------

# Brands are categorical so we need to generate dummy variables for each brand

stan_code1 <- "
data {
  int brand_count;
  int conversions[brand_count];
  int clicks[brand_count];
  matrix[brand_count, brand_count] brands;
}
parameters {
  vector[brand_count] rate;
}
model {
  conversions ~ binomial_logit(clicks, brands * rate);
}"

# Let's compile our program first this time
stan_program1 <- stan_model(model_code = stan_code1)

# We need to create dummy variables for our brand categories
brand_matrix <- model.matrix(~ brand + conversions + clicks + 0, data = brand_conversions)
brand_matrix[1,c(1:3, 17:18)]
brand_matrix[2,c(1:3, 17:18)]
brand_matrix[3,c(1:3, 17:18)]

brand_count <- length(unique(brand_conversions$brand))

stan_fit1 <- sampling(stan_program1,
                      data = list(
                        brand_count = brand_count,
                        conversions = brand_matrix[, "conversions"],
                        clicks = brand_matrix[, "clicks"],
                        brands = brand_matrix[,1:brand_count]
                      )
)
stan_fit1

plot(stan_fit1, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_trace")
plot(stan_fit1, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_hist")


## ---- Binomial with constrained parameters ----------------------------------------------

qlogis(c(0.005, 0.04, 0.99))  # qlogis calculates the logit

stan_code2 <- "
data {
  int brand_count;
  matrix[brand_count, brand_count] brands;
  int conversions[brand_count];
  int clicks[brand_count];
}
parameters {
  vector<lower=-5,upper=5>[brand_count] rate;
}
model {
  conversions ~ binomial_logit(clicks, brands * rate);
}
"

stan_program2 <- stan_model(model_code = stan_code2)

stan_fit2 <- sampling(stan_program2, 
                      data = list(
                        brand_count = brand_count,
                        brands = brand_matrix[,1:brand_count],
                        conversions = brand_matrix[, "conversions"],
                        clicks = brand_matrix[, "clicks"])
)
stan_fit2

plot(stan_fit2, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_trace")
plot(stan_fit2, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_hist")

## ---- Binomial -- adding inverse logit ----------------------------------------------

stan_code3 <- "
data {
  int brand_count;
  matrix[brand_count, brand_count] brands;
  int conversions[brand_count];
  int clicks[brand_count];
}
parameters {
  vector<lower=-5,upper=5>[brand_count] rate;
}
model {
  conversions ~ binomial_logit(clicks, brands * rate);
}
generated quantities  {
  vector[brand_count] conversion_rate;
  conversion_rate = inv_logit(rate);
}
"

stan_program3 <- stan_model(model_code = stan_code3)

stan_fit3 <- sampling(stan_program3, 
                      data = list(
                        brand_count = brand_count,
                        brands = brand_matrix[,1:brand_count],
                        conversions = brand_matrix[, "conversions"],
                        clicks = brand_matrix[, "clicks"])
)
stan_fit3

plot(stan_fit3, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_trace")
plot(stan_fit3, pars = c('conversion_rate[1]', 'conversion_rate[15]', 'conversion_rate[16]'), plotfun = "stan_hist")

## ---- Binomial  -- adding hierarchical priors  ----------------------------------------------

stan_hier1 <- "
data {
  int brand_count;
  matrix[brand_count, brand_count] brands;
  int conversions[brand_count];
  int clicks[brand_count];
}
parameters {
  vector<lower=-5,upper=5>[brand_count] rate;  // flat prior
  real<lower=-5,upper=5> rate_mean;
  real<lower=0> rate_sd;                       // flat prior
}
model {
  conversions ~ binomial_logit(clicks, brands * rate);  // likelihood
  rate ~ normal(rate_mean, rate_sd);                    // hierarchical prior
}
generated quantities  {
  vector[brand_count] conversion_rate;
  conversion_rate = inv_logit(rate);
}
"

stan_hier_program1 <- stan_model(model_code = stan_hier1)

stan_hier_fit1 <- sampling(stan_hier_program1, 
                           data = list(
                             brand_count = brand_count,
                             brands = brand_matrix[,1:brand_count],
                             conversions = brand_matrix[, "conversions"],
                             clicks = brand_matrix[, "clicks"])
)
stan_hier_fit1

plot(stan_hier_fit1, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_trace")
plot(stan_hier_fit1, pars = c('conversion_rate[1]', 'conversion_rate[15]', 'conversion_rate[16]'), plotfun = "stan_hist")


## ---- Handle divergence -----------------

stan_hier_fit2 <- sampling(stan_hier_program1, 
                           data = list(
                             brand_count = brand_count,
                             brands = brand_matrix[,1:brand_count],
                             conversions = brand_matrix[, "conversions"],
                             clicks = brand_matrix[, "clicks"]),
                           control = list(
                             stepsize = 0.001,
                             adapt_delta = 0.999)
)
stan_hier_fit2

plot(stan_hier_fit1, pars = c('rate[1]', 'rate[15]', 'rate[16]'), plotfun = "stan_trace")
plot(stan_hier_fit1, pars = c('conversion_rate[1]', 'conversion_rate[15]', 'conversion_rate[16]'), plotfun = "stan_hist")

## ---- Extract for tidyverse tools -------------------------

posterior_matrix <- rstan::extract(stan_hier_fit1)  # Extract a posterior distribution
posterior_df <- as.data.frame(posterior_matrix) # Turn it into a dataframe for ease of use

# Fix up the names of the coefficients
names(posterior_df)[19:(19+15)] <- str_sub( colnames(brand_matrix)[1:16], 6)

# Show your new data
posterior_df %>%
  
  # first select only the coefficients
  select(19:(19+15)) %>%
  
  # Turn them into a 'long' or 'tidy' format to make comparative graphing easier
  gather(brand, rate) %>%
  
  # and plot them
  ggplot(aes(brand, rate)) +  
  geom_boxplot() +
  theme_minimal() +
  coord_flip()  # then the x-axis has long text I prefer to flip thet axes

posterior_df %>%
  select(19:(19+15)) %>%
  gather(brand, estimate) %>%
  group_by(brand) %>%
  summarise(median_price = median(estimate))


