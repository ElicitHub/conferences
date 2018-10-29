## Copyright(c) 2018, Elicit. All Rights Reserved.

# Setup -------------------------------------------------------------------

library(rstan)
library(tidyverse)

# Generate data -----------------------------------------------------------

# Create a history of clicks and conversions for each brand

brand_conversions <- data_frame(  # dplyr's function for creating data frames
  
  # The brands with more history
  brand = c("Bianchi", "Cannondale", "Cervelo", "Colnago", "Fuji", "Giant","GT",
            "Kona","Merida","Pinarello","Santa Cruz","Schwinn","Scott","Specialized","Trek","Wilier"),
  
  # Simulate clicks with a Poisson distribution
  clicks = c(1963,1937,2061,2011,2007,1979,1985,1995,2000,2005,2018,2025,2055,2069,3,3),
  
  # Simulate conversions with a binomial distribution where the 
  # mean of the distribution is also simulated via a normal distribution
  conversions = c(58,60,65,67,67,74,75,81,82,85,88,93,99,112, 1,3)) %>%
  
  #  calculate their conversion rates and 
  #  reorder the brands by their conversion rates 
  mutate(conversion_rate = conversions / clicks) %>%
  arrange(conversion_rate)


# Stan Program ------------------------------------------------------------

simple_hierarchical <- "
data {
  int brand_count;
  matrix[brand_count, brand_count] brands;
  int conversions[brand_count];
  int clicks[brand_count];
}
parameters {
  vector<lower=-5,upper=5>[brand_count] rate;
  real<lower=-5,upper=5> rate_mean;
  real<lower=0> rate_sd;
}
model {
  conversions ~ binomial_logit(clicks, brands * rate);
  rate ~ normal(rate_mean, rate_sd);
}
generated quantities  {
  vector[brand_count] conversion_rate;
  conversion_rate = inv_logit(rate);
}
"

# Use model.matrix to create dummy variables for categorical values

brand_matrix <- model.matrix(~ brand + conversions + clicks + 0, data = brand_conversions)
brand_count <- length(unique(brand_conversions$brand))

# Compile and sample in one function call
fit <- stan(model_code = simple_hierarchical, 
            data = list(
              brand_count = brand_count,
              brands = brand_matrix[,1:brand_count],
              conversions = brand_matrix[, "conversions"],
              clicks = brand_matrix[, "clicks"]),
            control = list(stepsize = 0.001, adapt_delta = 0.99)
)

# Check for warning and error messages

fit  # Check for n_eff > 50-100 and rhat < 1.05

plot(fit, plotfun = "stan_trace")  # Insure that the trace plots look stable
plot(fit, plotfun = "stan_hist",
     pars = c("conversion_rate[15]",
              "conversion_rate[16]"))                          # You can plot the output

posterior_matrix <- rstan::extract(fit)  # Extract a posterior distribution
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
