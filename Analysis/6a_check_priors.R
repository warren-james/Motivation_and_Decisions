#### Check some priors ####
# This script will generate random beta distributions to show how the prior may look 

#### library #### 
library(tidyverse)

# settings for the priors
n <- 250 
beta_mu <- 0
beta_sd <- 0.25
gamma_mu <- 2
gamma_sd <- 0.5
x_vals <- seq(0,1.001, 0.001)

# empty frame to add to
priors <- data.frame(iter = numeric(),
                     x_vals = numeric(),
                     y = numeric())
for(ii in 1:n){
  # select from priors
  beta <- rnorm(1, beta_mu, beta_sd)
  gamma <- rnorm(1, gamma_mu, gamma_sd)
  
  # transform
  mu <- plogis(beta)
  phi <- exp(gamma)
  
  # setup distribution
  A <- mu * phi
  B <- (1 - mu) * phi
  
  # get posterior
  y <- dbeta(x_vals, A, B)
  
  # add to frame
  priors <- rbind(priors, data.frame(iter = ii,
                                     x_vals = x_vals,
                                     y = y))
}

# plot this 
priors %>% 
  ggplot(aes(x_vals, y, group = iter)) + 
  geom_line(alpha = 0.15) + 
  theme_bw() + 
  scale_x_continuous(labels = scales::percent)-> priors_plt
priors_plt$labels$x <- ""
priors_plt$labels$y <- ""
priors_plt

ggsave("../Figures/priors_plt.png",
       height = 3.5,
       width = 5.6)
