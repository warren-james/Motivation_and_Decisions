#### Check some priors ####
#### library #### 
library(tidyverse)

# quick example
x_vals <- seq(0,1.001,0.001)
y <- dbeta(x_vals, 2, 2)
plot(x_vals, y)

# try another way
# here's our priors
beta <- rnorm(1, mean = 0, sd = 0.25)
gamma <- rnorm(1, mean = 2, sd = 0.5) 

# here's the hyperparameters
mu <- plogis(beta)
phi <- exp(gamma)
  
# here's the shape of the distribution
A <- mu * phi
B <- (1 - mu) * phi

y <- dbeta(x_vals, A, B)
plot(x_vals, y,
     main = paste("beta = ", round(beta, digits = 3), ", gamma = ", round(gamma, digits = 3), sep = ""))


# so let's loop to make 250 random ones using values we like 
# just so we can see what other distributions are likely under the
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
  geom_line(alpha = 0.25) + 
  theme_bw() -> priors_plt
priors_plt$labels$x <- ""
priors_plt$labels$y <- ""
priors_plt

ggsave("../Figures/priors_plt.png",
       height = 3.5,
       width = 5.6)
