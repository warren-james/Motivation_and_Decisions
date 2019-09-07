#### Check some priors ####
# This script will generate random beta distributions to show how the prior may look 

#### library #### 
library(tidyverse)
library(extraDistr)

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


#### Make priors using Part 1 ####
load("scratch/Part1_peng") 
df_peng <- df %>%
  mutate(participant = paste(participant, "penguin", sep = "_"))
load("scratch/Part1_cont")
df_part1 <- df %>%
  mutate(participant = paste(participant, "control", sep = "_")) %>%
  rbind(df_peng)

# tidy
rm(df, df_peng)

# make a quick plot?
m_dat <- df_part1 %>%
  group_by(participant) %>%
  summarise(accuracy = mean(accuracy))
  
m_dat %>% 
  ggplot(aes(accuracy)) +
  geom_density()  

# fit distribution
prior_est <- fitdistrplus::fitdist(m_dat$accuracy, "beta")
a <- as.numeric(prior_est$estimate[1])
b <- as.numeric(prior_est$estimate[2])


# settings for the priors
n <- 250 
# mean of distribution
mu <- a/(a+b)
# transform for prior
beta_mu <- gtools::logit(mu)
beta_sd <- .4
# get precision
phi <- 1/((a*b)/(((a+b)^2)*(a+b+1)))
# transform for prior
gamma_mu <- log(phi)
gamma_sd <- 1.4
x_vals <- seq(0,1.001, 0.001)

# for plotting real shape
temp <- data.frame(x = x_vals,
                   y = dbeta(x_vals, a, b))

# plot raw data and fit output 
m_dat %>%
  ggplot(aes(accuracy)) + 
  geom_density(fill = "black", alpha = 0.3) + 
  geom_line(data = temp, 
            aes(x, y),
            colour = "blue")

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
  ggplot(aes(x_vals, y)) + 
  geom_line(alpha = 0.15, aes(group = iter)) +
  theme_bw() + 
  scale_x_continuous(labels = scales::percent)-> priors_plt
priors_plt$labels$x <- ""
priors_plt$labels$y <- ""
priors_plt

priors %>% group_by(iter) %>% filter(y == max(y)) %>% ggplot(aes(x_vals)) + geom_histogram() 

# save this
ggsave("../Figures/priors_plt_estfromdata.png",
       height = 3.5,
       width = 5.6)

# show raw data overlay
priors_plt + geom_line(data = temp,
                       aes(x,y),
                       colour = "blue",
                       size = 1.5,
                       linetype = 3,
                       size = 0.7)




