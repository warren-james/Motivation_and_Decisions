#### For plotting model outputs ####

#### Library ####
library(brms)
library(rethinking)
library(psych)
library(rstan)
library(tidybayes)
library(tidyverse)
library(Rlab)

#### constants ####
Screen_dist <- 60
x_res <- 1920
x_width <- 54
ppcm <- x_res/x_width

# NB: setting seed to make results reproducible
set.seed(12345)

#### Functions #### 
# get visual degrees
get_VisDegs <- function(size,distance){
  ((2*atan2(size,(2*distance)))*180)/pi
}

# get posterior preds for beta dist 
post_preds_beta <- function(model, x_vals, m_matrix){
  post <- rstan::extract(model)
  
  beta <- colMeans(post$beta)
  gamma <- colMeans(post$gamma)
  
  mu  <- plogis(m_matrix %*% beta)
  phi <- exp(m_matrix %*% gamma)
  
  A <- mu * phi 
  B <- (1 - mu) * phi
  
  p <- unlist(map2(A, B, dbeta, x = x_vals))
  
  hpdi_dist <- data.frame(lower = numeric(),
                          upper = numeric())
  for(ii in 1:3){
    temp <- as.numeric(HDInterval::hdi(rbeta(1000, A[ii], B[ii])))
    hpdi_dist <- rbind(hpdi_dist, data.frame(lower = temp[1],
                                             upper = temp[2]))
  }
  
  result <- list("p" = p, "hpdi_dist" = hpdi_dist)
  return(result)
}


# plt posterior_beta
plt_post_beta <- function(model_df, values, model, m_matrix){
  plt_posterior <- tibble(
    group = rep(unique(model_df$group), each = length(values)),
    x = rep(values, 3),
    p = post_preds_beta(model, values, m_matrix)$p) %>%
    ggplot(aes(x, p, colour = group, fill = group)) + 
    geom_area(position = "dodge", alpha = 0.3) +
    theme_minimal() +  
    ggthemes::scale_color_ptol() + 
    ggthemes::scale_fill_ptol() + 
    theme(legend.position = "bottom")
  plt_posterior$labels$x <- "Posterior Distribution"
  plt_posterior$labels$y <- "density"
  return(plt_posterior)
}

# plotting mean effect 
plt_mu_beta <- function(samples, m_matrix){
  # get mu estimates
  mu <- array(0, dim = c(nrow(samples$beta), nrow(m_matrix)))
  for (ii in 1:nrow(samples$beta)) {
    mu[ii, ] <- plogis(m_matrix %*% samples$beta[ii, ])
  }
  
  # make data frame of this 
  mu_df <- as.tibble(mu) %>%
    `colnames<-`(c("control", "motivated", "optimal")) %>%
    gather(key = "group",
           value = "p_mu")
  
  # get hdpi
  hpdi_mu <- as.tibble(t(purrr::map_df(as.tibble(mu), hdi))) %>%
    cbind(tibble(group = c("control", "motivated", "optimal"))) %>%
    `colnames<-`(c("lower", "upper", "group")) %>%
    select(group, lower, upper)
  hpdi_mu
  
  # make plt
  plt_mu <- mu_df %>%
    ggplot(aes(p_mu, colour = group, fill = group)) +
    geom_density(alpha= 0.3) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    ggthemes::scale_color_ptol() +
    ggthemes::scale_fill_ptol()
  plt_mu$labels$colour <- "Group"
  plt_mu$labels$fill <- "Group"
  plt_mu$labels$x <- "Predicted Mean Accuracy"
  plt_mu
  
  my_list <- list(mu, mu_df, hpdi_mu, plt_mu)
  return(my_list)
}

# plt shaded_mu 
plt_shaded_mu_beta <- function(data_mu, data_hpdi, data_posterior){
  # get mu line
  mu_line <- data.frame(group = character(),
                        x = numeric(),
                        y = numeric())
  
  # get density profile
  for(ii in unique(data_mu$group)){
    temp <- filter(data_mu[data_mu$group == ii,])
    
    x <- density(temp$p_mu)$x
    y <- density(temp$p_mu)$y
    
    mu_line <- rbind(mu_line, data.frame(group = ii,
                                         x = x,
                                         y = y))
  }
  
  # now make plot
  plt_shaded_mu <- merge(mu_line, data_hpdi) %>%
    mutate(variable = ifelse(x > lower & x < upper, 1, 0))
  plt_shaded_mu <- ggplot(data_posterior,
                          aes(colour = group,
                              fill = group)) +
    geom_line(aes(x, p)) + 
    geom_area(data = filter(plt_shaded_mu, variable == 1), 
              position = "dodge",
              aes(x = x,
                  y = y),
              alpha = 0.3) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    ggthemes::scale_color_ptol() +
    ggthemes::scale_fill_ptol() + 
    scale_x_continuous(limits = c(0.5, 0.9))
  plt_shaded_mu$labels$colour <- "Group"
  plt_shaded_mu$labels$fill <- "Group"  
  plt_shaded_mu$labels$x <- "Predicted Accuracy"
  plt_shaded_mu$labels$y <- "Density"
  plt_shaded_mu
  
  return(plt_shaded_mu)
}

# plt difference for means
plt_diff_beta <- function(mu){
  plt_diff <- tibble(control = mu[,1],
                     motivated = mu[,2],
                     optimal = mu[,3]) %>%
    mutate("Motivated - Control" = motivated - control,
           "Optimal - Control" = optimal - control,
           "Optimal - Motivated" = optimal - motivated) %>%
    select(-control,
           -motivated,
           -optimal) %>%
    gather(key = "Comparison",
           value = "Difference") %>%
    ggplot(aes(Difference,
               colour = Comparison,
               fill = Comparison)) +
    geom_density(alpha = 0.3) +
    geom_vline(xintercept = 0,
               linetype = "dashed") +
    ggthemes::scale_color_ptol() +
    ggthemes::scale_fill_ptol() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # return plt
  return(plt_diff)
}

#### NB: need to sort this to be a distribution ####
# post for berno 
post_berno <- function(model, x_vals, m_matrix){
  post <- rstan::extract(model) 
  
  beta <- colMeans(post$beta)
  
  mu <- m_matrix %*% beta
  
  # sort this to be a distribution over x_vals
  # ... not sure it will work with this one? 
  p <- plogis(mu)
  
  
  return(p)
}

#### PLOTTING MODELS ####
#### BETA ####
# setup effects
X <- tibble(intercept = c(1,1,1),
            motivated = c(0,1,0),
            optimal = c(0,0,1))
X <- as.matrix(X)

# sequence to estimate likelihood 
x_vals <- seq(0,1-0.001,0.001)


#### m1: acc ~ group ####
load("modelling/model_data/beta_1")
load("modelling/model_outputs/m_stan_group_beta_1")
samples <- rstan::extract(m_stan_group)

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, m_stan_group, X)
plt_posterior <- plt_posterior + 
  scale_x_continuous(limits = c(0.5, 0.9)) + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_rawacc.png",
       height = 3.5,
       width = 5.6)



# get predictions for mu
mu_list <- plt_mu_beta(samples, X)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]]
plt_mu <- mu_list[[4]]
plt_mu

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu)
plt_diff

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff, ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_rawacc_compare.png",
       height = 5,
       width = 13)

#### m2: pred_acc ~ group ####
load("modelling/model_data/beta_2")
load("modelling/model_outputs/m_stan_group_beta_2")
samples <- rstan::extract(m_stan_group_exp)

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, m_stan_group_exp, X)
plt_posterior <- plt_posterior + 
  scale_x_continuous(limits = c(0.5, 0.9)) + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_expacc.png",
       height = 3.5,
       width = 5.6)



# get predictions for mu
mu_list <- plt_mu_beta(samples, X)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]]
plt_mu <- mu_list[[4]]
plt_mu

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu)
plt_diff

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff, ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_expacc_compare.png",
       height = 5,
       width = 13)

#### BERNOULLI ####
# setup effects
X <- tibble(intercept = c(1,1,1),
            motivated = c(0,1,0),
            optimal = c(0,0,1))
X <- as.matrix(X)

# sequence to estimate likelihood 
x_vals <- seq(0,1-0.001,0.001)

#### m1: correct ~ group ####
load("modelling/model_data/berno_1")
load("modelling/model_outputs/m_stan_berno_1")

# get samples
samples <- rstan::extract(m_stan_berno)

# this is for mu
posterior <- as.tibble(samples$beta) %>% 
  `colnames<-`(c("beta_1", "beta_2", "beta_3")) %>%
  mutate(control = logistic(beta_1), 
         motivated = logistic(beta_1 + beta_2),
         optimal = logistic(beta_1 + beta_3)) %>%
  select(control, motivated, optimal) %>%
  gather(key = "group",
         value = "mu") %>% 
  ggplot(aes(mu,
             colour = group,
             fill = group)) + 
  geom_density(alpha = 0.3)
posterior

# try a simulation using this method?
beta <- colMeans(samples$beta)
# beta <- samples$beta

plt_berno <- tibble(group = rep(c("control", "motivated", "optimal"), each = 1000))
# plt_berno <- tibble(group = rep(c("control", "motivated", "optimal"), each = 1000*100))

beta[2] <- beta[2] + beta[1]
beta[3] <- beta[3] + beta[1]
# beta[,2] <- beta[,2] + beta[,1]
# beta[,3] <- beta[,3] + beta[,1]

p <- c()
for(y in 1:3){
  for(ii in 1:1000){
    est = sum(rbernoulli(1000, logistic(beta[y])))/1000
    p <- c(p, est)
  }
}


# for(y in 1:3){
#   for(x in 1:1000){
#     for(ii in 1:100){
#       est = sum(rbernoulli(1000, logistic(beta[x,y])))/1000
#       p <- c(p, est)
#     }
#   }
# }
plt_berno <- cbind(plt_berno, p)
plt_berno <- plt_berno %>% 
  ggplot(aes(p,
             colour = group,
             fill = group)) + 
  geom_density(alpha = 0.3) + 
  ggthemes::scale_color_ptol() + 
  ggthemes::scale_fill_ptol() + 
  theme_minimal() + 
  theme(legend.position = "bottom")
plt_berno

#### m2: correct ~ (group + dist_type)^2 ####
# setup effects 
X <- tibble(control = rep(c(1,1,1),2),
            motivated = rep(c(0,1,0),2),
            optimal = rep(c(0,0,1),2),
            control_f = rep(c(0,1), each = 3),
            moti_f = ifelse(motivated == 1 & control_f == 1, 1, 0),
            opt_f = ifelse(optimal == 1 & control_f == 1, 1, 0))

X <- as.matrix(X)

# load in models 
load("modelling/model_data/berno_2")
load("modelling/model_outputs/m_stan_berno_2")

# get samples
samples <- rstan::extract(m_stan_berno_dt)

post_berno(m_stan_berno_dt, 1, X)

beta <- colMeans(samples$beta)

plt_berno <- tibble(group = rep(c("control", "motivated", "optimal"), each = 2000),
                    dist_t = rep(c("close", "far"), each = 1000, 3))

for(ii in 2:length(beta)){
  beta[ii] <- beta[ii] + beta[1]
}

##### sort this ####
# p <- c()
# for(ii in 1:3){
#   for
#   for(i in 1:1000){
#     est = sum(rbernoulli(1000, logistic(beta[ii])))/1000
#     p <- c(p, est)
#   }
# }

plt_berno <- cbind(plt_berno, p)
