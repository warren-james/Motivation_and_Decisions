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
  for(ii in 1:ncol(m_matrix)){
    temp <- as.numeric(HDInterval::hdi(rbeta(1000, A[ii], B[ii])))
    hpdi_dist <- rbind(hpdi_dist, data.frame(lower = temp[1],
                                             upper = temp[2]))
  }
  
  result <- list("p" = p, "hpdi_dist" = hpdi_dist)
  return(result)
}


# plt posterior_beta 
# NB only for discrete factors
plt_post_beta <- function(model_df, values, model, m_matrix){
  list_unique <- c()
  list_names <- c()
  effects <- model_df %>%
    ungroup() %>%
    select(-1, -ncol(model_df))
  for(ii in colnames(effects)){
    list_unique <- c(list_unique, length(unique(effects[[ii]])))
    list_names <- c(list_names, ii)
  }
  # list_product
  list_product <- prod(list_unique)
  # sort out list for appropriate list building
  if(length(list_unique)>1){
    list_unique <- abs(list_unique - sum(list_unique))
  } else {
    list_unique = 1
  }

  plt_dat <- tibble(remove = rep(0, prod(list_unique)))
  count <- 0
  for(ii in list_names){
    count <- count + 1
    if(count == 1){
      temp <- rep(unique(effects[[ii]]), each = length(values)*list_unique[count])
    } else {
      temp <- rep(rep(unique(effects[[ii]]), each = length(values)), list_unique[count])
    }

    plt_dat <- cbind(plt_dat, temp)
  }
  # plt_dat <- tibble(
  #   group = rep(unique(model_df$group), each = length(values)),
  #   x = rep(values, length(unique(model_df$group))),
  #   p = post_preds_beta(model, values, m_matrix)$p
  # )
  plt_posterior <- plt_dat %>%
    `colnames<-`(c("remove", unique(list_names))) %>%
    select(-remove) %>%
    mutate(x = rep(values, list_product),
           p = post_preds_beta(model, values, m_matrix)$p) %>%
  # plt_posterior <- plt_dat %>%
    ggplot(aes(x, p, colour = group, fill = group)) + 
    geom_area(position = "dodge", alpha = 0.3) +
    theme_minimal() +
    ggthemes::scale_color_ptol() +
    ggthemes::scale_fill_ptol() +
    scale_x_continuous(limits = c(0.4, 1),
                       breaks = seq(.4, 1, .1),
                       labels = scales::percent_format(accuracy = 1)) + 
    theme(legend.position = "bottom")
  if(length(list_names)>1){
    plt_posterior <- plt_posterior + facet_wrap(~acc_type)
  }
  
  plt_posterior$labels$x <- "Posterior Distribution"
  plt_posterior$labels$y <- "density"
  return(plt_posterior)
}

# plotting mean effect 
plt_mu_beta <- function(samples, m_matrix, model_df){
  # get mu estimates
  mu <- array(0, dim = c(nrow(samples$beta), nrow(m_matrix)))
  for (ii in 1:nrow(samples$beta)) {
    mu[ii, ] <- plogis(m_matrix %*% samples$beta[ii, ])
  }
  
  # get column names 
  col_names <- model_df %>% 
    select(-1, -ncol(model_df))
  col_names <- colnames(col_names)
  
  # setup data frame of mu
  mu_df <- as.tibble(mu) %>% 
    `colnames<-`(colnames(m_matrix)) %>%
    gather(key = "temp",
           value = "p_mu") %>% 
    separate(temp, 
             col_names,
             sep = ":")
  
  # setup hpdi for mu
  hpdi_mu <- as.tibble(t(purrr::map_df(as.tibble(mu), hdi))) %>% 
    cbind(tibble(temp = colnames(m_matrix))) %>% 
    `colnames<-`(c("lower", "upper", "temp")) %>% 
    separate(temp,
             col_names,
             sep = ":")
  
  
  # make data frame of this 
  # mu_df <- as.tibble(mu) %>%
  #   `colnames<-`(c("control", "motivated", "optimal")) %>%
  #   gather(key = "group",
  #          value = "p_mu")
  
  # get hdpi
  # hpdi_mu <- as.tibble(t(purrr::map_df(as.tibble(mu), hdi))) %>%
  #   cbind(tibble(group = c("control", "motivated", "optimal"))) %>%
  #   `colnames<-`(c("lower", "upper", "group")) %>%
  #   select(group, lower, upper)
  # hpdi_mu
  

  
  # make plt
  plt_mu <- mu_df %>%
    ggplot(aes(p_mu, colour = group, fill = group)) +
    geom_density(alpha= 0.3) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    ggthemes::scale_color_ptol() +
    ggthemes::scale_fill_ptol() + 
    scale_x_continuous(limits = c(0.5, 1),
                       labels = scales::percent_format(accuracy = 1))
  if(length(col_names) > 1){
    plt_mu <- plt_mu + facet_wrap(~mu_df[[col_names[[2]]]])
  }
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
    scale_x_continuous(limits = c(0.5, 1),
                       labels = scales::percent_format(accuracy = 1))
  plt_shaded_mu$labels$colour <- "Group"
  plt_shaded_mu$labels$fill <- "Group"  
  plt_shaded_mu$labels$x <- "Predicted Accuracy"
  plt_shaded_mu$labels$y <- "Density"
  plt_shaded_mu
  
  return(plt_shaded_mu)
}

# plt difference for means
plt_diff_beta <- function(mu, m_matrix){
  # setup data
  temp_data <- tibble(control = mu[,1],
                     motivated = mu[,2],
                     optimal = mu[,3]) %>%
    mutate("Motivated - Control" = motivated - control,
           "Optimal - Control" = optimal - control,
           "Optimal - Motivated" = optimal - motivated) %>%
    select(-control, -motivated, -optimal)
  
  # get mean diff 
  mean_diff <- temp_data %>% 
    gather("Comparison", 
           "Difference") %>%
    group_by(Comparison) %>% 
    summarise(mean_diff = mean(Difference)) 
  
  # make plot
  plt_diff <- temp_data %>%
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
    theme(legend.position = "bottom") + 
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  
  # get HPDI of difference 
  hpdi_diff <- temp_data 
  col_names <- as.tibble(colnames(hpdi_diff))
  hpdi_diff <- as.tibble(t(purrr::map_df(hpdi_diff, hdi))) %>% 
    cbind(col_names) %>% 
    `colnames<-`(c("lower", "upper", "Comparison")) %>%
    merge(mean_diff)
  
  # return this
  my_list <- list(plt_diff, hpdi_diff)
  return(my_list)
}


#### PLOTTING MODELS ####
#### BETA ####
# setup effects
X <- tibble(control = c(1,1,1),
            motivated = c(0,1,0),
            optimal = c(0,0,1))
X <- as.matrix(X)

# sequence to estimate likelihood 
x_vals <- seq(0,1-0.001,0.001)


#### m1: acc ~ group ####
#### m1: First no priors ####
load("modelling/model_data/beta_1")
load("modelling/model_outputs/m_stan_group_beta_acc")
model <- m_stan_group
samples <- rstan::extract(model)

# sort out group labels 
model_data <- model_data %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, model, X)
plt_posterior <- plt_posterior + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_rawacc_np.png",
       height = 3.5,
       width = 5.6)


# get predictions for mu
mu_list <- plt_mu_beta(samples, X, model_data)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]] %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))
plt_mu <- mu_list[[4]]
plt_mu

# plot with hpdi lines drawn
max_height <- plt_posterior[["data"]] %>% 
  group_by(group) %>%
  mutate(height = max(p)/5) %>%
  summarise(height = unique(height)) %>%
  merge(hpdi_mu) %>%
  ungroup()

# add in line for hpdi 
plt_posterior + 
  geom_segment(data = max_height, 
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2,
               lineend = "round") + 
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0.5, units = "cm"))

ggsave("../Figures/Model_stan_rawacc_hpdi_np.png",
       height = 3.5,
       width = 5.6)

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu, X)
height <- plt_diff[[1]][["data"]] %>% 
  group_by(Comparison) %>% 
  summarise(height = mean(Difference)*15) %>% 
  merge(plt_diff[[2]])

plt_diff[[1]] + 
  geom_segment(data = height,
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2, 
               lineend = "round")

# get hpdi 
hpdi_diff <- plt_diff[[2]]
hpdi_diff 

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff[[1]], ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_rawacc_compare_np.png",
       height = 5,
       width = 13)

# tidy 
rm(height,
   hpdi_diff,
   hpdi_mu, 
   max_height,
   mu,
   mu_df,
   mu_list,
   plt_diff,
   plt_mu,
   plt_posterior,
   plt_save,
   plt_shaded_mu,
   samples)

#### m1: Now with priors ####
load("modelling/model_outputs/m_stan_group_beta_acc_pdata")
model <- m_stan_group_pdata
samples <- rstan::extract(model)

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, model, X)
plt_posterior <- plt_posterior + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_rawacc_pdata.png",
       height = 3.5,
       width = 5.6)


# get predictions for mu
mu_list <- plt_mu_beta(samples, X, model_data)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]] %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))
plt_mu <- mu_list[[4]]
plt_mu

# plot with hpdi lines drawn
max_height <- plt_posterior[["data"]] %>% 
  group_by(group) %>%
  mutate(height = max(p)/5) %>%
  summarise(height = unique(height)) %>%
  merge(hpdi_mu) %>%
  ungroup()

# add in line for hpdi 
plt_posterior + 
  geom_segment(data = max_height, 
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2,
               lineend = "round") + 
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0.5, units = "cm"))

ggsave("../Figures/Model_stan_rawacc_pdata_hpdi.png",
       height = 3.5,
       width = 5.6)

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu, X)
height <- plt_diff[[1]][["data"]] %>% 
  group_by(Comparison) %>% 
  summarise(height = mean(Difference)*15) %>% 
  merge(plt_diff[[2]])

plt_diff[[1]] + 
  geom_segment(data = height,
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 1, 
               lineend = "round")

# get hpdi 
hpdi_diff <- plt_diff[[2]]
hpdi_diff 

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff[[1]], ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_rawacc_compare.png",
       height = 5,
       width = 13)

# tidy 
rm(height,
   hpdi_diff,
   hpdi_mu, 
   max_height,
   mu,
   mu_df,
   mu_list,
   plt_diff,
   plt_mu,
   plt_posterior,
   plt_save,
   plt_shaded_mu,
   samples)

#### m2: pred_acc ~ group ####
#### m2: no priors ####
load("modelling/model_data/beta_2")
load("modelling/model_outputs/m_stan_group_beta_exp_np")
model <- m_stan_group_exp
samples <- rstan::extract(model)

# sort out group labels
model_data <- model_data %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, model, X)
plt_posterior <- plt_posterior + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_expacc_np.png",
       height = 3.5,
       width = 5.6)

# get predictions for mu
mu_list <- plt_mu_beta(samples, X, model_data)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]] %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))
plt_mu <- mu_list[[4]]
plt_mu

# plot with hpdi lines drawn
max_height <- plt_posterior[["data"]] %>% 
  mutate(height = max(p)/5) %>% 
  group_by(group) %>% 
  summarise(height = unique(height)) %>% 
  mutate(height = ifelse(group == "Control", height - .5, height)) %>%
  merge(hpdi_mu)

# add in line for hpdi 
plt_posterior + 
  geom_segment(data = max_height, 
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2,
               lineend = "round") + 
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0.5, units = "cm"))


ggsave("../Figures/Model_stan_expacc_hpdi_np.png",
       height = 3.5,
       width = 5.6)

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu, X)
height <- plt_diff[[1]][["data"]] %>% 
  group_by(Comparison) %>% 
  summarise(height = mean(Difference)*15) %>% 
  merge(plt_diff[[2]])

plt_diff[[1]] + 
  geom_segment(data = height,
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2, 
               lineend = "round")

# get hpdi 
hpdi_diff <- plt_diff[[2]]
hpdi_diff 

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff[[1]], ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_expacc_compare_np.png",
       height = 5,
       width = 13)

# tidy 
rm(height,
   hpdi_diff,
   hpdi_mu, 
   max_height,
   mu,
   mu_df,
   mu_list,
   plt_diff,
   plt_mu,
   plt_posterior,
   plt_save,
   plt_shaded_mu,
   samples)

#### m2: w/ priors ####
load("modelling/model_outputs/m_stan_group_beta_exp_pdata")
model <- m_stan_group_exp_pdata
samples <- rstan::extract(model)

# plt posterior
plt_posterior <- plt_post_beta(model_data, x_vals, model, X)
plt_posterior <- plt_posterior + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior

# save 
ggsave("../Figures/Model_stan_expac_wp.png",
       height = 3.5,
       width = 5.6)

# get predictions for mu
mu_list <- plt_mu_beta(samples, X, model_data)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]] %>% 
  mutate(group = ifelse(group == "optimal", "Optimal",
                        ifelse(group == "motivated", "Motivated", "Control")))
plt_mu <- mu_list[[4]]
plt_mu

# plot with hpdi lines drawn
max_height <- plt_posterior[["data"]] %>% 
  group_by(group) %>% 
  mutate(height = max(p)/5) %>% 
  summarise(height = unique(height)) %>%
  ungroup() %>%
  merge(hpdi_mu)

# add in line for hpdi 
plt_posterior + 
  geom_segment(data = max_height, 
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2,
               lineend = "round") + 
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(0.5, units = "cm"))


ggsave("../Figures/Model_stan_expacc_hpdi_wp.png",
       height = 3.5,
       width = 5.6)

# extract density information for this? 
plt_shaded_mu <- plt_shaded_mu_beta(plt_mu[["data"]], hpdi_mu, plt_posterior[["data"]])
plt_shaded_mu


# looking at differences 
plt_diff <- plt_diff_beta(mu, X)
height <- plt_diff[[1]][["data"]] %>% 
  group_by(Comparison) %>% 
  summarise(height = mean(Difference)*15) %>% 
  merge(plt_diff[[2]])

plt_diff[[1]] + 
  geom_segment(data = height,
               aes(x = lower, y = height,
                   xend = upper, yend = height),
               size = 2, 
               lineend = "round")

# get hpdi 
hpdi_diff <- plt_diff[[2]]
hpdi_diff 

# side by side for paper 
plt_save <- gridExtra::grid.arrange(plt_posterior, plt_diff[[1]], ncol = 2)
ggsave(plt_save, file = "../Figures/Model_stan_expacc_compare_np.png",
       height = 5,
       width = 13)

# tidy 
rm(height,
   hpdi_diff,
   hpdi_mu, 
   max_height,
   mu,
   mu_df,
   mu_list,
   plt_diff,
   plt_mu,
   plt_posterior,
   plt_save,
   plt_shaded_mu,
   samples)

#### m4: actual acc ~ (group + scaled_sep)^2 ####
load("modelling/model_data/model_data_scaled")
load("modelling/model_outputs/m_stan_group_scaled_acc")

seps <- c(min(model_data_scaled$scaled_sep), mean(model_data_scaled$scaled_sep), max(model_data_scaled$scaled_sep))

# setup effects 
X <- tibble(control = rep(1, 9),
            motivated = rep(c(0,1,0), 3),
            optimal = rep(c(0,0,1), 3),
            control_sep = rep(c(seps[1], seps[2],seps[3]), each = 3),
            motivated_sep = ifelse(motivated == 1, control_sep, 0),
            optimal_sep = ifelse(optimal == 1, control_sep, 0))
X <- as.matrix(X)

x_vals <- seq(0,1-0.001,0.001)

# extract samples 
samples <- rstan::extract(m_stan_group_scaled_acc)

# plot something...
# doesn't work...
# plt_posterior <- plt_post_beta(model_data_scaled, x_vals, m_stan_group_scaled_acc, X)


#### m3: Acc ~ (group + acc_type)^2 ####
#### NB: work in progress... not sure if needed? ####
load("modelling/model_outputs/m_stan_both")
load("modelling/model_data/beta_3")

# setup effects 
X <- data.frame(group = rep(c("Control", "Motivated", "Optimal"), each = 2),
                acc_type = rep(c("Raw", "Prediced"), 3),
                acc = rep(1, 6))

X <- as.matrix(model.matrix(acc ~ (group + acc_type)^2, data = X))
colnames(X) <- c("Control:Raw",
                 "Control:Predicted",
                 "Motivated:Raw",
                 "Motivated:Predicted",
                 "Optimal:Raw",
                 "Optimal:Predicted")

# sequence to predict over 
x_vals <- seq(0,1-0.001, 0.001)

# get samples 
samples <- rstan::extract(m_stan_both)

plt_posterior <- plt_post_beta(model_data, x_vals, m_stan_both, X)
plt_posterior <- plt_posterior + 
  scale_x_continuous(limits = c(0.5, 0.9)) + 
  scale_y_continuous(breaks = seq(0,15,5))
plt_posterior$labels$colour <- ""
plt_posterior$labels$fill <- ""
plt_posterior

# hpdi etc
mu_list <- plt_mu_beta(samples, X, model_data)
mu <- mu_list[[1]]
mu_df <- mu_list[[2]]
hpdi_mu <- mu_list[[3]]
plt_mu <- mu_list[[4]]
plt_mu$labels$colour <- ""
plt_mu$labels$fill <- ""
plt_mu

