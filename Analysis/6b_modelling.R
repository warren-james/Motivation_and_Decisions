#### modelling penguin ####
# This script is to work on modelling the data 
# from the penguin version of the task with other 
# control versions (using instructed and practice from 
# the transfer paper)

#### Library ####
library(brms)
library(rethinking)
library(rstan)
library(tidybayes)
library(tidyverse)

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


#### load in data ####
load("scratch/model_data")


#### STAN: Beta ####
#### STAN: Accuracy ~ group ####
# replicating the BRMS version essentially
model_data <- df_all %>%
  group_by(participant, group) %>%
  summarise(accuracy = mean(correct)) %>% 
  ungroup()

m_matrix <- model.matrix(accuracy ~ group, data = model_data)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$accuracy,
  X = m_matrix
)

m_stan_group <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 2000,
  iter = 4000,
  refresh = 100
)

# save above
save(model_data, file = "modelling/model_data/beta_1")
save(m_stan_group, file = "modelling/model_outputs/m_stan_group_beta_1")


#### STAN: Predicted Accuracy ####
# same as above but now on expected accuracy
model_data <- df_all %>%
  group_by(participant, group) %>%
  summarise(pred_accuracy = mean(accuracy)) %>%
  ungroup()

m_matrix <- model.matrix(pred_accuracy ~ group, data = model_data)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$pred_accuracy,
  X = m_matrix
)

m_stan_group_exp <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 2000,
  iter = 4000,
  refresh = 100
)

save(model_data, file = "modelling/model_data/beta_2")
save(m_stan_group_exp, file = "modelling/model_outputs/m_stan_group_beta_2")


#### STAN: acc ~ group * acc_type ####
model_data <- df_all %>% 
  group_by(participant, group) %>% 
  summarise(Raw = mean(correct),
            Predicted = mean(accuracy)) %>% 
  gather(c(Raw, Predicted),
         key = "acc_type",
         value = "accuracy") %>% 
  ungroup()

# model_matrix 
m_matrix <- model.matrix(accuracy ~ (group + acc_type)^2,
                         data = model_data)

# stan_df
stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$accuracy,
  X = m_matrix
)

m_stan_both <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save 
save(model_data, file = "modelling/model_data/beta_3")
save(m_stan_both, file = "modelling/model_outputs/m_stan_both")

#### STAN: try bernoulli? ####
# real model 
model_data <- df_all %>% 
  select(participant, group, correct) 

m_matrix <- model.matrix(correct ~ group, data = model_data)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$correct,
  X = m_matrix
)

# WIP, takes far too long, not sure why
m_stan_berno <- stan(
  file = "modelling/models/stan_berno.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

save(model_data, file = "modelling/model_data/berno_1")
save(m_stan_berno, file = "modelling/model_outputs/m_stan_berno_1")



#### STAN: add in dist_type ####
#### STAN: Actual Accuracy ####
model_data <- df_all %>% 
  group_by(participant, dist_type, group) %>%
  summarise(Accuracy = mean(correct)) %>%
  mutate(Accuracy = (Accuracy + 1e-5)*0.9999)

m_matrix <- model.matrix(Accuracy ~ (group + dist_type)^2, data = model_data)

model_data_new <- model_data %>%
  rownames_to_column(var = "row_num")

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$Accuracy,
  X = m_matrix
)


m_stan_group_dist <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)



#### STAN: Expected Accuracy and dist_type ####
model_data_new <- df_all %>% 
  group_by(participant, group, dist_type) %>%
  summarise(pred_accuracy = mean(accuracy)) %>%
  ungroup() %>%
  rownames_to_column(var = "row_num")

m_matrix <- model.matrix(pred_accuracy ~ (group + dist_type)^2, data = model_data_new)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data_new$pred_accuracy,
  X = m_matrix
)

m_stan_group_dist_exp <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)
