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

# extract samples
samples <- rstan::extract(m_stan_group)




#### STAN: Predicted Accuracy ####
# same as above but now on expected accuracy
model_data <- df_all %>%
  group_by(participant, group) %>%
  summarise(pred_accuracy = mean(accuracy)) %>%
  ungroup()

m_matrix <- model.matrix(pred_accuracy ~ group, data = model_data)

stan_df <- list(
  N = nrow(model_data),l
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

#### STAN: add in block... #### 
# maybe people in the motivated condition stayed more accurate for longer?
model_data <- df_all %>%
  select(participant, group, block, correct)

m_matrix <- model.matrix(correct ~ (group + block)^2, data = model_data)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$correct,
  X = m_matrix
)

# WIP, takes far too long, not sure why
m_stan_berno_blk <- stan(
  file = "modelling/models/stan_berno.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# no need to worry, this doesn't contribute much 
# the effect of block pretty much overlaps with 0 for all groups

#### STAN: bernoulli with rand intercepts? ####
model_data <- model_data %>% 
  ungroup() %>%
  mutate(participant = as.numeric(as.factor(participant)))

# try random int version
m_matrix <- model.matrix(correct ~ group, data = model_data)

# setup stan_df
stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$correct,
  S = length(unique(model_data$participant)),
  subj = model_data$participant,
  X = m_matrix
)

# run model?
# this seems to take a fair while... so some changes might need to be made...
m_stan_berno <- stan(
  file = "modelling/models/stan_berno_rint.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

#### STAN: add in dist_type Bernoulli ####
model_data <- df_all %>%
  select(participant, group, dist_type, correct)

m_matrix <- model.matrix(correct ~ (group + dist_type)^2, data = model_data)

stan_df <- list(
  N = nrow(model_data),
  K = ncol(m_matrix),
  y = model_data$correct,
  X = m_matrix
)

# WIP, takes far too long, not sure why
m_stan_berno_dt <- stan(
  file = "modelling/models/stan_berno.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

save(model_data, file = "modelling/model_data/berno_2")
save(m_stan_berno_dt, file = "modelling/model_outputs/m_stan_berno_2")


#### Using Binomial count data ####
# trying to avoid throwing away data 

# starting off with brms as this is a good way to start...
m_brms_log <- brm(correct | trials(1) ~ group,
                  data = df_all, 
                  family = binomial,
                  chains = 1,
                  iter = 2000,
                  warmup = 1000)

m_brms_log_rint <- brm(correct | trials(1) ~ group + (1|participant),
                       data = df_all, 
                       family = binomial,
                       chains = 1,
                       iter = 2000,
                       warmup = 1000)


#### Bernoulli with rand intercepts ####
# this might look better?
# dist is wider for control which is a good start
m_brms_bern <- brm(correct ~ group,
                   data = df_all, 
                   family = "bernoulli",
                   chains = 1,
                   iter = 2000,
                   warmup = 1000)

m_brms_bern_rint <- brm(correct ~ group + (1|participant),
                        data = df_all, 
                        family = "bernoulli",
                        chains = 1,
                        iter = 2000,
                        warmup = 1000)

# check this for how to write the stan code
make_stancode(correct ~ group + (1|participant),
              data = df_all, 
              family = "bernoulli")

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

samples <- rstan::extract(m_stan_group_dist)
plt_mu_df_dist <- make_plt(samples$mu, model_data_new, TRUE)
plt_mu_df_dist$labels$x <- "Predicted Mean Success Rate"
plt_mu_df_dist$labels$colour <- "Group"
plt_mu_df_dist$labels$fill <- "Group"
plt_mu_df_dist

# HPDI 
HPDI_sag_d <- plt_mu_df_dist[["data"]] %>%
  group_by(dist_type, group) %>%
  summarise(mean_est = mean(pred_mu),
            lower = HPDI(pred_mu, 0.95)[1],
            upper = HPDI(pred_mu, 0.95)[2])
HPDI_sag_d


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

samples <- rstan::extract(m_stan_group_dist_exp)

# this just plots the mean
# plt_mu_df_dist_exp <- make_plt(samples$mu, model_data_new, TRUE)
# plt_mu_df_dist_exp$labels$x <- "Predicted Estimated Mean Success Rate"
# plt_mu_df_dist_exp$labels$colour <- "Group"
# plt_mu_df_dist_exp$labels$fill <- "Group"
# plt_mu_df_dist_exp

# HPDI 
HPDI_seg_d <- plt_mu_df_dist[["data"]] %>%
  group_by(dist_type, group) %>%
  summarise(mean_est = mean(pred_mu),
            lower = HPDI(pred_mu, 0.95)[1],
            upper = HPDI(pred_mu, 0.95)[2])
HPDI_seg_d



#### ACCURACY ####
#### brms models ####
#### using all data: binomial ####
# quick oneof dist_type * group for accuracy 
# m_acc_group_dist <- brm(correct ~ (dist_type + group)^2,
#                         data = df,
#                         family = "bernoulli",
#                         chains = 1,
#                         cores = 1,
#                         iter = 4000)

# plot posterior 
# too big... doesn't like this...
# plt_brms_group_dist <- df %>%
#   add_predicted_draws(m_acc_group_dist) %>%
#   ggplot(aes(.prediction, colour = group, fill = group)) +
#   geom_density(alpha = 0.3) +
#   # geom_density(data = df,
#   #              aes(Accuracy,
#   #                  colour = group,
#   #                  fill = NA),
#   #              alpha = 0.0001) +
#   theme_minimal() +
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol() + 
#   facet_wrap(~dist_type)

#### Using averages: beta ####
#### Beta: group*dist ####
# model_data <- df_all%>%
#   group_by(participant, dist_type, group) %>%
#   summarise(Accuracy = mean(correct)) %>%
#   mutate(Accuracy = (Accuracy + 1e-5)*0.9999)

# m_acc_group_dist_beta <- brm(Accuracy ~ (dist_type + group)^2,
#                              data = model_data,
#                              family = "beta",
#                              chains = 1,
#                              cores = 1,
#                              iter = 4000)
# 
# # plot posterior
# # try using the tidybayes idea?
# plt_group_dist <- model_data %>%
#   add_predicted_draws(m_acc_group_dist_beta) %>%
#   ggplot(aes(.prediction, colour = dist_type, fill = dist_type)) +
#   geom_density(alpha = 0.3) +
#   geom_density(data = model_data,
#                aes(Accuracy,
#                    colour = dist_type,
#                    fill = NA),
#                alpha = 0.0001) +
#   theme_minimal() +
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol() +
#   facet_wrap(~group)
# plt_group_dist
# 
# 
# 
#### Beta: Group only ####
# model_data_2 <- df_all%>%
#   group_by(participant, group) %>%
#   summarise(Accuracy = mean(correct)) %>%
#   mutate(Accuracy = (Accuracy + 1e-5)*0.9999) 
# 
# m_acc_group <- brm(Accuracy ~ group,
#                    data = model_data_2,
#                    family = "beta",
#                    chains = 1,
#                    cores = 1,
#                    iter = 2000,
#                    warmup = 1000)
# 
# # plot
# plt_group <- model_data_2 %>%
#   add_predicted_draws(m_acc_group) %>%
#   ggplot(aes(.prediction, colour = group, fill = group)) +
#   geom_density(alpha = 0.3) +
#   theme_minimal() +
#   theme(legend.position = "bottom") + 
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol()
# plt_group$labels$x <- "Success Rate"
# plt_group$labels$fill <- "Group"
# plt_group$labels$colour <- "Group"
# plt_group
# 
# # check overlap?
# plt_diff <- plt_group[["data"]] %>%
#   ungroup() %>%
#   select(group, .prediction, .draw) %>%
#   group_by(group, .draw) %>%
#   summarise(.prediction = mean(.prediction)) %>%
#   spread(group, .prediction) %>%
#   mutate("Motivated vs Control" = motivated - control,
#          "Optimal vs Motivated" = optimal - motivated,
#          "Optimal vs Control" = optimal - control) %>%
#   select(-control, -motivated, -optimal) %>%
#   gather("Motivated vs Control":"Optimal vs Control",
#          key = "Comparison",
#          value = "Difference") %>%
#   ggplot(aes(Difference, colour = Comparison, fill = Comparison)) + 
#   geom_density(alpha = 0.3) + 
#   theme_minimal() +
#   theme(legend.position = "bottom") + 
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol() + 
#   geom_vline(xintercept = 0,
#              linetype = "dashed")
# plt_diff
# 
# # plot together
# plt_raw_acc <- gridExtra::grid.arrange(plt_group, plt_diff, ncol = 2)
# ggsave(plt_raw_acc, file = "../Figures/Model_output_raw_acc.png",
#        height = 5,
#        width = 13)
# 
# # HPDI for estimates 
# HPDI_m_acc_group <- plt_group[["data"]] %>%
#   group_by(group) %>%
#   summarise(lower = HPDI(.prediction, 0.95)[1],
#             upper = HPDI(.prediction, 0.95)[2])
# 
# 
#### BRMS on expected accuracy ####
# model_data_3 <- df_all%>%
#   group_by(participant, group) %>%
#   summarise(pred_accuracy = mean(accuracy))
# 
# # model 
# m_pacc_group <- brm(pred_accuracy ~ group,
#                     data = model_data_3,
#                     family = "beta",
#                     chains = 1,
#                     cores = 1,
#                     iter = 2000,
#                     warmup = 1000)
# 
# plt_group <- model_data_3 %>%
#   add_predicted_draws(m_pacc_group) %>%
#   ggplot(aes(.prediction, colour = group, fill = group)) +
#   geom_density(alpha = 0.3) +
#   theme_minimal() +
#   theme(legend.position = "bottom") + 
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol()
# plt_group$labels$x <- "Accuracy"
# plt_group$labels$fill <- "Group"
# plt_group$labels$colour <- "Group"
# plt_group
# 
# plt_diff <- plt_group[["data"]] %>%
#   ungroup() %>%
#   select(group, .prediction, .draw) %>%
#   group_by(group, .draw) %>%
#   summarise(.prediction = mean(.prediction)) %>%
#   spread(group, .prediction) %>%
#   mutate("Motivated vs Control" = motivated - control,
#          "Optimal vs Motivated" = optimal - motivated,
#          "Optimal vs Control" = optimal - control) %>%
#   select(-control, -motivated, -optimal) %>%
#   gather("Motivated vs Control":"Optimal vs Control",
#          key = "Comparison",
#          value = "Difference") %>%
#   ggplot(aes(Difference, colour = Comparison, fill = Comparison)) + 
#   geom_density(alpha = 0.3) + 
#   theme_minimal() + 
#   theme(legend.position = "bottom") + 
#   ggthemes::scale_colour_ptol() +
#   ggthemes::scale_fill_ptol() + 
#   geom_vline(xintercept = 0,
#              linetype = "dashed")
# plt_diff
# 
# # save plot 
# plt_exp_acc <- gridExtra::grid.arrange(plt_group, plt_diff, ncol = 2)
# ggsave(plt_exp_acc, file = "../Figures/Model_output_exp_acc.png",
#        height = 5,
#        width = 13)
# 
# 
# 
# 
#### STAN ####
#### STAN: binomial ####
# # setup contrasts myself  
# df$motivated <- 0 
# df$motivated[df$group == "motivated"] <- 1
# df$optimal <- 0 
# df$optimal[df$group == "optimal"] <- 1
# df$dist_far <- 0 
# df$dist_far[df$dist_type == "far"] <- 1 
# 
# stan_df <- list(
#   N = nrow(df), 
#   optimal = df$optimal,
#   motivated = df$motivated,
#   dist_far = df$dist_far, 
#   correct = df$correct
# )
# 
# m_stan_group_dist_bin <- stan(
#   file = "modelling/models/binomial_model_2.stan",
#   data = stan_df,
#   chains = 1, 
#   warmup = 1000,
#   iter = 2000,
#   refresh = 100
# )
# 
# # save this and work on  
# save(m_stan_group_dist_bin, file = "modelling/model_outputs/m_stan_group_dist_bin")
# 
# # working on plotting this output
# post_samples <- as.tibble(rstan::extract(m_stan_group_dist_bin))
# 
# # use this to set up esimates 
# get_preds <- function(dist_far, post){
#   # setting dist_type 
#   if(dist_far > 0){
#     dist_type = "far"
#   } else {
#     dist_type = "close"
#   }
#   preds <- data.frame(
#     group = rep(c(
#       "control",
#       "motivated",
#       "optimal"
#     ), each = length(post$c)),
#     dist_type = dist_type, 
#     samples = c(
#       plogis(post$c + post$b_f*dist_far),
#       plogis(post$c + post$b_f*dist_far + post$b_m + post$b_mf*dist_far),
#       plogis(post$c + post$b_f*dist_far + post$b_o + post$b_of*dist_far)))
#   return(preds)
# }
# 
# samples_close <- get_preds(0, post_samples)
# samples_far <- get_preds(1, post_samples)
# 
# samples <- rbind(samples_close, samples_far)
# 
# # plot this
# samples %>% 
#   ggplot(aes(samples, colour = group, fill = group)) + 
#   geom_density(alpha = 0.3) + 
#   theme_minimal() + 
#   ggthemes::scale_colour_ptol() + 
#   ggthemes::scale_fill_ptol() + 
#   facet_wrap(~dist_type)
# 
# 
# # same as above but remove the dist_type interaction 
# stan_df <- list(
#   N = nrow(df), 
#   optimal = df$optimal,
#   motivated = df$motivated, 
#   correct = df$correct
# )
# 
# m_stan_group_bin <- stan(
#   file = "modelling/models/binomial_model_main_group.stan",
#   data = stan_df,
#   chains = 1,
#   warmup = 1000,
#   iter = 2000,
#   refresh = 100
# )
# 
# # get post 
# post_samples <- as.tibble(extract(m_stan_group_bin))
# 
# # predictions 
# plt_stan_acc_group <- data.frame(
#   group = rep(c(
#     "control",
#     "motivated",
#     "optimal"), each = length(post_samples$c)),
#   samples = c(
#     plogis(post_samples$c),
#     plogis(post_samples$c + post_samples$b_m),
#     plogis(post_samples$c + post_samples$b_o))) %>% 
#   ggplot(aes(samples, colour = group, fill = group)) + 
#   geom_density(alpha = 0.3) + 
#   theme_minimal() + 
#   ggthemes::scale_colour_ptol() + 
#   ggthemes::scale_fill_ptol()
# 


# PROPORTION SIDE #
# # make a side column 
# df$side <- 1 - df$centre
# 
# # make norm delta 
# df_all<- df_all%>% 
#   group_by(participant) %>%
#   mutate(norm_delta = separation/max(separation))
# 



