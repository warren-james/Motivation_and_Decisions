#### modelling penguin ####
# This script is to work on modelling the data 
# from the penguin version of the task with other 
# control versions (using instructed and practice from 
# the transfer paper)

#### Library ####
library(tidyverse)
library(brms)
library(tidybayes)
library(rstan)

#### constants ####
Screen_dist <- 60
x_res <- 1920
x_width <- 54
ppcm <- x_res/x_width

#### Functions #### 
# get visual degrees
get_VisDegs <- function(size,distance){
  ((2*atan2(size,(2*distance)))*180)/pi
}

#### load in data ####
load("scratch/all_data")
df_all <- df

#tidy
rm(df)

# work out expected accuracy? 
# motivated 
load("scratch/acc_sep_peng")
acc_sep_peng <- acc_sep %>%
  mutate(participant = paste(participant,
                             "motivated",
                             sep = "_"))

# control + optimal 
load("scratch/acc_sep_contopt")
load("scratch/df_groupID")
acc_sep <- merge(acc_sep, df_groupID) %>%
  mutate(participant = paste(participant, group, sep = "_")) %>%
  select(-group) %>%
  rbind(acc_sep_peng)

# tidy 
rm(acc_sep_peng)

# bind this to df 
# need to figure out distances... before binding...
acc_sep_1 <- acc_sep %>%
  mutate(separation_1 = separation,
         accuracy_1 = accuracy) %>%
  select(-separation, -accuracy)
acc_sep_2 <- acc_sep %>%
  mutate(separation_2 = separation,
         accuracy_2 = accuracy) %>%
  select(-separation, -accuracy)

df_all <- df_all %>%
  mutate(separation_1 = separation*centre,
         separation_2 = (separation*2)-separation_1)

# merge this 
df_all<- merge(df_all, acc_sep_1)
df_all<- merge(df_all, acc_sep_2) %>%
  mutate(accuracy = (accuracy_1 + accuracy_2)/2) %>%
  select(-separation_1, -separation_2,
         -accuracy_1, -accuracy_2)

# tidy 
rm(acc_sep_1, acc_sep_2)


#### remove participant that didn't complete 4 blocks ####
df_all <- df_all %>%
  group_by(participant) %>%
  filter(max(block) == 4)

# plot something to check 
df_all%>% 
  group_by(participant, group) %>%
  summarise(predicted = mean(accuracy),
            actual = mean(correct)) %>%
  gather(predicted:actual,
         key = "type",
         value = "accuracy") %>%
  ggplot(aes(accuracy, colour = group, fill = group)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() +
  facet_wrap(~type)


# sort out other groups too...
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
model_data <- df_all%>% 
  group_by(participant, dist_type, group) %>%
  summarise(Accuracy = mean(correct)) %>%
  mutate(Accuracy = (Accuracy + 1e-5)*0.9999)

m_acc_group_dist_beta <- brm(Accuracy ~ (dist_type + group)^2,
                             data = model_data,
                             family = "beta",
                             chains = 1,
                             cores = 1,
                             iter = 4000)

# plot posterior
# try using the tidybayes idea?
plt_group_dist <- model_data %>%
  add_predicted_draws(m_acc_group_dist_beta) %>%
  ggplot(aes(.prediction, colour = dist_type, fill = dist_type)) +
  geom_density(alpha = 0.3) +
  geom_density(data = model_data,
               aes(Accuracy,
                   colour = dist_type,
                   fill = NA),
               alpha = 0.0001) +
  theme_minimal() +
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() +
  facet_wrap(~group)
plt_group_dist



#### Beta: Group only ####
model_data_2 <- df_all%>%
  group_by(participant, group) %>%
  summarise(Accuracy = mean(correct)) %>%
  mutate(Accuracy = (Accuracy + 1e-5)*0.9999) 

m_acc_group <- brm(Accuracy ~ group,
                   data = model_data_2,
                   family = "beta",
                   chains = 1,
                   cores = 1,
                   iter = 2000,
                   warmup = 1000)

# plot
plt_group <- model_data_2 %>%
  add_predicted_draws(m_acc_group) %>%
  ggplot(aes(.prediction, colour = group, fill = group)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol()
plt_group$labels$x <- "Success Rate"
plt_group$labels$fill <- "Group"
plt_group$labels$colour <- "Group"
plt_group

# check overlap?
plt_diff <- plt_group[["data"]] %>%
  ungroup() %>%
  select(group, .prediction, .draw) %>%
  group_by(group, .draw) %>%
  summarise(.prediction = mean(.prediction)) %>%
  spread(group, .prediction) %>%
  mutate("Motivated vs Control" = motivated - control,
         "Optimal vs Motivated" = optimal - motivated,
         "Optimal vs Control" = optimal - control) %>%
  select(-control, -motivated, -optimal) %>%
  gather("Motivated vs Control":"Optimal vs Control",
         key = "Comparison",
         value = "Difference") %>%
  ggplot(aes(Difference, colour = Comparison, fill = Comparison)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() + 
  geom_vline(xintercept = 0,
             linetype = "dashed")
plt_diff

# plot together
plt_raw_acc <- gridExtra::grid.arrange(plt_group, plt_diff, ncol = 2)
ggsave(plt_raw_acc, file = "../Figures/Model_output_raw_acc.png",
       height = 5,
       width = 13)

# HPDI for estimates 
HPDI_m_acc_group <- plt_group[["data"]] %>%
  group_by(group) %>%
  summarise(lower = HPDI(.prediction, 0.95)[1],
            upper = HPDI(.prediction, 0.95)[2])


#### BRMS on expected accuracy ####
model_data_3 <- df_all%>%
  group_by(participant, group) %>%
  summarise(pred_accuracy = mean(accuracy))

# model 
m_pacc_group <- brm(pred_accuracy ~ group,
                    data = model_data_3,
                    family = "beta",
                    chains = 1,
                    cores = 1,
                    iter = 2000,
                    warmup = 1000)

plt_group <- model_data_3 %>%
  add_predicted_draws(m_pacc_group) %>%
  ggplot(aes(.prediction, colour = group, fill = group)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol()
plt_group$labels$x <- "Accuracy"
plt_group$labels$fill <- "Group"
plt_group$labels$colour <- "Group"
plt_group

plt_diff <- plt_group[["data"]] %>%
  ungroup() %>%
  select(group, .prediction, .draw) %>%
  group_by(group, .draw) %>%
  summarise(.prediction = mean(.prediction)) %>%
  spread(group, .prediction) %>%
  mutate("Motivated vs Control" = motivated - control,
         "Optimal vs Motivated" = optimal - motivated,
         "Optimal vs Control" = optimal - control) %>%
  select(-control, -motivated, -optimal) %>%
  gather("Motivated vs Control":"Optimal vs Control",
         key = "Comparison",
         value = "Difference") %>%
  ggplot(aes(Difference, colour = Comparison, fill = Comparison)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  ggthemes::scale_colour_ptol() +
  ggthemes::scale_fill_ptol() + 
  geom_vline(xintercept = 0,
             linetype = "dashed")
plt_diff

# save plot 
plt_exp_acc <- gridExtra::grid.arrange(plt_group, plt_diff, ncol = 2)
ggsave(plt_exp_acc, file = "../Figures/Model_output_exp_acc.png",
       height = 5,
       width = 13)




#### STAN ####
#### STAN: binomial ####
# setup contrasts myself  
df$motivated <- 0 
df$motivated[df$group == "motivated"] <- 1
df$optimal <- 0 
df$optimal[df$group == "optimal"] <- 1
df$dist_far <- 0 
df$dist_far[df$dist_type == "far"] <- 1 

stan_df <- list(
  N = nrow(df), 
  optimal = df$optimal,
  motivated = df$motivated,
  dist_far = df$dist_far, 
  correct = df$correct
)

m_stan_group_dist_bin <- stan(
  file = "modelling/models/binomial_model_2.stan",
  data = stan_df,
  chains = 1, 
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save this and work on  
save(m_stan_group_dist_bin, file = "modelling/model_outputs/m_stan_group_dist_bin")

# working on plotting this output
post_samples <- as.tibble(rstan::extract(m_stan_group_dist_bin))

# use this to set up esimates 
get_preds <- function(dist_far, post){
  # setting dist_type 
  if(dist_far > 0){
    dist_type = "far"
  } else {
    dist_type = "close"
  }
  preds <- data.frame(
    group = rep(c(
      "control",
      "motivated",
      "optimal"
    ), each = length(post$c)),
    dist_type = dist_type, 
    samples = c(
      plogis(post$c + post$b_f*dist_far),
      plogis(post$c + post$b_f*dist_far + post$b_m + post$b_mf*dist_far),
      plogis(post$c + post$b_f*dist_far + post$b_o + post$b_of*dist_far)))
  return(preds)
}

samples_close <- get_preds(0, post_samples)
samples_far <- get_preds(1, post_samples)

samples <- rbind(samples_close, samples_far)

# plot this
samples %>% 
  ggplot(aes(samples, colour = group, fill = group)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  facet_wrap(~dist_type)


# same as above but remove the dist_type interaction 
stan_df <- list(
  N = nrow(df), 
  optimal = df$optimal,
  motivated = df$motivated, 
  correct = df$correct
)

m_stan_group_bin <- stan(
  file = "modelling/models/binomial_model_main_group.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# get post 
post_samples <- as.tibble(extract(m_stan_group_bin))

# predictions 
plt_stan_acc_group <- data.frame(
  group = rep(c(
    "control",
    "motivated",
    "optimal"), each = length(post_samples$c)),
  samples = c(
    plogis(post_samples$c),
    plogis(post_samples$c + post_samples$b_m),
    plogis(post_samples$c + post_samples$b_o))) %>% 
  ggplot(aes(samples, colour = group, fill = group)) + 
  geom_density(alpha = 0.3) + 
  theme_minimal() + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol()



# PROPORTION SIDE #
# # make a side column 
# df$side <- 1 - df$centre
# 
# # make norm delta 
# df_all<- df_all%>% 
#   group_by(participant) %>%
#   mutate(norm_delta = separation/max(separation))
# 

#### STAN: Beta ####
#### STAN: Accuracy ~ group ####
# replicating the BRMS version essentially
X <- model.matrix(Accuracy ~ group, data = model_data_2)

model_data_new <- model_data_2 %>%
  rownames_to_column(var = "row_num")

stan_df <- list(
  N = nrow(model_data_2),
  K = ncol(X),
  y = model_data_2$Accuracy,
  X = X
)

m_stan_group <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)


samples <- rstan::extract(m_stan_group)

# This looks like it works?
# dists look a lot tighter though 
#### function to plot STAN output ####
make_plt <- function(model_output, dataframe, dist_true){
  output <- as.tibble(model_output) %>%
    gather(key = "remove",
           value = "pred_mu") %>%
    group_by(remove) %>%
    mutate(row_num = strsplit(remove, split = "V")[[1]][2]) %>%
    ungroup() %>%
    select(-remove) %>%
    merge(dataframe) %>%
    ggplot(aes(pred_mu,
               colour = group,
               fill = group)) + 
    geom_density(alpha = 0.3) + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    ggthemes::scale_color_ptol() + 
    ggthemes::scale_fill_ptol()
  if(dist_true == T){
    output <- output + facet_wrap(~dist_type)
  }
  return(output)
}

plt_m_real_acc <- make_plt(samples$mu, model_data_new, FALSE)
plt_m_real_acc$labels$x <- "Predicted Mean Success Rate"
plt_m_real_acc$labels$colour <- "Group"
plt_m_real_acc$labels$fill <- "Group"
plt_m_real_acc

# save this 
ggsave(file = "../Figures/Stan_act_acc.png",
       heigh = 5,
       width = 5)

# get HPDI 
HPDI_sag <- plt_m_real_acc[["data"]] %>%
  group_by(group) %>%
  summarise(lower = HPDI(pred_mu, 0.95)[1],
            upper = HPDI(pred_mu, 0.95)[2])
HPDI_sag

#### STAN: Predicted Accuracy ####
# same as above but now on expected accuracy
X <- model.matrix(pred_accuracy ~ group, data = model_data_3)

# setup data for plotting
model_data_new <- model_data_3 %>%
  rownames_to_column(var = "row_num")

stan_df <- list(
  N = nrow(model_data_3),
  K = ncol(X),
  y = model_data_3$pred_accuracy,
  X = X
)

m_stan_group_exp <- stan(
  file = "modelling/models/stan_model.stan",
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)


samples <- rstan::extract(m_stan_group_exp)

# This looks like it works?
# dists look a lot tighter though 
plt_m_exp_acc <- make_plt(samples$mu, model_data_new, FALSE)
plt_m_exp_acc$labels$x <- "Predicted Mean Expected Success Rate"
plt_m_exp_acc$labels$colour <- "Group"
plt_m_exp_acc$labels$fill <- "Group"
plt_m_exp_acc

# save 
ggsave(file = "../Figures/Stan_exp_acc.png",
       height = 5,
       width = 5)

# get HPDI
HPDI_seg <- plt_m_exp_acc[["data"]] %>%
  group_by(group) %>%
  summarise(lower = HPDI(pred_mu, 0.95)[1],
            upper = HPDI(pred_mu, 0.95)[2])
HPDI_seg

#### doing the above again with dist_type added? ####
X <- model.matrix(Accuracy ~ (group + dist_type)^2, data = model_data)

model_data_new <- model_data %>%
  rownames_to_column(var = "row_num")

stan_df <- list(
  N = nrow(model_data),
  K = ncol(X),
  y = model_data$Accuracy,
  X = X
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


