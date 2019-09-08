# markdown models 
library(tidyverse)
library(brms)

# load data
load("../Analysis/scratch/model_data")

# format data for model
model_data_scaled <- df_all %>% 
  group_by(participant, group, separation) %>% 
  summarise(act_acc = mean(correct),
            exp_acc = mean(accuracy)) %>%
  mutate(scaled_sep = separation/max(separation),
         act_acc = act_acc*0.9999) %>%
  ungroup()

# make first model on actual accuracy
m1 <- brm(act_acc ~ (group + scaled_sep)^2,
          data = model_data_scaled,
          family = "beta",
          chains = 1,
          iter = 2000,
          warmup = 1000)
# save
save(m1, file = "scratch/markdown_model_act")

m2 <- brm(exp_acc ~ (group + scaled_sep)^2,
          data = model_data_scaled,
          family = "beta",
          chains = 1,
          iter = 2000,
          warmup = 1000)
# save
save(m2, file = "scratch/markdown_model_exp")