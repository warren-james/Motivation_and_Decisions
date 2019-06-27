#### Modelling penguin experiment ####
# Aim is to compare this with the original experiment?
# or one that was very similar... which is probably the original
# experiment itself
# Or use the practice part of the Transfer paper 

#### Library ####
library(tidyverse)

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

# work out score 
get_Score <- function(dataframe){
  # setup empty frame
  score_frame <- data.frame(score = numeric())
  
  # loop for score
  for(i in unique(dataframe$participant)){
    for(j in unique(dataframe$block)){
      score <- 0 
      # loop through each row separately
      for(k in unique(dataframe$trial[dataframe$participant == i &
                                      dataframe$block == j])){
        if(k == min(dataframe$trial[dataframe$participant == i &
                                    dataframe$block == j])){
          score <- 1 # centre the score
        }
        if(dataframe$correct[dataframe$participant == i &
                             dataframe$block == j &
                             dataframe$trial == k] == 1){
          score <- score + 1
          if(score == 6){
            score <- 1 # recentre the score
          }
        } else {
          score <- score - 1
          if(score == -4){
            score <- 1 # recentre the score
          }
        } 
        score_frame <- rbind(score_frame, data.frame(score = score))
      }
    }
  }
  return(score_frame)
}

# work out num fish 
get_Fish <- function(dataframe){
  # make data frame
  num_fish_frame <- data.frame(num_fish = numeric())
  
  # now loop 
  for(p in unique(dataframe$participant)){
    for(b in unique(dataframe$block)){
      # set counter to 0
      num_fish <- 0
      # loop through trials 
      for(row in unique(dataframe$trial[dataframe$participant == p &
                                        dataframe$block == b])){
        # add to frame 
        num_fish_frame <- rbind(num_fish_frame, data.frame(num_fish = num_fish))
        
        # get some values
        score <- dataframe$score[dataframe$trial == row]
        correct <- dataframe$correct[dataframe$trial == row]
        
        # check for change
        if(score + correct == 6){
          num_fish <- num_fish + 1
        } else if(score + correct == -4) {
          num_fish <- num_fish - 1
        } else {
          num_fish <- num_fish
        }
      }
    }
  }
  return(num_fish_frame)
}

# make position plot 
plt_pos <- function(data, condition, palette, switch_frame, ordered){
  plt <- data %>% 
    group_by(participant, separation, group, order_cent) %>% 
    summarise(side = 1 - mean(centre)) %>% 
    filter(group == condition) %>%
    ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
               side,
               colour = group)) + 
    geom_line(data = switch_frame, 
              aes(get_VisDegs(separation/ppcm, Screen_dist),
                  opt_pos),
              colour = "black") + 
    geom_point() + 
    theme_bw() + 
    theme(strip.text.x = element_blank(),
          legend.position = "none") +
    scale_colour_manual(values = palette) + 
    scale_y_continuous(breaks = c(0,0.5,1))
  if(ordered == TRUE){
    plt <- plt + facet_wrap(~ order_cent + participant, ncol = 6) 
  } else { 
    plt <- plt + facet_wrap(~participant, ncol = 6)
  }
  return(plt)
}


#### Load in Data ####
#### Load Penguin ####
# read in penguin data 
load("scratch/switch_nar_data")
# remove participant that didn't complete 4 blocks #
switch_df <- switch_df %>%
  group_by(participant) %>%
  filter(max(block) == 4) %>%
  ungroup()
df_penguin <- switch_df

# tidy 
rm(switch_df)

#### Load Control ####
# read in control data 
load("scratch/switch_df_control_nar")

# subset this data
df_control <- switch_df_control %>%
  filter(condition == "No_instructions",
         block < 5)

# subset for optimal 
df_optimal <- switch_df_control %>% 
  filter(condition == "Instructions",
         block < 5)

# tidy 
rm(switch_df_control)

# add in row identifier 
df_optimal$trial <- rownames(df_optimal)
df_control$trial <- rownames(df_control)

# Add in score
df_control <- cbind(df_control, get_Score(df_control))
df_optimal <- cbind(df_optimal, get_Score(df_optimal))

# get num_fish
df_control <- cbind(df_control, get_Fish(df_control))
df_optimal <- cbind(df_optimal, get_Fish(df_optimal))

#### SORT data ####
# need identifiers for each group 
df_optimal$group <- "optimal"
df_control$group <- "control"
df_penguin$group <- "motivated"

# select what we need
df_optimal <- df_optimal %>% 
  select(-condition,
         -trial, 
         -part)

df_control <- df_control %>%
  select(-condition, 
         -trial,
         -part)

df_penguin <- df_penguin %>%
  select(participant,
         block,
         separation,
         fixated_box,
         correct,
         switch_point,
         centre,
         score,
         num_fish,
         group)

# sort participants 
df_all <- rbind(df_control, df_optimal) %>%
  rbind(df_penguin) %>%
  mutate(participant = paste(participant, group, sep = "_"),
         dist_type = ifelse(separation > round(as.numeric(switch_point)), "far", "close"))

# save this
save(df_all, file = "scratch/all_data")

#### get groupID for acc_sep file ####
df_groupID <- rbind(df_control, df_optimal) %>%
  group_by(participant) %>%
  summarise(group = unique(group))

# save 
save(df_groupID, file = "scratch/df_groupID")

#### PLOTS ####
#### PLOTS: num_fish ####
# compare max number of fish for each group?
# this might be a bit dumb... and really crude... but ah well
# might need to work on the scripts for working out the score etc
# as it seems to overestimate somewhat 
plt_fish <- df_all %>%
  group_by(participant, group) %>%
  summarise(Score = max(num_fish)) %>% 
  ggplot(aes(Score,
             fill = group)) +
  geom_density(alpha = 0.5)
plt_fish

#### PLOTS: overall acc ####
# acc plot 
plt_acc <- df_all %>%
  group_by(participant, group) %>%
  summarise(Accuracy = mean(correct)) %>%
  ungroup() %>%
  ggplot(aes(Accuracy,
             fill = group,
             colour = group)) +
  geom_density(alpha = 0.5)+
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  theme_minimal()
plt_acc


#### PLOTS: acc ~ dist_type ####
# try adding in dist type as a wrap?
plt_acc_dist <- df_all %>%
  group_by(participant, group, dist_type) %>%
  summarise(Accuracy = mean(correct)) %>%
  ungroup() %>%
  ggplot(aes(Accuracy,
             fill = group,
             colour = group)) +
  geom_density(alpha = 0.5)+
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol() + 
  theme_minimal() + 
  facet_wrap(~dist_type)
plt_acc_dist


#### Same plots as always ####
# create switch line
switch_line <- df_all %>% 
  group_by(participant, group) %>% 
  summarise(max_delta = max(separation),
            min_delta = min(separation),
            switch_point = mean(as.numeric(switch_point)))

acc_sep <- tibble(participant = character(),
                  group = character(),
                  separation = numeric(),
                  opt_pos = numeric())

for(p in unique(switch_line$participant)){
  group = switch_line$group[switch_line$participant == p]
  switch = switch_line$switch_point[switch_line$participant == p]
  max_d = switch_line$max_delta[switch_line$participant == p]
  min_d = switch_line$min_delta[switch_line$participant == p]
  for(ii in seq(min_d, max_d, 1)){
    if(ii < switch){
      opt_pos <- 0
    } else {
      opt_pos <- 1
    }
    acc_sep <- rbind(acc_sep, data.frame(participant = p, 
                                         group = group,
                                         separation = ii,
                                         opt_pos = opt_pos))
  }
}

# sort out ordering 
# this doesn't seem to work...
df_order <- df_all %>% 
  group_by(participant) %>% 
  summarise(order_cent = 1 - mean(centre)) 

df_all <- merge(df_all, df_order)
acc_sep <- merge(acc_sep, df_order)
acc_sep_opt <- acc_sep[acc_sep$group == "optimal",]
acc_sep_mot <- acc_sep[acc_sep$group == "motivated",]
acc_sep_cont <- acc_sep[acc_sep$group == "control",]

# make plots 
# optimal
plt_pos_optimal <- plt_pos(df_all, "optimal", "#CC6677", acc_sep_opt, TRUE)
plt_pos_optimal$labels$y <- ""
plt_pos_optimal$labels$x <- "Delta (Visual Degrees)"

# motivated 
plt_pos_motivated <- plt_pos(df_all, "motivated", "#DDCC77", acc_sep_mot, TRUE)
plt_pos_motivated$labels$y <- "Proportion of Fixations to the side"
plt_pos_motivated$labels$x <- "" 

# control 
plt_pos_control <- plt_pos(df_all, "control", "#4477AA", acc_sep_cont, TRUE)
plt_pos_control$labels$y <- ""
plt_pos_control$labels$x <- ""


plt <- gridExtra::grid.arrange(plt_pos_control,
                               plt_pos_motivated,
                               plt_pos_optimal,
                               heights = c(2.2,3,2.2))


ggsave(file = "../Figures/Part_2_all_groups.png", plt,
       height = 10,
       width = 10)

#### Make exp vs act plots ####
load("scratch/all_data")

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


# remove participant that didn't complete 4 blocks 
df_all <- df_all %>%
  group_by(participant) %>%
  filter(max(block) == 4) %>% 
  ungroup()

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

# save this 
save(df_all, file = "scratch/model_data")

# now make plot 
df_all %>% 
  group_by(participant, separation, group) %>% 
  summarise(Actual = mean(correct),
            Expected = mean(accuracy)) %>%
  gather(c(Actual, Expected),
         key = "acc_type",
         value = "Accuracy") %>%
  ggplot(aes(separation, Accuracy, colour = acc_type)) +
  geom_point() + 
  see::scale_color_flat() +
  see::theme_lucid() +
  facet_wrap(~group)

#### Box plots of accuracy ####
plt_boxplt_acc <- df_all %>% 
  group_by(participant, group) %>% 
  summarise(Accuracy = mean(correct)) %>% 
  ggplot(aes(group, Accuracy,
             fill = group)) + 
  geom_boxplot() + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggthemes::scale_fill_ptol() + 
  scale_y_continuous(limits = c(0.5, 1))

plt_boxplt_exp <- df_all %>% 
  group_by(participant, group) %>% 
  summarise(Accuracy = mean(accuracy)) %>% 
  ggplot(aes(group, Accuracy,
             fill = group)) + 
  geom_boxplot() + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggthemes::scale_fill_ptol() + 
  scale_y_continuous(limits = c(0.5, 1))
plt_boxplt_exp$labels$y <- "Predicted Accuracy"

# plot together
plt <- gridExtra::grid.arrange(plt_boxplt_acc,
                               plt_boxplt_exp,
                               nrow = 1)

# save 
ggsave(file = "../Figures/Part_2_acc_boxplots.png", plt,
       height = 3,
       width = 4)
