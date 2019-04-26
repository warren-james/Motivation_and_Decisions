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
df <- rbind(df_control, df_optimal) %>%
  rbind(df_penguin) %>%
  mutate(participant = paste(participant, group, sep = "_"),
         dist_type = ifelse(df$separation > round(as.numeric(df$switch_point)), "far", "close"))

# save this
save(df, file = "scratch/all_data")

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
plt_fish <- df %>%
  group_by(participant, group) %>%
  summarise(Score = max(num_fish)) %>% 
  ggplot(aes(Score,
             fill = group)) +
  geom_density(alpha = 0.5)
plt_fish

#### PLOTS: overall acc ####
# acc plot 
plt_acc <- df %>%
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
plt_acc_dist <- df %>%
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
plt_pos_optimal <- df %>%
  group_by(participant, separation, group) %>%
  summarise(centre = mean(centre)) %>%
  mutate(side = 1 - centre) %>%
  filter(group == "optimal") %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             side,
             colour = group)) + 
  geom_point() + 
  theme_bw() + 
  theme(strip.text.x = element_blank()) +
  facet_wrap(~group + participant, ncol = 6) + 
  scale_colour_manual(values = "#CC6677")
plt_pos_optimal$labels$y <- ""
plt_pos_optimal$labels$x <- "Delta (Visual Degrees)"
plt_pos_optimal$labels$colour <- ""

plt_pos_motivated <- df %>%
  group_by(participant, separation, group) %>%
  summarise(centre = mean(centre)) %>%
  mutate(side = 1 - centre) %>%
  filter(group == "motivated") %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             side,
             colour = group)) + 
  geom_point() + 
  theme_bw() + 
  theme(strip.text.x = element_blank()) +
  facet_wrap(~group + participant, ncol = 6) + 
  scale_colour_manual(values = "#DDCC77")
plt_pos_motivated$labels$y <- "Proportion of Fixations to the side"
plt_pos_motivated$labels$x <- "" 
plt_pos_motivated$labels$colour <- ""

plt_pos_control <- df %>%
  group_by(participant, separation, group) %>%
  summarise(centre = mean(centre)) %>%
  mutate(side = 1 - centre) %>%
  filter(group == "control") %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist),
             side,
             colour = group)) + 
  geom_point() + 
  theme_bw() + 
  theme(strip.text.x = element_blank()) +
  facet_wrap(~group + participant, ncol = 6) + 
  scale_colour_manual(values = "#4477AA")
plt_pos_control$labels$y <- ""
plt_pos_control$labels$x <- ""
plt_pos_control$labels$colour <- "Group"

plt <- gridExtra::grid.arrange(plt_pos_control, plt_pos_motivated, plt_pos_optimal)


ggsave(file = "../Figures/Part_2_all_groups.png", plt,
       height = 10,
       width = 10)
