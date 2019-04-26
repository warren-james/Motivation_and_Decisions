#### Add in switch points ####
# Level 4 Thesis by Ellen 
# 2017/18
# Written by Warren James
# This script adds in the optimal switch points

#### libraries needed ####
library(tidyverse)
library(R.matlab)

# load in part 2 data 
load("scratch/df_raw")

# create a tibble for switch points 
switch_df <- tibble(
  participant = numeric(),
  switch_point = numeric()
)

results_files <- dir("data/switch_points/")

# add in a counter to assign the row
count <- 1

for (f in results_files){
  d <- readMat(
    paste("data/switch_points/", f, sep=""))
  
  # get switch point
  d <- d$switchdist[1]
  
  # get parts of the string for part no.
  temp <- strsplit(f, '[_.]')[[1]]
  
  # now input this information
  switch_df[count,] <- c(temp[2], d)
  
  # increas the count 
  count <- count + 1
  
}

# tidy
rm(count, d, f, temp, results_files)

# combine the datasets
switch_df <- merge(df, switch_df, by="participant")

# tidy
rm(df)

#### Create column for whether they made the optimal choice or not ####
# Make side vs centre first 
# Think this is right, check the script though
# originally made as 1, 2, 3 = left, centre, right... doesn't seem to be the case going
# by my own testing of this experiment... looks like it's as follows instead
# 1 = centre, 2 = left, 3 = right
switch_df$centre <- ifelse(switch_df$fixated_box == 1, 1, 0)

#### save the file (everything) ####
save(switch_df, file = "scratch/switch_data")

#### Create version with Na's removed ####
switch_df <- switch_df[complete.cases(switch_df),]

#### Track score? ####
# make score its own data fram then bind it later...
score_frame <- tibble(score = numeric()) # too few obs

# Within each block and participant there should be a count based on whether people
# were accurate or not, so start count at 0,
# if correct then add one, if incorrect, subtract one
# if thresholds are reached then reset (6 upper, lower -4 I think?)
# If a threshold is reached, score is reset to one 
# so as if it is a new block, does this also when making a mistake
# 1 is the "centre" of the bar... that explains it 

for(i in unique(switch_df$participant)){
  for(j in unique(switch_df$block)){
    score <- 0 
    # loop through each row separately
    for(k in unique(switch_df$trial[switch_df$participant == i &
                                    switch_df$block == j])){
      if(k == min(switch_df$trial[switch_df$participant == i &
                                  switch_df$block == j])){
        score <- 1 # centre the score
      }
      if(switch_df$correct[switch_df$participant == i &
                           switch_df$block == j &
                           switch_df$trial == k] == 1){
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


switch_df <- cbind(switch_df, score_frame)


# Just to check that the score variable is coded properly
temp <- group_by(switch_df, participant, block, num_fish)
temp <- summarise(temp, max_count = max(score))

# tidy 
rm(i, j, k, score, score_frame)

# create save files
save(switch_df, file = "scratch/switch_nar_data")

rm(switch_df)