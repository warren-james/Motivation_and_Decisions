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
# 1 = centre, 2 = left, 3 = right
switch_df$centre <- ifelse(switch_df$fixated_box == 1, 1, 0)

#### save the file (everything) ####
save(switch_df, file = "scratch/switch_data")

#### Create version with Na's removed ####
switch_df <- switch_df[complete.cases(switch_df),]

#### Track score ####
# make score its own data fram then bind it later...
score_frame <- tibble(score = numeric()) # too few obs

# loop to add in score
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

# tidy 
rm(i, j, k, score, score_frame)

# create save files
save(switch_df, file = "scratch/switch_nar_data")

rm(switch_df)