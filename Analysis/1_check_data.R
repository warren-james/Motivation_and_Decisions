#### Script to extract Data ####
# Level 4 Thesis by Ellen
# 2017/18
# Script written by Warren James
# This script reads in the data and produces a file containing 
# all participants data (pilot atm)

#### libraries needed ####
library(tidyverse)

#### load in the dataset #### 
# create a tibble for dataset 
df <- tibble(
  block = numeric(),
  separation = numeric(), 
  fixated_box = numeric(), 
  correct = numeric(),
  num_fish = numeric())

# create column names for the data we have
import_names <- c(
  "block",
  "separation",
  "fixated_box",
  "correct",
  "num_fish")

# set up directory for loop 
results_files <- dir("data/results/part_2/")

for (f in results_files){
  d <- read.csv(
    paste("data/results/part_2/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- temp[2]
  
  # bind to df
  df <- bind_rows(df, d)
}

# tidy 
rm(f, import_names, results_files, temp, d)

# add in trial number? 
df$trial <- 0
df$temp_count <- c(1:nrow(df))

# pretty sure we need this 
for(i in unique(df$participant)){
  for(x in unique(df$block)){
    # set a counter 
    num <- 1
    for(z in df$temp_count[df$block == x & df$participant == i]){
      df$trial[as.numeric(z)] <- num
      num <- num + 1
    }
  }
}

# tidy 
rm(i, num, x, z)
      
# drop temp_count
df <- df[ , !(names(df) %in% "temp_count")]

# reorder df
df <- df[,c("participant",
            "block",
            "trial",
            "separation",
            "fixated_box",
            "correct",
            "num_fish")]

# save data file 
save(df, file = "scratch/df_raw")

