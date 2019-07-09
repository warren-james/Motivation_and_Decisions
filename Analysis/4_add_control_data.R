#### Sort out data to compare to ####
# This is to read in the data from https:// osf.io/t6c5q/
# then save it for other scripts to use 

#### Library ####
library(tidyverse)
library(R.matlab)
library(psyphy)

#### Load in data ####
df_control <- tibble(
  block = numeric(),
  separation = numeric(), 
  fixated_box = numeric(), 
  correct = numeric())

# create column names for the data we have
import_names <- c(
  "block",
  "separation",
  "fixated_box",
  "correct")

# set up directory for loop 
results_files <- dir("data/Control_dat/Part_2-3/")

# read in each data file
for (f in results_files){
  d <- read.csv(
    paste("data/Control_dat/Part_2-3/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- temp[2]
  
  d$block <- as.numeric(temp[3])
  
  d$part <- substring(temp[4],5,5)
  
  # bind to df
  df_control <- bind_rows(df_control, d)
}

# tidy
rm(temp, f, d, import_names, results_files)

# re-order the dataset 
df_control <- select(df_control,
                     participant,
                     part,
                     block,
                     separation,
                     fixated_box,
                     correct)

# save processed data file
save(df_control, file = "scratch/df_control")

#### Sort Switch points ####
switch_df <- tibble(
  participant = numeric(),
  switch_point = numeric()
)

results_files <- dir("data/Control_dat/switching_points/")

# add in a counter to assign the row
count <- 1

# read in the Matlab data 
for (f in results_files){
  d <- readMat(
    paste("data/Control_dat/switching_points/", f, sep=""))
  
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
switch_df_control <- merge(df_control, switch_df, by="participant")

# tidy 
rm(switch_df)

#### add in condition part #### 
# Turn "part" numeric for the loop 
switch_df_control$part <- as.numeric(switch_df_control$part)

for(i in unique(switch_df_control$participant)){
  if(mean(switch_df_control$part[switch_df_control$participant == i]) > 2)
  {
    switch_df_control$condition[switch_df_control$participant == i] <- "Instructions"
  } else {
    switch_df_control$condition[switch_df_control$participant == i] <- "No_instructions"
  }
}

#tidy
rm(i)

#### Create column for whether they made the optimal choice or not ####
# Make side vs centre first 
# 1 = centre, 2 & 3 = sides
switch_df_control$centre <- ifelse(switch_df_control$fixated_box == 1, 1, 0)

#### save the file (everything) ####
save(switch_df_control, file = "scratch/switch_df_control")

#### Create version with Na's removed ####
switch_df_control <- switch_df_control[complete.cases(switch_df_control),]

# create save files
save(switch_df_control, file = "scratch/switch_df_control_nar")

#### PART 1 ####
#### Read in part 1 measures for accuracy across distances ####
# create dataframe
df <- tibble(
  block = numeric(),
  separation = numeric(),
  accuracy = numeric()
)

# colnames 
import_names <- c(
  "block",
  "separation",
  "accuracy"
)

# set up directory for loop 
results_files <- dir("data/Control_dat/part_1/")

for (f in results_files){
  d <- read.csv(
    paste("data/Control_dat/Part_1/", f, sep=""), header = F)
  
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
rm(d, f, import_names, results_files)

# re-order
df <- select(df,
             participant,
             block,
             separation,
             accuracy)

# remove NA trials 
df <- df[complete.cases(df),]

#### Calculate curve for accuracy across distances ####
# data frame for accuracy accross separations
acc_sep <- tibble(
  participant = character(),
  separation = numeric(),
  accuracy = numeric()
)


# loop through participants for accuracy over all separations
for (p in unique(df$participant))
{
  # general linear model
  ss = df[which(df$participant==p),]
  m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
  
  # get unique distances 
  seps <- unique(switch_df_control$separation[switch_df_control$participant == p])
  seps <- c(0, seps, seps*2)
  seps <- unique(seps)
  
  # now loop
  for(i in seps){
    y = predict(m, data.frame(separation = i), type = "response")
    
    # add into new data frame
    acc_sep <- rbind(acc_sep, data.frame(participant = p,
                                         separation  = i,
                                         accuracy = y))
  }
}

# tidy 
rm(m, ss, i, p, y, seps)

# save this
save(acc_sep, file = "scratch/acc_sep_contopt")






