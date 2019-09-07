#### Plotting Script #### 
# 2017/18
# Written by Warren James
# Script used to make plots of proportion of fixations 
# made to the centre or side box(es)
# only deals with the motivated group 


#### libraries needed ####
library(tidyverse)
library(psyphy) 

#### constants ####
Screen_dist <- 60
x_res <- 1920
x_width <- 54
ppcm <- x_res/x_width

#### Functions #### 
get_VisDegs <- function(size,distance){
  ((2*atan2(size,(2*distance)))*180)/pi
}

#### load in data ####
# part2
load("scratch/switch_nar_data")

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
results_files <- dir("data/results/part_1/")

for (f in results_files){
  d <- read.csv(
    paste("data/results/Part_1/", f, sep=""), header = F)
  
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
rm(d, f, import_names, results_files, temp)

# re-order
df <- select(df,
             participant,
             block,
             separation,
             accuracy)

# remove NA trials 
df <- df[complete.cases(df),]

# save this 
save(df, file = "scratch/Part1_peng")

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
 seps <- unique(switch_df$separation[switch_df$participant == p])
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
rm(m, ss, i, p, y)

# save this
save(acc_sep, file = "scratch/acc_sep_peng")

# tidy 
rm(m, ss, i, p, sep, y)

#### Part 1 plots #### 
agdat <- df %>%
  na.omit %>%
  group_by(participant, separation) %>% 
  summarise(meanAcc = mean(accuracy))

# make plot
plt <- agdat %>%
  ggplot(aes(separation, meanAcc)) + 
  stat_smooth(colour = "black", method = glm,
              method.args = list(family = binomial(mafc.logit(2))),
              se = F, fullrange = T) + 
  geom_point() + 
  theme_minimal() + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(~participant) + 
  scale_y_continuous(breaks = seq(0.25, 1, 0.25)) + 
  scale_x_continuous(breaks = seq(0, 450, 150))
plt$labels$x <- "Proportion Correct"
plt$labels$y <- "Delta (pixels)"
plt

# example plot
participant <- "19"
# get values
separation <- round(as.numeric(unique(switch_df$switch_point[switch_df$participant == participant])))
meanAcc <- acc_sep$accuracy[acc_sep$participant == participant & acc_sep$separation == separation]
acc <- as.data.frame(cbind(meanAcc, separation))

# make plot 
plt <- agdat[agdat$participant == participant,] %>%
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist), meanAcc)) + 
  stat_smooth(colour = "blue", method = glm,
              method.args = list(family = binomial(mafc.logit(2))),
              se = F, fullrange = T) + 
  geom_point() + 
  theme_bw() + 
  geom_segment(data = acc,
               aes(xend = get_VisDegs(separation/ppcm,
                                      Screen_dist),
                   yend = -Inf)) + 
  geom_segment(data = acc,
               aes(xend = -Inf,
                   yend = meanAcc))
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt

# save 
# ggsave("../Figures/Part_1_Example.png", height = 10, width = 10)

#### PART 2 ####
#### make proportion plots ####
# should remove participant that didn't complete all blocks 
switch_df <- switch_df %>%
  group_by(participant) %>%
  filter(max(block) == 4)

# centre fixations
centre_dat <- switch_df %>%
  group_by(participant, separation) %>% 
  summarise(prop_fixated = mean(centre)) %>%
  mutate(box = "centre")

# side fixations
side_dat <- mutate(centre_dat, prop_fixated = 1 - prop_fixated,
                   box = "side") 

# merge these
prop_dat <- rbind(centre_dat, side_dat)

# add in switch point data 
switching_points <- switch_df %>%
  group_by(participant) %>%
  summarise(switch_point = unique(switch_point))

# round this
switching_points$switch_point <- as.numeric(switching_points$switch_point)
switching_points$switch_point <- round(switching_points$switch_point)

#### make plots ####
prop_plt <- prop_dat %>% 
  ggplot(aes(get_VisDegs(separation/ppcm, Screen_dist), prop_fixated)) + 
  geom_area(aes(colour = box,
                fill = box)) + 
  geom_vline(data = switching_points, 
             aes(xintercept = get_VisDegs(switch_point/ppcm, Screen_dist)),
             linetype = "dashed") + 
  facet_wrap(~participant) + 
  theme_minimal() + 
  theme(strip.text.x = element_blank()) + 
  ggthemes::scale_colour_ptol() + 
  ggthemes::scale_fill_ptol()
prop_plt$labels$x <- "Delta (Visual Degrees)"
prop_plt$labels$y <- "Proportion of time fixating:"
prop_plt$labels$colour <- "Box location"
prop_plt$labels$fill <- "Box location"
prop_plt
# to save a copy
# ggsave("../Figures/proportions_plot.png", width = 10, height = 10)

# same again, but with dots
# So this needs to be just dots to show the proportion of fixations to the side boxes 
side_fixations <- prop_dat[prop_dat$box == "side",]
side_fixations <- merge(side_fixations, switching_points)

dot_plt <- ggplot(side_fixations, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                     prop_fixated))
dot_plt <- dot_plt + geom_point() 
dot_plt <- dot_plt + geom_vline(aes(xintercept = get_VisDegs(switch_point/ppcm, Screen_dist)),
                              linetype = "dashed")
dot_plt <- dot_plt + scale_y_continuous(name="Proportion of fixations to one of the side boxes")
dot_plt <- dot_plt + scale_x_continuous(name="separation (pixels)")
dot_plt <- dot_plt + theme_minimal() 
dot_plt <- dot_plt + theme(strip.text.x = element_blank())
dot_plt <- dot_plt + facet_wrap(~participant)
dot_plt
# save
# ggsave("../Figures/Part_2.png", height = 10, width = 10)

