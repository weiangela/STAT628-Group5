library(tidyverse)
# read data
bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat.csv")
# convert all units to metric
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm

# data cleaning
bodyFat <- bodyFat %>% filter(BODYFAT > 3, HEIGHT > 90, WEIGHT < 160, 
                              ADIPOSITY < 45, NECK < 50, ABDOMEN < 140, 
                              HIP < 140, THIGH < 80, KNEE < 47, 
                              ANKLE < 32,BICEPS<42)
# Export cleaned data
write.csv(bodyFat, file="BodyFat_clean.csv", row.names=FALSE, quote = FALSE)
