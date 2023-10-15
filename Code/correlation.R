library(tidyverse)
library(corrplot)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat_cleaned.csv")

# correlation plots
norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))

corr_matrix <- norm_bodyFat %>% 
  select(-HEIGHT, -WEIGHT, -DENSITY, -BODYFAT) %>%
  cor()
corrplot(corr = corr_matrix, method = "color") # base correlation plots
