library(tidyverse)
library(corrplot)
library(factoextra)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat_cleaned.csv")

# correlation plots
norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))

corr_matrix <- norm_bodyFat %>% 
  select(-HEIGHT, -WEIGHT, -DENSITY, -BODYFAT) %>%
  cor()
corrplot(corr = corr_matrix, method = "color") # base correlation plots

#PCA 
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]

fviz_eig(data.pca, addlabels = TRUE) # scree plot shows that PC 1 and 2 represent most of the data
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2", repel = TRUE, axes = c(1, 2))
