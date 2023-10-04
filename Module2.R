library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

# convert all units to metric
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm


# data exploration
summary(bodyFat)

norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))
corr_matrix <- cor(norm_bodyFat[, 3:16])
corrplot(corr = corr_matrix, method = "color") # base correlation plots

# PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]

fviz_eig(data.pca, addlabels = TRUE) # scree plot shows that PC 1 and 2 represent most of the data
fviz_cos2(data.pca, choice = "var", axes = 1:3)
fviz_pca_var(data.pca, col.var = "cos2", repel = TRUE, axes = c(1, 2))


bodyFat$calc_BODYFAT <- 495/bodyFat$DENSITY - 450
bodyFat$calc_BMI <- bodyFat$WEIGHT / (bodyFat$HEIGHT^2)

# TODO: DEFINE GOOD VS. GREAT LEVELS OF PRECISION FOR ACCURACY AND ROBUSTNESS
# NOTE: GOOD MODELS TEND TO HAVE R^2 AROUND 0.75

# compare age distribution curve to overall US population age distribution 
# curve to assess if group is generally representative
# might be good to do weighted least squares where the weight is % of us population at that age
ggplot(bodyFat) + geom_histogram(aes(x = AGE))
ggplot(bodyFat) + geom_histogram(aes(x = HEIGHT))
ggplot(bodyFat) + geom_histogram(aes(x = WEIGHT))

# probably gonna have to use linear regression
y_bodyFat <- bodyFat[bodyFat$AGE < 35, ]
m_bodyFat <- bodyFat[(bodyFat$AGE >= 35) & (bodyFat$AGE < 60), ]
o_bodyFat <- bodyFat[bodyFat$AGE > 60]

y_bf_lm <- lm(BODYFAT ~ . -IDNO -DENSITY -calc_BODYFAT -calc_BMI, data = y_bodyFat)
summary(y_bf_lm)

# In the shiny app, maybe have a BMI calculator that updates graphics based on their data

# PCA Analysis 
