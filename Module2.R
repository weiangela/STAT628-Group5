library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)
library(rpart)
library(rpart.plot)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

# convert all units to metric

bodyFat$calc_BODYFAT <- 495/bodyFat$DENSITY - 450
bodyFat$calc_BMI <- bodyFat$HEIGHT / (bodyFat$WEIGHT^2)
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm

# data exploration
summary(bodyFat)

norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))

corr_matrix <- norm_bodyFat %>% 
  select(-HEIGHT, -WEIGHT, -DENSITY, -BODYFAT) %>%
  cor()
corrplot(corr = corr_matrix, method = "color") # base correlation plots

# PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]

fviz_eig(data.pca, addlabels = TRUE) # scree plot shows that PC 1 and 2 represent most of the data
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2", repel = TRUE, axes = c(1, 2))

# TODO: DEFINE GOOD VS. GREAT LEVELS OF PRECISION FOR ACCURACY AND ROBUSTNESS
# NOTE: GOOD MODELS TEND TO HAVE R^2 AROUND 0.75

# compare age distribution curve to overall US population age distribution 
# curve to assess if group is generally representative
# might be good to do weighted least squares where the weight is % of us population at that age
ggplot(bodyFat) + geom_histogram(aes(x = AGE))
ggplot(bodyFat) + geom_histogram(aes(x = ABDOMEN))
ggplot(bodyFat) + geom_histogram(aes(x = WEIGHT))

# probably gonna have to use linear regression
bf_lm <- lm(BODYFAT ~. -IDNO -DENSITY -calc_BODYFAT -calc_BMI, data = bodyFat)
bf_lm_1 <- lm(BODYFAT ~ AGE + THIGH + HIP, data = bodyFat)

summary(bf_lm)
summary(bf_lm_1)

bf_lm_2 <- lm(BODYFAT ~ AGE + NECK + ABDOMEN + FOREARM + WRIST, data = bodyFat)
summary(bf_lm_2)

bodyFat$AGE_GROUP <- "Y"
bodyFat[(bodyFat$AGE >= 35) & (bodyFat$AGE < 60), ]$AGE_GROUP <- "M"
bodyFat[bodyFat$AGE > 60, ]$AGE_GROUP <- "O"

y_bf_lm <- lm(BODYFAT ~ . -IDNO -DENSITY -calc_BODYFAT -calc_BMI, data = y_bodyFat)
summary(y_bf_lm)


ab_lm <- lm(BODYFAT ~ ABDOMEN, data = bodyFat)
summary(ab_lm)
# In the shiny app, maybe have a BMI calculator that updates graphics based on their data

# PCA Analysis 

# Decision Tree
n <- dim(bodyFat)[1]
ind <- sample(1:n, size = n/2)

train <- bodyFat[ind,]
test <- bodyFat[-ind,]

obj_tree <- rpart(BODYFAT ~ . -IDNO -DENSITY, 
                  control = rpart.control(xval = 10, minbucket = 5), 
                  data = train)
rpart.plot(obj_tree)
printcp(obj_tree)

cptable <- obj_tree$cptable
# complexity parameter values
CP <- cptable[,1]
# obtain the optimal parameter
cp.opt <- CP[which.min(cptable[,4])]
# Prune the tree 
fit <- prune(obj_tree, cp = cp.opt)
# plot the pruned tree structure
rpart.plot(fit)
printcp(fit)

