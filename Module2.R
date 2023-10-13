library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)
library(haven)
library(leaps)
library(Hmisc)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

################# Data Cleaning #################
# convert all units to metric
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm

#Cutting out unreasonable values and outliers to ensure normality assumption
bodyFat <- bodyFat %>% filter(BODYFAT > 3, HEIGHT > 90, WEIGHT < 160, 
                              ADIPOSITY < 45, NECK < 50, ABDOMEN < 140, 
                              HIP < 140, THIGH < 80, KNEE < 47, 
                              ANKLE < 32, BICEPS < 42)

# Create calculation fields to compare estimations to actual values (verify they were entered correctly)
bodyFat$calc_BODYFAT <- 495/bodyFat$DENSITY - 450
bodyFat$calc_BMI <- bodyFat$HEIGHT / (bodyFat$WEIGHT^2)

#################Data exploration #################
# histograms of all data
bodyFat %>% select(-IDNO) %>% hist.data.frame()
norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))
norm_bodyFat %>% hist.data.frame()

# correlation plots
corr_matrix <- norm_bodyFat %>% 
  select(-HEIGHT, -WEIGHT, -DENSITY, -BODYFAT) %>%
  cor()
corrplot(corr = corr_matrix, method = "color") # base correlation plots

################# PCA ################# 
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:4]

fviz_eig(data.pca, addlabels = TRUE) # scree plot shows that PC 1 and 2 represent most of the data
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2", repel = TRUE, axes = c(1, 2))

################# Linear Regression ################# 
bf_lm <- lm(BODYFAT ~. -IDNO -DENSITY -calc_BODYFAT, data = bodyFat) # Base linear model
best_subset <- regsubsets(BODYFAT ~. -IDNO -DENSITY -calc_BODYFAT, data = bodyFat, 
                          nbest = 1, nvmax = NULL, force.in = NULL, force.out = NULL, 
                          method = "exhaustive")
summary_best_subset <- summary(best_subset)
as.data.frame(summary_best_subset$adjr2)
summary_best_subset$which[2,]

best_bf_lm <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = bodyFat)
summary(best_bf_lm)

################# Decision Tree ################# 
set.seed(123)
n <- dim(bodyFat)[1]
ind <- sample(1:n, size = n * 0.75)

train <- bodyFat[ind,]
test <- bodyFat[-ind,]

obj_tree <- rpart(BODYFAT ~ . -IDNO -DENSITY -calc_BODYFAT, 
                  method = "anova",
                  control = rpart.control(minbucket = 5, cp=0.0001), 
                  data = train)

basic_tree <- rpart(BODYFAT ~ . -IDNO -DENSITY -calc_BODYFAT -AGE, 
                    method = "anova",
                    data = train)
rpart.plot(basic_tree)
cor(predict(basic_tree, newdata=test), test$BODYFAT)^2

rsq.rpart(obj_tree)
plotcp(obj_tree)
rpart.plot(obj_tree)

CP <- obj_tree$cptable[,1]
cp.opt <- CP[which.min(cptable[,4])]

# Prune the tree 
fit <- prune(obj_tree, cp = cp.opt, minbucket = 4)
rpart.plot(fit)

# Correlation Coefficient
cor(predict(fit, newdata=test), test$BODYFAT)^2
cor(predict(bf_lm, newdata=test), test$BODYFAT)^2
cor(predict(bf_lm_1, newdata=test), test$BODYFAT)^2


################# TO DO ################# 
# TODO: DEFINE GOOD VS. GREAT LEVELS OF PRECISION FOR ACCURACY AND ROBUSTNESS
# NOTE: GOOD MODELS TEND TO HAVE R^2 AROUND 0.75
# In the shiny app, maybe have a BMI calculator that updates graphics based on their data
# compare age distribution curve to overall US population age distribution 
# curve to assess if group is generally representative
# might be good to do weighted least squares where the weight is % of us population at that age
