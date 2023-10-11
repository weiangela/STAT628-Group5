library(tidyverse)
library(ggplot2)
library(corrplot)
library(factoextra)
library(rpart)
library(rpart.plot)
library(haven)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

################# Data Cleaning #################
bodyFat <- bodyFat %>% filter(BODYFAT > 3, HEIGHT > 60)

# convert all units to metric
bodyFat$WEIGHT <- bodyFat$WEIGHT / 2.20462  # kg
bodyFat$HEIGHT <- bodyFat$HEIGHT * 2.54     # cm

# Create calculation fields to compare estimations to actual values (verify they were entered correctly)
bodyFat$calc_BODYFAT <- 495/bodyFat$DENSITY - 450
bodyFat$calc_BMI <- bodyFat$HEIGHT / (bodyFat$WEIGHT^2)

# Differentiate study participants by ages
bodyFat$AGE_GROUP <- "Y"
bodyFat[(bodyFat$AGE >= 35) & (bodyFat$AGE < 60), ]$AGE_GROUP <- "M"
bodyFat[bodyFat$AGE > 60, ]$AGE_GROUP <- "O"

#################Data exploration #################

# histograms of all data
library(Hmisc)
bodyFat %>% select(-IDNO) %>% hist.data.frame()
bodyFat %>% select(-IDNO) %>% summary()

# correlation plots
norm_bodyFat <- as.data.frame(scale(bodyFat[, 2:17]))

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
bf_lm <- lm(BODYFAT ~. -IDNO -DENSITY -calc_BODYFAT -calc_BMI, data = bodyFat) # Base linear model
summary(bf_lm)

bf_y <- bodyFat %>% filter(AGE_GROUP == "Y")
bf_lm_y <- lm(BODYFAT ~ ABDOMEN + WRIST,data = bf_y)
bf_m <- bodyFat %>% filter(AGE_GROUP == "M")
bf_lm_m <- lm(BODYFAT ~ ABDOMEN + WRIST,data = bf_y)

bf_lm_o <- bodyFat %>% filter(AGE_GROUP == "O") 
bf_lm_m <- lm(BODYFAT ~ ABDOMEN + WRIST,data = bf_y)

summary(bf_lm_y)
summary(bf_lm_m)
summary(bf_lm_o)


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
