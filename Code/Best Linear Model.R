library(leaps)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat_cleaned.csv")

################# Linear Regression ################# 
bf_lm <- lm(BODYFAT ~. -IDNO -DENSITY, data = bodyFat) # Base linear model
best_subset <- regsubsets(BODYFAT ~. -IDNO -DENSITY, data = bodyFat, 
                          nbest = 1, nvmax = NULL, force.in = NULL, force.out = NULL, 
                          method = "exhaustive")
summary_best_subset <- summary(best_subset)
as.data.frame(summary_best_subset$adjr2)
summary_best_subset$which[2,]

best_bf_lm <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = bodyFat)
summary(best_bf_lm)

# Separate to train and test sets
set.seed(123)
n <- dim(bodyFat)[1]
ind <- sample(1:n, size = n * 0.75)

train <- bodyFat[ind,]
test <- bodyFat[-ind,]

# Correlation Coefficient
cor(predict(bf_lm, newdata=test), test$BODYFAT)^2
cor(predict(best_bf_lm, newdata=test), test$BODYFAT)^2
