library(rpart)
library(rpart.plot)
library(modeldata)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/Data/BodyFat_cleaned.csv")

################# Decision Tree ################# 
set.seed(123)
n <- dim(bodyFat)[1]
ind <- sample(1:n, size = n * 0.75)

train <- bodyFat[ind,]
test <- bodyFat[-ind,]

obj_tree <- rpart(BODYFAT ~ . -IDNO -DENSITY, 
                  method = "anova",
                  control = rpart.control(minbucket = 5, cp=0.0001), 
                  data = train)

rpart.plot(obj_tree)
cor(predict(obj_tree, newdata=test), test$BODYFAT)^2

rsq.rpart(obj_tree)
plotcp(obj_tree)

# Prune the tree 
opt=which.min(obj_tree$cptable[,"xerror"])
cp = obj_tree$cptable[opt, "CP"]
fit_prune <- prune(obj_tree, cp=cp)
rpart.plot(fit_prune)
cor(predict(fit_prune, newdata=test), test$BODYFAT)^2
