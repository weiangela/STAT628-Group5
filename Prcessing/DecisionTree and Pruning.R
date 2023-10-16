library(rpart)
library(rpart.plot)
library(modeldata)

bodyFat <- read.csv("https://raw.githubusercontent.com/weiangela/STAT628-Group5/main/BodyFat.csv")

# Build a decision tree and Plot.
bodyFat_regression <- bodyFat[, -c(1,3)]
fit <- rpart(BODYFAT~., data = bodyFat_regression)
printcp(fit) # Similar to Summary()
rpart.plot(fit)

# Pruning and Plot optimized decision tree.
opt=which.min(fit$cptable[,"xerror"])
cp = fit$cptable[opt, "CP"]
fit_prune <- prune(fit, cp=cp)
rpart.plot(fit_prune)
