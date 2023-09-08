# Reading data.
train <- read.table(file.path(getwd(), "train2.txt"))
test <- read.table(file.path(getwd(), "train3.txt"))

## Filtering to 2’s and 3’s and desired variables.
train <- train[train[,1] %in% c(2, 3),]
test <- test[test[,1] %in% c(2, 3),]
pixels <- c("V1", "V3", "V5", "V7", "V15")
train <- train[,pixels]
test <- test[,pixels]

# Running linear regression.
lin.mod <- lm(train[,1] ˜ ., data=train[,-1])
weighted.ave <- predict(lin.mod, test[,2:5])
pred.vals.lin <- ifelse(weighted.ave>2.5, 3, 2)
error.rate.lin <- mean(pred.vals.lin!=test[,1])

# Running K-nearest neighbors.
require(class)
pred.vals.knn <- knn(train[,2:5], test[,2:5], train[,1], k=5)
error.rate.knn <- mean(pred.vals.knn!=test[,1])

# Comparing the two errors
print(error.rate.lin)
print(error.rate.knn)