library(caret)
data <- readRDS("./data/data_complete.rds")
small.index <- createDataPartition(data$isFraud, p = 0.5, list = FALSE )

data.small <- data[small.index, ]

train.index <- createDataPartition(data.small$isFraud, p = 0.8, list = FALSE )


# train
train <- data.small[train.index, ]

# test
test <- data.small[-train.index, ]

subsets <- seq(10, 50, 10)
subsets = 20

set.seed(10)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 5)


ProfileLog <- rfe(x = train[, -1],
                  y = train$isFraud,
                  sizes = subsets,
                  rfeControl = ctrl,
                  method="glm")

ProfileLog$optVariables
ProfileTree <- rfe(x = train[, -1],
                   y = train$isFraud,
                   sizes = subsets,
                   rfeControl = ctrl,
                   method="rpart2")

ProfileTree$results
selected <- c("isFraud", ProfileTree$optVariables)

dt <- list(train = train[, selected],
           test = test[, selected])
saveRDS(dt, "./data/data_final.RDS")
