library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)

# standarizacija in split list strain test itd

get_desc <- function(x) {
  map(x, ~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
} 

#' Given a dataset and normalization constants it will create a min-max normalized
#' version of the dataset.
normalization_minmax <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
}

df <- readRDS('./data/data_final.RDS')
df_train <- df$train

desc <- df$train[, -c(1, 2, 8)] %>% 
  get_desc()

x_train <- df$train[, -c(1, 2, 8)] %>%
  normalization_minmax(desc) %>%
  as.matrix()

x_test <- df$test[, -c(1, 2, 8)] %>%
  normalization_minmax(desc) %>%
  as.matrix()


x_train <- cbind(model.matrix(~ .-1, data = df$train[, c(2, 8)]),
                 x_train)

x_test <- cbind(model.matrix(~ .-1, data = df$test[, c(2,8)]),
                x_test)

y_train <- as.numeric(as.character(df$train[, 1]))
y_test <- as.numeric(as.character(df$test[, 1]))

data_final_std <- list(x_train = x_train,
                       x_test = x_test,
                       y_train = y_train,
                       y_test = y_test)

saveRDS(data_final_std, './data/data_final_std.RDS')

df <- readRDS('./data/data_final_std2.RDS')

pred_knn <- readRDS('./data/knn_napovedi.rds')
pred_log <- readRDS('./data/log_napovedi.rds')
pred <- readRDS('./data/pred_nn_ae.RDS')
pred_nn <- pred$neuralnetwork
pred_ae <- pred$autoencoder

x_train <- df$x_train
x_test <- df$x_test
y_train <- df$y_train
y_test <- df$y_test

res <- cbind(actual = y_test[x_test$P_emaildomain != "hotmail.co.uk"],
            logistic = as.numeric(pred_log),
            knn = as.numeric(pred_knn),
            neuralnetwork = pred_nn[x_test$P_emaildomain != "hotmail.co.uk"],
            autoencoder = pred_ae[x_test$P_emaildomain != "hotmail.co.uk"])

df_test_sub <- subset(df_test, P_emaildomain != "hotmail.co.uk")

saveRDS(res, './data/predictions.RDS')
        