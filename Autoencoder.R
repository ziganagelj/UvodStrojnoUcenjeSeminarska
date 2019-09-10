library(keras)
library(Metrics)


ae <- keras_model_sequential()
ae %>%
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 5, activation = "tanh") %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 15, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))

summary(ae)

ae %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)

checkpoint <- callback_model_checkpoint(
  filepath = "autoencoder.hdf5", 
  save_best_only = TRUE, 
  period  = 1
)

early_stopping <- callback_early_stopping(patience = 5)

history <- ae %>% fit(
  x = x_train[y_train == 0,], 
  y = x_train[y_train == 0,], 
  epochs = 100, 
  batch_size = 128,
  validation_data = list(x_test[y_test == 0,], x_test[y_test == 0,]), 
  callbacks = list(checkpoint, early_stopping)
)

plot(history)
pred_train <- predict(ae, x_train)
mse_train <- apply((x_train - pred_train)^2, 1, sum)

pred_test <- predict(ae, x_test)
mse_test <- apply((x_test - pred_test)^2, 1, sum)


summary(mse_train)

possible_k <- seq(0, 1, 0.005)


all <- sapply(possible_k, function(k) {
  predicted <- as.numeric(mse_train > k)
  c <- confusionMatrix(as.factor(predicted), as.factor(y_train), mode = 'prec_recall', positive = '1')
  
  c$byClass[5:7]
  
})
all <- t(all)
rownames(all) <- possible_k
all


qplot(possible_k, all[, 'F1'], geom = "line") + labs(x = "Threshold", y = "F1") + theme_minimal()

k <- round(possible_k[which.max(all[, 'F1'])], 2)

pred_ae <- as.numeric(mse_test > k)
ae.confusion <- confusionMatrix(as.factor(pred_ae), as.factor(y_test), mode = 'prec_recall', positive = '1')
