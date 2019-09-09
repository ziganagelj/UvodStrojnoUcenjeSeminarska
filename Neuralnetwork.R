library(keras)
library(caret)


nn <- keras_model_sequential()
nn %>%
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 5, activation = "tanh") %>%
  layer_dense(units = 1, activation = 'sigmoid')

summary(nn)

nn %>% compile(
  loss = "binary_crossentropy", 
  optimizer = "adam"
)

checkpoint <- callback_model_checkpoint(
  filepath = "nn.hdf5", 
  save_best_only = TRUE, 
  period  = 1
)

early_stopping <- callback_early_stopping(patience = 5)

history_nn <- nn %>% fit(
  x = x_train, 
  y = y_train, 
  epochs = 5, 
  batch_size = 128,
  validation_data = list(x_test, y_test), 
  callbacks = list(checkpoint, early_stopping)
)

plot(history)

pred_train <- predict(nn, x_train)
pred_test <- predict(nn, x_test)

nn.confusion <- confusionMatrix(as.factor(round(pred_test)), as.factor(y_test), mode = 'prec_recall', positive = '1')




plot_model(model, to_file='neuralnetwork.png')

model %>% plot_model()
