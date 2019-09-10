library(class)
library(caret)


data <- readRDS("./data/data_final_std2.rds")

df_train = data$x_train
df_train$y_train = data$y_train

df_test = data$x_test
df_test$y_test = data$y_test

logit_fit = glm(y_train ~  .,data = df_train, family="binomial")
summary(logit_fit)


df_test_sub <- subset(df_test, P_emaildomain != "hotmail.co.uk") # Izpustimo ker pri treniranju nismo "videli" takega primera
glm_prob = predict(logit_fit, newdata = df_test_sub, type="response", se.fit=FALSE)
glm_pred = ifelse(glm_prob > 0.5, 1, 0)


# # Confusion matrix
# cm = table(glm_pred, df_test_sub$y_test)
# 
# n = sum(cm)
# bc = nrow(cm)
# rowsums = apply(cm, 1, sum)
# colsums = apply(cm, 2, sum)
# p = rowsums / n
# q = colsums / n
# diag = diag(cm)
# 
# # Accuracy
# accuracy = sum(diag) / n
# 
# # Percision per-class, recall, F-1
# precision = diag / colsums
# recall = diag / rowsums
# f1 = 2 * precision * recall / (precision + recall)
# 
# data.frame(precision, recall, f1)
# 
# # Average ppc, recall, F-1
# data.frame(mean(precision), mean(recall), mean(f1))


log_confusion <- confusionMatrix(as.factor(glm_pred), as.factor(df_test_sub$y_test), mode = 'prec_recall', positive = '1')
saveRDS(log_confusion, file = "log_pred.RDS")
readRDS("./data/log_pred.RDS")

