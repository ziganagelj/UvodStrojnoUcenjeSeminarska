library(class)
library(caret)
library(ggplot2)
data <- readRDS("./data/data_final_std2.rds")

df_train = data$x_train
df_train$y_train = as.numeric(as.factor(data$y_train))
df_train$P_emaildomain = as.numeric(df_train$P_emaildomain)
df_train$card6 = as.numeric(df_train$card6)


df_test = data$x_test
df_test$y_test = as.numeric(as.factor(data$y_test))
df_test$P_emaildomain = as.numeric(df_test$P_emaildomain)
df_test$card6 = as.numeric(df_test$card6)

lbl = c("TransactionAmt", "card1", "card2", "card5", "card6", "P_emaildomain","V76", "V78", "V83", "V283", "V285", "V294", "V296", "C1", "C2", "C6", "C9", "C11", "C13", "C14")


accuracy_0 = NULL
accuracy_1 = NULL

# Za risanje scree diagrama
for (i in 1:15) {
  
  print(i)
  knn_model = knn(df_train[,lbl], df_test[, lbl], cl=df_train$y_train,k=i, prob = TRUE)
  knn_prob = attr(knn_model, "prob")
  
  # Confusion matrix
  cm = table(knn_model, df_test$y_test)
  
  rowsums = apply(cm, 1, sum)
  colsums = apply(cm, 2, sum)
  diag = diag(cm)
  
  # Percision per-class,
  precision = diag / colsums
  
  accuracy_0 = c(accuracy_0, precision[1]) # Not Froud
  accuracy_1 = c(accuracy_1, precision[2]) # Froud accuracy
}

plot(1:15, accuracy_0, type = "l")
plot(1:15, accuracy_1, type = "l")


p = ggplot() + 
  geom_line(aes(x = 1:15, y = accuracy_0, colour="blue"), size=1) +
  geom_line(aes(x = 1:15, y = accuracy_1, colour="red"), size=1) + scale_color_discrete(name = "Razred", labels = c("Fraud", "NotFraud")) + labs(x = "Stevilo sosedov", y="Natacnost") + theme_minimal()

ggsave("./slike/knn_scree.pdf", p, width = 6, height = 6)
geom_line(data=Summary,aes(y=Y1,x= X,colour="darkblue"),size=1 )
apply(df_train, 2, function(x) any(is.na(x)))


lbl = c("TransactionAmt", "card1", "card2", "card5", "card6", "P_emaildomain","V76", "V78", "V83", "V283", "V285", "V294", "V296", "C1", "C2", "C6", "C9", "C11", "C13", "C14")

# Treniranje knn
knn_model = knn(df_train[,lbl], df_test[,lbl], cl=df_train$y_train,k=1, prob = TRUE);
knn_prob = attr(knn_model, "prob");
knn_cm = table(knn_model, df_test$y_test)
knn_labels =  as.integer(knn_model)
knn_labels = knn_labels - 1



# Confusion matrix
cm = table(knn_model, df_test$y_test)
cm
n = sum(cm)
bc = nrow(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)
p = rowsums / n
q = colsums / n
diag = diag(cm)

# Accuracy
accuracy = sum(diag) / n

# Percision per-class, recall, F-1
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1)

# Average ppc, recall, F-1
data.frame(mean(precision), mean(recall), mean(f1))


log_confusion <- confusionMatrix(as.factor(knn_model), as.factor(df_test$y_test), mode = 'prec_recall', positive = '1')
saveRDS(log_confusion, file = "knn_pred.RDS")
readRDS("./data/knn_pred.RDS")