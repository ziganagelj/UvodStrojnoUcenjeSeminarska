library(caret)
library(data.table)

df_trans = read.csv("./data/train_transaction.csv")
options(scipen = 999) # za decimalni prikaz
head(df_trans)



# correctly detected NA and factors
df_trans$isFraud <- as.factor(df_trans$isFraud)
levels(df_trans$card4)[1] <- NA
levels(df_trans$card6)[1] <- NA
df_trans$addr1 <- as.factor(df_trans$addr1)
df_trans$addr2 <- as.factor(df_trans$addr2)
levels(df_trans$P_emaildomain)[1] <- NA
levels(df_trans$R_emaildomain)[1] <- NA

summary(df_trans[,2])
# MISSING VALUES ----
na_count <- sapply(df_trans, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count = na_count / 590540
na_count # delež NA per column

df_trans_NA <- df_trans[, -which(colMeans(is.na(df_trans)) > 0.20)]
head(df_trans_NA)

# NEAR ZERO VARIANCE ----
nearZero <- nearZeroVar(df_trans_NA, saveMetrics = TRUE)
badCOls <- nearZeroVar(df_trans_NA)
badCOls <- badCOls[3:length(badCOls)]

df_trans_NA_remove <- df_trans_NA[, -badCOls]  # samo is fraud treba dat vija
head(df_trans_NA_remove)
# TransactionDT engineered ----



time <- df_trans_NA_remove$TransactionDT
time_mod <- (time - min(time)) %% (86400*7)
day <- rep(7, length(time_mod))

for (i in 6:1) {
  idx <- which(time_mod < i * 86400)
  day[idx] <- i
  
}

day_f <- as.factor(as.character(day))

df_trans_NA_remove$TransactionDT <- day_f

# complete
df_trans_com <- df_trans_NA_remove[complete.cases(df_trans_NA_remove), -c(1, 28, 12, 13)]
head(df_trans_com)
summary(df_trans_com$P_emaildomain)
summary(df_trans_com[df_trans_com$isFraud == 1,]$P_emaildomain)

rmv <- c('servicios-ta.com', 'yahoo.co.jp', 'yahoo.co.uk', 'yahoo.de', 'live.fr', 'hotmail.de')
df_trans_com <- df_trans_com[!df_trans_com$P_emaildomain %in% rmv, ]
df_trans_com$P_emaildomain <- droplevels(df_trans_com$P_emaildomain)

saveRDS(df_trans_com, "./data/data_complete.rds")

summary(df_trans_com)
# Logstic Regression

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
