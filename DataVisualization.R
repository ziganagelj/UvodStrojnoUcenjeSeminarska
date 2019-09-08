library(caret)
library(ggplot2)
library(corrplot)
library(psych)
library(tidyr)
library(dplyr)
library(ggridges)
data <- readRDS("./data/data_final.RDS")


df_test = data$train



# TODO obravnavamo kot fakotrske če obstaja več kot 15 različnih vrednosti, čene bodo numerične, 
# TODO nariši korelacijsko matriko
# TODO izpiši tabelo informacij za podatke

# Histograms for numerical data
ggplot(df_test, aes(x=TransactionAmt, fill=isFraud)) + geom_histogram(binwidth = 10) + scale_x_continuous(limits = c(0, 1000)) + theme_minimal()


### Card Variables
ggplot(df_test, aes(x=card1, fill=isFraud)) + geom_histogram(binwidth = 100) + theme_minimal()
ggplot(df_test, aes(x=card2, fill=isFraud)) + geom_histogram(binwidth = 5) + theme_minimal()
ggplot(df_test, aes(x=card5, fill=isFraud)) + geom_histogram(binwidth = 5 ) + theme_minimal()
ggplot(df_test, aes(x=card6, fill=isFraud)) + geom_bar() + theme_minimal()

### Email domains
#ggplot(data=df_test, aes(x=P_emaildomain, fill=isFraud)) + geom_bar() +  theme_minimal()

# Prikazemo le bolj pogoste vrednosti
df_test %>%
  group_by(P_emaildomain) %>%
  mutate(group_num = n()) %>%
  dplyr::filter(group_num >= 2000) %>%
  ggplot(aes(x=P_emaildomain, fill=isFraud)) + geom_bar() +  theme_minimal()


## V variables

df_test %>% select(V76, V78, V83, V283, V285, V294, V296, isFraud) %>% gather(Spremenljivke, value, -isFraud) %>% 
  ggplot(aes(y = Spremenljivke, fill = as.factor(isFraud),x = percent_rank(value))) + geom_density_ridges(scale=1) 




## C variables
df_test %>% select(C1, C2, C6, C9, C11, C13, C14, isFraud) %>% gather(Spremenljivke, value, -isFraud) %>% 
  ggplot(aes(y = Spremenljivke, fill = as.factor(isFraud), x = percent_rank(value)))  + geom_density_ridges(scale=1)
+ labs(x = "Normalizirane vrednosti", y="Spremenljivke")
  




# Correlation Matrix
df_numeric = df_test[, c("TransactionAmt", "card1", "card2", "card5", "card5","V76", "V78", "V83", "V283", "V285", "V294", "V296",
                         "C1", "C2", "C6", "C9", "C11", "C13", "C14")]
M = cor(df_numeric)
corrplot::corrplot(M, method = "color")



# Tabela za numerične podatke
describe(df_test[, c("TransactionAmt", "card1", "card2", "card5", "card5","V76", "V78", "V83", "V283", "V285", "V294", "V296",
                     "C1", "C2", "C6", "C9", "C11", "C13", "C14")])

# Tabela za isFraud

table(as.character(df_test$isFraud))
#kable(data.frame(tbl),col.names = c("skupina","število"), caption="število enot v skupinah")


# Za printat tabelo

# 
# ```{r,echo=FALSE}
# povzetek <- as.data.frame(psych::describe(podatki[, kratkaImena]))
# 
# knitr::kable(round(povzetek[,c(2, 3, 4, 5, 8, 9, 10, 11, 12)],2),"latex") %>%
#   kable_styling(bootstrap_options = c("striped", "condensed",  "responsive","bordered"),full_width = F)
# ```

# Staro
ggplot(df_test, aes(x=C13, fill=isFraud)) + geom_histogram(binwidth = 15) + scale_x_continuous(limits = c(-30, 1000))
length(unique(df_test$card5))


ggplot(df_test, aes(x=C14, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-10, 200))
ggplot(df_test, aes(x=V294, fill=isFraud)) + geom_histogram()  + scale_x_continuous(limits = c(-2, 30))
ggplot(df_test, aes(x=C2, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-10, 300))
ggplot(df_test, aes(x=C1, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-10, 300))
ggplot(df_test, aes(x=C6, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-10, 300))
ggplot(df_test, aes(x=C11, fill=isFraud)) + geom_histogram()  + scale_x_continuous(limits = c(-10, 300))
ggplot(df_test, aes(x=V296, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-1, 15))
ggplot(df_test, aes(x=C9, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-10, 150))
ggplot(df_test, aes(x=V285, fill=isFraud)) + geom_histogram() + scale_x_continuous(limits = c(-1, 30))

# Barplot for factor data, TODO prikazi samo faktorje z večjimi deleži


ggplot(data=df_test, aes(x=as.factor(V83), fill=isFraud)) + geom_bar()
ggplot(data=df_test, aes(x=as.factor(V76), fill=isFraud)) + geom_bar()
ggplot(data=df_test, aes(x=as.factor(V78), fill=isFraud)) + geom_bar()
ggplot(data=df_test, aes(x=as.factor(V296), fill=isFraud)) + geom_bar()
ggplot(data=df_test, aes(x=as.factor(V283), fill=isFraud)) + geom_bar()







# Tabela
describe(df_test[, c("TransactionAmt", "card1", "card2", "C13", "card5", "C14", "V294", "C14", "C2", "C1", "C6", "C11", "V296", "C9", "V285")])

# not ggplot
pdf("./images/pc_scree.pdf", width=6, height=6)
fa.parallel(x = R, fa = "pc", n.iter = 100, use = "complete", n.obs = n)
dev.off()

# ggplot
ggsave("games_per_league.pdf", p, width = 6, height = 6)

# table save
table.list3 <- list(t1 = t1,
                    t2 = t2,
                    t3 = t3,
                    t4 = t4,
                    t5 = t5)
# Za printat tabelo

# 
# ```{r,echo=FALSE}
# povzetek <- as.data.frame(psych::describe(podatki[, kratkaImena]))
# 
# knitr::kable(round(povzetek[,c(2, 3, 4, 5, 8, 9, 10, 11, 12)],2),"latex") %>%
#   kable_styling(bootstrap_options = c("striped", "condensed",  "responsive","bordered"),full_width = F)
# ```


saveRDS(table.list3, "./table_list3.rds")
