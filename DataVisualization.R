library(caret)
library(ggplot2)
library(corrplot)
library(psych)
library(tidyr)
library(dplyr)
library(ggridges)
library(ggpubr)
data <- readRDS("./data/data_final.RDS")


df_test = data$train



# TODO obravnavamo kot fakotrske če obstaja več kot 15 različnih vrednosti, čene bodo numerične, 
# TODO nariši korelacijsko matriko
# TODO izpiši tabelo informacij za podatke

# Histograms for numerical data
p = ggplot(df_test, aes(x=TransactionAmt, fill=isFraud)) + geom_histogram(binwidth = 10) + scale_x_continuous(limits = c(0, 1000)) + theme_minimal()
ggsave("./slike/transactionAmt.pdf", p, width = 6, height = 6)

### Card Variables
p1 = ggplot(df_test, aes(x=card1, fill=isFraud)) + geom_histogram(binwidth = 100, show.legend = FALSE) + theme_minimal() + theme(legend.title = element_blank())
p2 = ggplot(df_test, aes(x=card2, fill=isFraud)) + geom_histogram(binwidth = 5, show.legend = FALSE) + theme_minimal() + theme(legend.title = element_blank())
p3 = ggplot(df_test, aes(x=card5, fill=isFraud)) + geom_histogram(binwidth = 5 ,show.legend = FALSE) + theme_minimal() + theme(legend.title = element_blank())
p4 = ggplot(df_test, aes(x=card6, fill=isFraud)) + geom_bar(show.legend = FALSE) + theme_minimal() + theme(legend.title = element_blank())

skp = ggarrange(p1, p2, p3, p4, ncol=2, nrow = 2)
ggsave("./slike/card.pdf", skp, width = 6, height = 6)

### Email domains
#ggplot(data=df_test, aes(x=P_emaildomain, fill=isFraud)) + geom_bar() +  theme_minimal()

# Prikazemo le bolj pogoste vrednosti

p = df_test %>%
  group_by(P_emaildomain) %>%
  mutate(group_num = n()) %>%
  dplyr::filter(group_num >= 3000) %>%
  ggplot(aes(x=P_emaildomain, fill=isFraud)) + geom_bar() +  theme_minimal() 

ggsave("./slike/email.pdf", p, width = 6, height = 6)
## V variables

p = df_test %>% select(V76, V78, V83, V283, V285, V294, V296, isFraud) %>% gather(Spremenljivke, value, -isFraud) %>% 
  ggplot(aes(y = Spremenljivke,x = percent_rank(value))) + geom_density_ridges(scale=1) + labs(x = "Normalizirane vrednosti", y="Spremenljivke") + theme_minimal()

ggsave("./slike/v_variables.pdf", p, width = 6, height = 6)


## C variables
p =df_test %>% select(C1, C2, C6, C9, C11, C13, C14, isFraud) %>% gather(Spremenljivke, value, -isFraud) %>% 
  ggplot(aes(y = Spremenljivke, x = percent_rank(value)))  + geom_density_ridges(scale=1) + labs(x = "Normalizirane vrednosti", y="Spremenljivke") + theme_minimal()
ggsave("./slike/c_variables.pdf", p, width = 6, height = 6)




# Correlation Matrix
df_numeric = df_test[, c("TransactionAmt", "card1", "card2", "card5","V76", "V78", "V83", "V283", "V285", "V294", "V296",
                         "C1", "C2", "C6", "C9", "C11", "C13", "C14")]
M = cor(df_numeric)
library(ggcorrplot)
p = ggcorrplot(M)
ggsave("./slike/korelacijska_matrika.pdf", p, width = 6, height = 6)




# Tabela za numerične podatke
describe(df_test[, c("TransactionAmt", "card1", "card2", "card5", "card5","V76", "V78", "V83", "V283", "V285", "V294", "V296",
                     "C1", "C2", "C6", "C9", "C11", "C13", "C14")])



# Za printat tabelo v Rmd
# 
# ```{r,echo=FALSE}
# povzetek <- as.data.frame(psych::describe(podatki[, kratkaImena]))
# 
# knitr::kable(round(povzetek[,c(2, 3, 4, 5, 8, 9, 10, 11, 12)],2),"latex") %>%
#   kable_styling(bootstrap_options = c("striped", "condensed",  "responsive","bordered"),full_width = F)
# ```


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
