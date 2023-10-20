library(readr)
library(dplyr)

# reading dataset

df <-  read_csv("https://raw.githubusercontent.com/HumayDS/Preprocessing-tips/main/tips2.csv")

df %>%  head()

df %>%  View()

df <-  df %>%  select(-...1)


df %>%  glimpse()

df %>%  summary()

df$time = factor(df$time)


df$sex <-  factor(df$sex)
df$smoker <-  factor(df$smoker)
df$day <-  factor(df$day)
df$size <-  factor(df$size , levels = c(1 , 2,3,4,5,6))

df %>%  summary()



df <-  df %>%  arrange(desc(total_bill))

df %>%  head()


df %>%  rename("cins" = "sex")


df %>%  select(tip, total_bill) %>%  
  rename("caevoy" = "tip" , "hesab" = "total_bill") %>% 
  arrange(desc(caevoy)) %>%  slice(1:10)

# yeni clumn elave et 
df %>%  mutate(cemi = total_bill + tip)

df %>%  mutate(cemi = total_bill + tip , orta_hesab = cemi / size)


df %>%  mutate(tip_cat = case_when(  tip > 5 ~ "Yes",
                                     TRUE ~ "No"
)) 



df[10:15 ,  1:3]

table(df$size)
 
df$sex %>%  unique()

df$size %>%  unique()


# column sec ve filterle

df %>%  filter(tip > 20)

df %>% filter((tip > 15) | (total_bill < 50))  #veya

df %>% filter((tip > 15)  & (total_bill < 50))  #ve



#Missing values 
summary(df)
is.na(df) %>%  sum()

index  <-  which(is.na(df$tip))
index

df[index, ]

df %>%  summary()


# deleting missing values 

df_remove_missing <-  na.omit(df)

df_remove_missing %>%  summary()


# Mean Imputation


ortalama <-  mean(df$tip , na.rm = T)

df_missing_imputation <-  df 

df_missing_imputation$tip[index] <-  ortalama 

df %>%  summary()

df_missing_imputation %>%  summary()

df$tip[index] 




# agregat funskiyalari
df_remove_missing %>%  summary()
mean(df_remove_missing$tip)
sd(df_remove_missing$tip)
median(df_remove_missing$tip)
min(df_remove_missing$tip)
max(df_remove_missing$tip)
var(df_remove_missing$tip)
quantile(df_remove_missing$tip , probs = 0.25 )
quantile(df_remove_missing$tip  )


# outlier detection emprical rule

boxplot(df_remove_missing$tip)
hist(df_remove_missing$tip)



lower_bound <-  quantile(df_remove_missing$tip , probs = 0.05)

upper_bound <-  quantile(df_remove_missing$tip , probs = 0.95)

outlier_ind <- which(df_remove_missing$tip < lower_bound | df_remove_missing$tip > upper_bound)
outlier_ind
df_remove_missing[outlier_ind,]


#outlieri sil
df_remove_outlier <-  df_remove_missing[-outlier_ind,]

# outlieri ortalamasi ile evezle


df_out_imputation <-  df_remove_missing

df_out_imputation$tip[outlier_ind] <-  ortalama 

df_out_imputation %>%  summary()

#outlier detection box plot method


q1 <-  quantile(df_remove_missing$tip ,probs = 0.25)
q3 <-  quantile(df_remove_missing$tip , probs = 0.75)
iqr = q3 - q1

up_bound <-  q3 + 1.5*iqr

low_bound <-  q1 - 1.5*iqr



out_index <-  which(df_remove_missing$tip < low_bound | df_remove_missing$tip > up_bound)

df_remove_missing$tip[out_index] <-  ortalama 

write_csv(x = df_remove_missing , "df.csv")

#pivoting

df %>%  group_by(sex) %>% summarize(orta_mean = mean(tip, na.rm = T))








