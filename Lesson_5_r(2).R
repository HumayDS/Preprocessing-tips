# install.packages("tidyverse")
# install.packages("dplyr") 
library(readr)
library(dplyr)
# handling working directory
getwd()
setwd("/Users/h.hasilli/Desktop")
getwd()

#Working directory-de olan fayllar
list.files()

# datani cek
df = read.csv("tips_r.csv")


#dataya bax
View(df)

write.csv(df,"tips_r.csv")
glimpse(df)
# tesviri statistikasi
summary(df)

# bashliqi sec
head(df , n = 20)
df %>%  head(5)

df %>%  head(5)
any(is.na(df))

# 10-15 setir , 1-3 sutunu sec 
df[10:15 ,  1:3]
df[, 3]
glimpse(df)
# setir  sayi
nrow(df)
# sutun sayi
ncol(df) 
# reset index
rownames(df) <-  seq(1: nrow(df))
# sampling
sample(df , size = 2)
sample_n(df , size = 2)

?sample()



# sutunlari sec
df$tip
df["tip"]

df %>% select(total_bill , tip)

# classlarini oyren

class(df$"tip")

class(df["tip"])

# tipleri mueyyenleshdir 

glimpse(df)
class(df$size)

df$time = factor(df$time)


df$sex <-  factor(df$sex)
df$smoker <-  factor(df$smoker)
df$day <-  factor(df$day)
df$time <-  factor(df$time)
df$size <-  factor(df$size)

summary(df)
# agregat funskiyalari

mean(df$tip)
sd(df$tip)
median(df$tip)
min(df$tip)
max(df$tip)
var(df$tip)

# histogram cek
hist(df$total_bill)

# unique deyerlerin sayina bax
table(df$size)

# 13-cu setri ve 1:3 sutunlari sil df2ye beraberlesdir
df2 = df[-13 , -c(1:3)]

# total bill 18.78e beraber olan setiri sill

which(df$total_bill == '18.78')
df4 <-  df[-240, ]

glimpse(df)

#selection
df %>%  select(total_bill , tip)
df %>%  select(total_bill , tip , everything())
df %>%  select(-X.1 , -X)

df %>% select(starts_with("s") , everything())

df %>%  pull(total_bill) %>%  mean()

df %>% select_if(is.character) %>%  glimpse()

df %>% select_if(is.factor) %>%  glimpse()
df %>% select_if(~ is.numeric(.))


df %>%select(tip, total_bill) %>%
  arrange(desc(total_bill)) %>% 
  View()

colnames(df)

num_cols = df %>%  select(total_bill , tip)
df[, colnames(num_cols)]

df %>%  slice(40:50)

#columnn adini deyish
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
# korrelyasiyaya bax
cor(df$total_bill, df$tip)

# unique deyerleri cixa=rt
unique(df$tip)

df$tip %>%  unique() 
table(df$day)

glimpse(df)


df[, -(1:3)]

#selection by category
df %>% select_if(is.character) 
df %>%  select_if(~ is.numeric(.))

df %>%  select_if(~ !is.numeric(.))


# column sec ve filterle
df %>%  select(tip, total_bill) %>%
  filter(tip > mean(tip))
df %>%  filter(tip > 20)
filter((tip > 5000) | (price < 1000))


write.csv(df , "tips_r.csv")
















