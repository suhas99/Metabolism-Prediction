library(data.table)
library(tidyverse)

#reading Male datasets
male_type0<- data.table::fread(file ='/Users/Suhas/Documents/data/Male_db/Male_type0.csv' , 
                               header = TRUE, 
                               sep = ',', drop = c(2,3,4,9,14,15))
male_type_both<- data.table::fread(file ='/Users/Suhas/Documents/data/Male_db/both_male.csv' , 
                               header = TRUE, 
                               sep = ',', drop = c(2,3,4,9,14,15))
female_type0<- data.table::fread(file ='/Users/Suhas/Documents/data/Female_db/Female_type0.csv', 
                               header = TRUE, 
                               sep = ',', drop = c(2,3,4,9,14,15))
female_type_both<- data.table::fread(file ='/Users/Suhas/Documents/data/Female_db/both_female.csv' , 
                               header = TRUE, 
                               sep = ',', drop = c(2,3,4,9,14,15))

if Mt0$Heart.Rate=='60'

a<-Mt0 %>% distinct(Mt0$Test,Mt0$Age,Mt0$Gender,Mt0$Height,Mt0$Weight,Mt0$Breathing.Rate,Mt0$Met,Mt0$Heart.Rate,Mt0$Breathing.Amplitude,Mt0$BMI, .keep_all = TRUE)

male_test0_df <- metabolic_df %>% filter(Gender == "Male", Test == "0")


df[df==""]<-NA


test_df<- data.table::fread(file ='/Users/Suhas/Downloads/start-end.csv' , 
                               header = TRUE, 
                               sep = ',',select = c(2,3,4))
uni<-as.data.frame(unique(test_df$gene))
install.packages("dict")
library("dict")
for (i in uni) {

 

mapping<-list(
  '[]'
)






lapply(test_df,ksh(test_df$start,test_df$end,test_df$gene))
