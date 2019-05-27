library(data.table)
library(tidyverse)
library(devtools)
library(ggbiplot)
library(ggplot2)


install.packages(pytorch)


path = "/Users/Suhas/Downloads/metabolism"
setwd(path)
#read the data and dropping columns(Features)
metabolic_df <- fread("metabolic.csv", sep = ",", header = TRUE, drop = c(2,3,4,9,14,15), stringsAsFactors = TRUE)

#redudant data treatment
metabolic_df <- unique(metabolic_df)

#making buckets heart rate
heart_rate_names <- c("very_low", "low", "normal", "high", "very_high")
metabolic_df$Hrate <- cut(metabolic_df$`Heart Rate`,
                          breaks = seq(from = 0, to = 160, by = 32), 
                          include.lowest = TRUE,
                          labels = heart_rate_names)

#making dummies (factors) for Type
metabolic_df$Type<-factor(metabolic_df$Test,levels = c("0","1","2")) 
c<-data.frame(metabolic_df)
c$Test<-NULL

#Treating Heart rate missing values
c$Heart.Rate[c$Heart.Rate=="0"]<-60

#creating age buckets
age_names<- c("Age 0 to 20","Age 20 to 40","Age 40 to 60","Age 60 to 80","Age 80 to 100")
c$Age_group<- cut(c$Age,
                 breaks = seq(from=0, to=100,by=20 ),
                 include.lowest = TRUE,
                 labels = age_names)

#calculating co-relation matrix
a<-male_test0_df[, c(1,3:9)]
cor_matrix<-cor(a)

barplot(c$Heart.Rate,names.arg=c$Met,xlab="Heartt",ylab="met",col="blue"
       )
#Breathing rate (meth inversly proportional)

ggplot(c,aes(x=c$Breathing.Rate,y=c$Met)) + geom_point()
#Heart rate(meth increases with breathing rate)
ggplot(c,aes(x=c$Heart.Rate,y=c$Met))+ geom_point()
#(Meth high between 20 to 40)
ggplot(c,aes(x=c$Age,y=c$Met))+ geom_point()
#(Male have higher meth (categorical var))
ggplot(c,aes(x=c$Gender,y=c$Met))+ geom_point()
#(linearly propoprational to meth)
ggplot(c,aes(x=c$Height,y=c$Met))+ geom_point()
#(increase till90 and remanis const)
ggplot(c,aes(x=c$Weight,y=c$Met))+ geom_point()
#(linearly increasing can make a constant variables)
ggplot(c,aes(x=c$Breathing.Amplitude,y=c$Met))+ geom_point()
#
ggplot(c,aes(x=c$BMI,y=c$Met))+ geom_point()
#hrate(linearly increasing)
ggplot(c,aes(x=c$Hrate,y=c$Met))+ geom_point()
#Type 0>2>1
ggplot(c,aes(x=c$Type,y=c$Met))+ geom_point()

ggplot(c,aes(x=c$Age_group,y=c$Met))+ geom_point()



#

#calculating pca
metabolic_df.pca<-prcomp(c[,c(1,3:9)], center = TRUE,scale. = TRUE)
summary(metabolic_df.pca)
ggbiplot(metabolic_df.pca)

#


 
#model fit with lm
model1<-lm(Met~Breathing.Rate+Heart.Rate+Breathing.Amplitude,c)
model2<-lm.fit(Met~Breathing.Rate+Heart.Rate+Breathing.Amplitude,c)
summary(model1)
plot(model1)
 

#Adding filters
male_test0_df <- c%>%filter(Gender == "Male", Type == "0")
male_testboth_df<-c%>%filter(Gender == "Male", Type == "2" | Type=="1")

model3<-lm(Met~Breathing.Rate+Heart.Rate+Breathing.Amplitude,male_test0_df)
plot(male_test0_df$Met ~ male_test0_df$Heart.Rate)
lines(lowess(male_test0_df$Met ~ male_test0_df$Heart.Rate))
annova(model3)

plot(model1)

female_type0_df<-c %>% filter(Gender=="Female",Type=="0")
female_type1_df<- c%>% filter(Gender == "Female", Type == "1" )
female_type1_df<- c%>% filter(Gender == "Female", Type == "2" )



male_type0_df<-c %>% filter(Gender=="Male",Type=="0")
male_type1_df<- c%>% filter(Gender == "Male", Type == "1" )
male_type1_df<- c%>% filter(Gender == "Male", Type == "2" )

#0.23 error rate

female_type0_df_cr<-

fo<-c$Breathing.Rate+-c$Heart.Rate+-c$Breathing.Amplitude
fo1<-female_type0_df$Breathing.Rate+female_type0_df$Breathing.Amplitude+female_type0_df$Heart.Rate
model5<-lm(female_type0_df$Met ~ poly(fo1,1, raw = TRUE))
summary(model5)


fo2<-female_type1_df$Breathing.Rate+female_type1_df$Breathing.Amplitude+female_type1_df$Heart.Rate
model5<-lm(female_type1_df$Met ~ poly(fo2,1, raw = TRUE))
summary(model5)

abline(x=4,y=5)



fo2<-female_type2_df$Breathing.Rate+female_type2_df$Breathing.Amplitude+female_type2_df$Heart.Rate
model7<-lm(female_type2_df$Met ~ poly(fo2,1, raw = TRUE))
summary(model7)


model4<-lm(c$Met ~ poly(fo,2, raw = TRUE))
summary(model4)

mo<-male_type1_df$Breathing.Rate+male_type1_df$Breathing.Amplitude+male_type1_df$Heart.Rate
model6<-lm(male_type1_df$Met ~ poly(mo,1, raw = TRUE))
summary(model6)
#----------------------------------
  
metabolic_df <- fread("metabolic.csv", sep = ",", header = TRUE, drop = c(2,3,4,9,14,15), stringsAsFactors = TRUE)




