#Load packages

install.packages("tidyverse")
install.packages("janitor")

library(pacman)
library(tidyverse)
library(datasets)
library(janitor)


#Loading Data
d_s<-read.csv("C:/Users/Admin/Desktop/My Projects/ds.salaries.csv")


glimpse(d_s)
head(d_s)

#Cleaning Data

clean_names(d_s)
ds2<-d_s[,-c(1,6:7)]
glimpse(ds2)
ds3<-subset(ds2,company_location %in% c("US"))
glimpse(ds3)
ds4<-subset(ds3,job_title %in% c("Machine Learning Engineer","Data Analyst", 
                                 "Data Scientist","Data Engineer"))
glimpse(ds4)

#Manipulation of data


library(dplyr)
install.packages("ggplot")
library(ggplot2)

ds_20<-subset(ds4,work_year %in% c('2020'))
glimpse(ds_20)
ds_21<-subset(ds4,work_year %in% c('2021'))
ds_22<-subset(ds4,work_year %in% c('2022')) 

#Visualizations

#Which job role has the highest salary range between 2020-2022?

ds20<-ds_20 %>% group_by(job_title) %>%
  summarise(Avg_salary = mean(salary_in_usd))
glimpse(ds20)
ds21<-ds_21 %>% group_by(job_title) %>% 
  summarise(Avg_salary =mean(salary_in_usd))
glimpse(ds21)
ds22<-ds_22 %>% group_by(job_title) %>% 
  summarise(Avg_salary = mean(salary_in_usd))
glimpse(ds22)

ds_sj<- rbind(
  mutate(ds22, year=2022),
  mutate(ds21, year=2021),
  mutate(ds20, year=2020))
glimpse(ds_sj)

ggplot(data = ds_sj,
       aes(x = job_title,
           y = Avg_salary,
           fill = factor(year))) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Salaries of Different Data Science Jobs between 2020-2022",
       x = "Job Roles", y = "Salary")

#Among the different job titles, the machine learning engineer consitently earn the
#highest, followed by Data scientist, Data engineer and finally data analyst.

#Does experience matter?-scatterplot

ds_Exp20<-ds_20 %>% group_by(experience_level) %>% 
  summarise(Avg_salary = mean(salary_in_usd))
glimpse(ds_Exp20)

ds_Exp21<-ds_21 %>% group_by(experience_level) %>% 
  summarise(Avg_salary = mean(salary_in_usd))
glimpse(ds_Exp21)

ds_Exp22<-ds_22 %>% group_by(experience_level) %>% 
  summarise(Avg_salary = mean(salary_in_usd))
glimpse(ds_Exp22)

  DsExp<- rbind(
    mutate(ds_Exp22, year=2022),
    mutate(ds_Exp21, year=2021),
    mutate(ds_Exp20, year=2020))
  glimpse(DsExp)

 ggplot(data = DsExp,aes(x = ,y = Avg_salary,
                         size = experience_level,
                         fill = factor(year)))+
          geom_point(stat = "identity",position = "dodge")+
          labs(title = "Influence of Experience on Salary",
               x= "Experience Level",
               y = "Average Salary")

  
 
 
  ds20Ex<-ds_20[,c(2,9)] 
glimpse(ds20Ex)    
ds21Ex<-ds_21 [,c(2,9)]
ds22Ex<-ds_22 [,c(2,9)]

Data_exp<-rbind(
  mutate(ds20Ex,year = 2020),
  mutate(ds21Ex,year = 2021),
  mutate(ds22Ex,year = 2022))
glimpse(Data_exp)

Exp_c<-cbind(Data_exp,DsExp)
head(Data_exp)
head(DsExp)
