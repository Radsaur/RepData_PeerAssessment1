setwd('D:/DOCS/Sauran.S/R/Coursera/Literate_programming')

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


#1. Code for reading in the dataset and/or processing the data
df<- read.csv('activity.csv')
df %>% summary()


#2. Histogram of the total number of steps taken each day
tot_steps<-df %>% group_by(date) %>% summarize(steps=sum(steps,na.rm = T))
tot_steps$steps %>% hist(main='Total number of steps',xlab='steps per day' ,ylim=c(0,28))


#3.Mean and median number of steps taken each day
df %>% group_by(date) %>% summarize(steps=mean(steps,na.rm = T))
df %>% group_by(date) %>% summarize(steps=median(steps,na.rm = T))


#4. Time series plot of the average number of steps taken
tot_steps$date<-tot_steps$date %>% ymd()
ggplot(tot_steps, aes(x=date, y=steps)) + geom_line()

df %>% group_by(interval) %>% 
    summarize(steps=mean(steps,na.rm = T)) %>% ggplot(aes(x=interval, y=steps)) + 
    geom_line()+ labs(title = 'average number of per inerval')

#5. The 5-minute interval that, on average, contains the maximum number of steps
intrv_steps<-df %>% group_by(interval) %>% summarize(steps=mean(steps,na.rm = T)) 
intrv_steps[intrv_steps$steps==intrv_steps$steps %>% max,1]

#6. Code to describe and show a strategy for imputing missing data
library(mice)

imputed_Data <- mice(df, maxit = 50, method = 'pmm', seed = 500,printFlag = F)
imputed_Data$imp$steps
completeData <- complete(imputed_Data,2)

#7. Histogram of the total number of steps taken each day after 
#missing values are imputed
group_steps<-completeData %>%group_by(date) %>% summarise(steps=sum(steps))  
group_steps$steps %>% hist(main='Total number of steps after imputting NAs',xlab='steps per day')


#8. Panel plot comparing the average number of steps taken per 5-minute 
#interval across weekdays and weekends
completeData$date<-completeData$date %>% ymd()
completeData$wday<-ifelse(completeData$date %>% wday()>5,'week_end','week_day')

av_steps_weekdays<-completeData %>% group_by(interval,wday) %>% summarise(steps=mean(steps))

ggplot(av_steps_weekdays, aes(interval, steps,color=wday))+geom_line()+
    labs(title = 'Panel plot weekend vs working days')
