#Rajk öknometria, 2024-09-17
#Bárdits Anna


#set working directory
#---------------------
setwd("~/Documents/Documents - betts’s MacBook Air/rajk/kurzusok/2.félév/ökonometria/ora -1")

#install and load packages
#install.packages("tidyverse")
library(tidyverse)

#Explore data set
#-------------------

#read in data - need to know the type
read.csv("hotels-vienna.csv") 

#put the data in a R data table
hotels_df<-read.csv("hotels-vienna.csv")

#nézzük meg
#keresztmetszeti? panel? idősoros?
distinct_ids<-hotels_df %>% select(hotel_id)  %>% distinct
sum(duplicated(hotels_df$hotel_id))


#hány mgefigyelés van
length(hotels_df$hotel_id)

#kb mi van benne?
summary(hotels_df %>% select(country, rating_count, price, stars, accommodation_type, distance, rating))

head(hotels_df$rating)

#missingek
hotels_df<-hotels_df %>% mutate(acc_type=as.factor(accommodation_type))
summary(hotels_df)
#value labelek
hotels_df<-hotels_df %>% arrange(rating)

levels(hotels_df$acc_type)

