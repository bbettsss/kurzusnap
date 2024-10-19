#Rajk ökonometria, 2024-09-24, EDA
#Bárdits Anna


#set working directory
#---------------------
setwd("~/Documents/Documents - betts’s MacBook Air/rajk/kurzusok/2.félév/ökonometria/ora - 2")

# load packages (install first, if needed)
library(tidyverse)
library(scales)
library(xtable)
library(modelsummary)


#put the data in a R data table
hotels_df<-read.csv("hotels-vienna.csv")

#goal: find a good deal among hotels - cheap relative to quality

hotels_df<-hotels_df %>% mutate(acc_type=as.factor(accommodation_type))
table(hotels_df$acc_type)

# just hotels
hotels_cut <- hotels_df %>% filter(acc_type=="Hotel")

#igy is lehetne
#hotels_cut <- hotels_df %>% filter(as.numeric(acc_type)==6)
table(hotels_cut$stars)

stars_freq_Vienna <- ggplot(hotels_cut, aes(x=stars)) + 
                    geom_histogram()
stars_freq_Vienna

stars_freq_Vienna <- ggplot(hotels_cut, aes(stars)) +
                    geom_bar()
stars_freq_Vienna

#kicicomázzuk, 3.2-től 3.4-ig érdemes átnézni
stars_freq_Vienna <- ggplot(hotels_cut, aes(stars)) +
                    geom_bar(color="black", fill="white") + 
                    labs(x="Csillagos értékelés (csillagok száma)", y="Gyakoriság") + 
                    geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
                    scale_x_continuous(breaks = seq(1, 5, by = 0.5)) + 
                    ylim(0, 150)
stars_freq_Vienna

#mi a módusz?

#save image
ggsave("graphs/stars_abs_freqs.png", stars_freq_Vienna )

#relativ gyakorisag
prop.table(table(hotels_cut$stars))

#table(hotels_cut$stars)/(length(hotels_cut$stars))

#pipeline tipusu
hotels_cut %>%
  group_by(stars) %>%
  summarise(count = n()) %>%
  mutate(relative_freq = count / sum(count))

#
#
stars_rel_freq_Vienna <- ggplot(hotels_cut, aes(stars)) +
  geom_bar(aes(y=..prop..)) + 
  geom_text(aes(y = ..prop.., label = scales::percent(..prop..), group = 1), stat = "count", vjust = -0.5) + 
  ylim(0,0.5)
#mit lehetne még ezen szépíteni?

stars_rel_freq_Vienna

#tovább szűkítjük az adatot - ez mit jelent?
hotels_cut <- hotels_df %>% filter(acc_type=="Hotel") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars))
  
#
ggplot(hotels_cut, aes(price)) + 
  geom_histogram(binwidth = 1)

#vajon a kiugró érték hiba??
hotels_cut<-hotels_cut %>% arrange(desc(price))

#dobjuk ki az extrém drágát
hotels_cut2 <- hotels_cut %>% filter(price<1000)

ggplot(hotels_cut2, aes(price)) + 
  geom_histogram(binwidth = 1, color="black", fill="white")

#próbáljunk különböző bin szélességeket
ggplot(hotels_cut2, aes(price)) + 
  geom_histogram(binwidth = 10, color="black", fill="white")

#milyen ez az eloszlás? ferde? van hosszú elnyúlása? - jobbra elnyújtott

ggplot(hotels_cut2, aes(distance)) + 
  geom_histogram(binwidth = 10, color="black", fill="white")

# és a távolságnak milyen az eloszlása 

#lehet kernel.sűrűség, főleg ha sokféle érték

ggplot(hotels_cut2, aes(price)) + 
  geom_density()

#leíró statisztikák
summary(hotels_cut$price)

summary(hotels_cut2$price)
var(hotels_cut$price)
sd(hotels_cut$price)


  
histprice
  
  filter(price<=1000)




