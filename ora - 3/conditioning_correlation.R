#Rajk ökonometria, 2024-10-01, conditining, correlation
#Bárdits Anna 
#working directory
# clear memory
rm(list=ls())

setwd("~/Documents/Documents - betts’s MacBook Air/rajk/kurzusok/2.félév/ökonometria/ora - 3")
#load libraries
library(tidyverse)
#library(binsreg)


#read in data file
df <- read_csv("wms_da_textbook.csv")

#choose MExico
df <- df %>%
  filter(country=="Mexico" & wave==2013 & emp_firm>=100  & emp_firm<=5000)
#mit jelent? ezeknek az értékeknek a kizárása ok?

#emp_firm
size_plot<-ggplot(df, aes(emp_firm)) +
  geom_histogram(aes(y=after_stat(count / sum(count))), binwidth = 1, color="black", fill="white") + 
  labs(x="Vállaltméret (alkalmazottak száma)", y="Relatív gyakoriság") 

size_plot



#milyen bin legyen? értelmezzük? milyen ez az eloszlás?
size_plot_ln<-ggplot(df, aes(log(emp_firm))) +
  geom_histogram(aes(y=after_stat(count / sum(count))), binwidth = 0.2, color="black", fill="white") + 
  labs(x="Vállaltméret (alkalmazottak száma)", y="Relatív gyakoriság") 

size_plot_ln


#management

man_quality_plot<-ggplot(df, aes(management)) +
  geom_histogram(aes(y=after_stat(count / sum(count))), binwidth = 0.2, color="black", fill="white") + 
  labs(x="Vállalatvezetési minőség (átlagos pontszám)", y="Relatív gyakoriság") 


man_quality_plot

#feltételes összevetés kevés érték esetén
#3 értékűre vesszük a méretet
df$emp3bins <- ifelse(df$emp_firm<200, 1, 
                      ifelse(df$emp_firm>=200 & df$emp_firm<1000, 2,
                             ifelse(df$emp_firm>=1000, 3,100)
                      )
)


# Create factor
df$emp3bins <- as.factor(df$emp3bins)

# 3 rekeszre osztjuk, lean menedzsment, teljesítménykövetés
df1 <- df %>% 
  dplyr::select(emp3bins,lean1) %>% 
  group_by (emp3bins,lean1) %>% 
  dplyr::summarise(Count = n()) %>% 
  mutate(Percent= round(Count / sum(Count),digits = 5)) %>% ungroup()

# Stacked bar
lean_man<-ggplot(data=df1, aes(x=emp3bins, y=Percent, fill = factor(lean1, levels = rev(unique(lean1))))) +
  geom_bar(stat = "identity", position = "fill",width = 0.6,  color = "white",  size = 0.5, alpha = 0.8) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 1), breaks = seq(0, 1, by = 0.2), labels = scales::percent_format()) +
  scale_x_discrete(labels=c("1" = "Small", "2" = "Medium", "3" = "Large")) +
  #scale_fill_manual(values = viridis(5, begin=0, end=0.9), name = NULL) +
  labs(x = "Firm size (employment), 3 bins", y = "Percent") +
  theme(legend.position = "bottom") + 
  theme(legend.title=element_blank())


lean_man
#scatter - many values
g5a<-ggplot(data = df, aes(x = emp_firm, y = management)) +
  geom_point(color = "black", size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=1000)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (employment)",y = "Management score")

g5a

df$lnemp = log(df$emp_firm)

g5b<-ggplot(data = df, aes(x = lnemp, y = management)) +
  geom_point(color = "black", size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  scale_x_continuous(expand = c(0.01,0.01),limits=c(4, 9), breaks=seq(4, 9, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(1, 5), breaks = seq(1, 5,1)) +
  labs(x = "Firm size (ln(employment))",y = "Management score") 
g5b

#binscatter

# Recode employee bins
df$emp3bins <- ifelse(df$emp3bins == 1 , 150, 
                      ifelse(df$emp3bins == 2, 600,
                             ifelse(df$emp3bins == 3, 3000, NA)))
# Summary

# Generate variables by mean
df1<-df %>% group_by(emp3bins) %>%
  dplyr::summarize(management_emp3bins=mean(management))




#binscatter
g4a<-ggplot(data = df1, aes(x = emp3bins, y = management_emp3bins)) +
  geom_point(size = 2, color = "black", fill= "black", shape = 21, alpha = 0.8, na.rm=T) +
  #geom_text(aes(label = round(management_emp3bins, 1)), hjust = 0.5, vjust = -1, color = "black", size = 3) +
  labs(x = "Firm size (employment), 3 bins", y = "Management score") + 
  scale_y_continuous(expand = c(0.01,0.01),limits = c(2.4, 3.4), breaks = seq(2.4, 3.4, by=0.2)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 3000), breaks = seq(0,3000, by=500))

g4a

binsreg(df$management, df$emp_firm, nbins = 10)

#függőség kovariancia korreláció, vállaltméret vállalatvezetési pontszám
cov(df$management, df$emp_firm)
cor(df$management, df$emp_firm)

#további feltételes összevetés. iparági kategóriák szerint
# by industry
df$industry_broad[df$sic<=21] <- 'food_drinks_tobacco'
df$industry_broad[df$sic>=22 & df$sic<=23 | df$sic==31  ] <- 'textile_apparel_leather_etc'
df$industry_broad[df$sic>=24& df$sic<=27] <- 'wood_furniture_paper'
df$industry_broad[df$sic>=28 & df$sic<=30] <- 'chemicals_etc'
df$industry_broad[df$sic>=32 & df$sic<35] <- 'materials_metals'
df$industry_broad[df$sic>=35 & df$sic<37] <- 'electronics'
df$industry_broad[df$sic==37 ] <- 'auto'
df$industry_broad[df$sic>=38]             <- 'other'

# Correlation
df %>%
  group_by(industry_broad) %>%
  dplyr::summarize(COR=cor(management, emp_firm))


