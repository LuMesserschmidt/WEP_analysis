
rm(list=ls())

library(hhi)
library(readr)
library(tidyverse)
library(scales)

combined_cases <- read_csv("data/merged_v5.csv") 
combined_cases <- combined_cases[!duplicated(combined_cases),]
combined_cases <- combined_cases %>% filter(region!="sum_cases") 
combined_cases<-combined_cases %>% group_by(region) %>% 
  mutate(
    new_cases = cases - lag(cases)
  )

sum_cum<- combined_cases %>% group_by(country,date)%>% summarise(sumd=sum(cases, na.rm=T))%>% ungroup()
sum_new<- combined_cases %>% group_by(country,date)%>% summarise(sumn=sum(new_cases, na.rm=T))%>% ungroup()

# Removing negative values for new cases (might be due to re-estimation)
sum_new<-sum_new[!sum_new$sumn<0,]
combined_cases<-combined_cases[!combined_cases$new_cases<0,]

cases1<-left_join(combined_cases,sum_cum,by=c("date","country"))
cases<-left_join(cases1,sum_new,by=c("date","country"))

# Ratio 

cases$ratio_cum <- (cases$cases/cases$sumd)
cases$ratio_new <- (cases$new_cases/cases$sumn)

cases<-cases[!is.na(cases$ratio_cum),]
cases<-cases[!is.na(cases$ratio_new),]

# Quadratic Ratio
cases$ratio_cum2 <- (cases$cases/cases$sumd)^2
cases$ratio_new2 <- (cases$new_cases/cases$sumn)^2

cases<-cases[!is.na(cases$ratio_cum2),]
cases<-cases[!is.na(cases$ratio_new2),]

test<-cases%>% select (1,2,3,4,140:147)

write_csv(cases,"data/Cases/cases.csv")

hhi<- cases %>% group_by(country,date)%>% summarise(hhi_cumulative=sum(ratio_cum2,na.rm=T),
                                                    hhi_new=sum(ratio_new2,na.rm=T)
                                                    )
write.csv(hhi,"Measures/hhi.csv")
# Vizualization

plot_hhi_cum<- hhi %>%ggplot( aes(x=date, y=hhi_cumulative, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Relative Herfindahl Concentration (cumulative cases)") +
  xlab("")

plot_hhi_new<- hhi %>%ggplot( aes(x=date, y=hhi_new, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylim(0,1)+
  ylab("Relative Herfindahl Concentration (new cases)") +
  xlab("")


ggsave(filename="Results/plot_hhi_cum.jpeg",
       plot=plot_hhi_cum,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



ggsave(filename="Results/plot_hhi_new.jpeg",
       plot=plot_hhi_new,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



#Old
