
rm(list=ls())

library(hhi)
library(readr)
library(tidyverse)
library(scales)

names(combined_cases)
combined_cases <- read_csv("data/merged_final.csv") %>% select("date","region","cases","cases_national","country","eurostat_total_population_2019","code")
combined_cases <- combined_cases[!duplicated(combined_cases),]
combined_cases <- combined_cases %>% filter(region!="sum_cases") 
combined_cases<-combined_cases %>% group_by(region) %>% 
  mutate(
    new_cases = cases - lag(cases),
    new_cases_national = cases_national - lag(cases_national)
  )%>%
  filter(region!="National")%>%
  drop_na(region)


sum_pop<- combined_cases %>% group_by(country,date)%>% summarise(sum_pop=sum(eurostat_total_population_2019, na.rm=T))%>% ungroup()

# Removing negative values for new cases (might be due to re-estimation)

cases<-left_join(combined_cases,sum_pop,by=c("date","country"))

#cases$cases_pop_sub = sub national cases / sub national population
#cases$cases_pop_nat = national cases / national population

cases$cases_pop_sub<-cases$cases/cases$eurostat_total_population_2019
cases$cases_pop_nat<-cases$cases_national/cases$sum_pop


#Ratio of sub national to national cases by population 
# cases$ratio_pop_cum = subnational cumulative cases per population - national cumulative cases per population
cases$ratio_pop_cum <- cases$cases_pop_sub - cases$cases_pop_nat


# Ratio to overall cases

cases$ratio_cum <- (cases$cases/cases$cases_national)
cases$ratio_new <- (cases$new_cases/cases$new_cases_national)


# Quadratic Ratio
cases$ratio_cum2 <- (cases$cases/cases$cases_national)^2
cases$ratio_new2 <- (cases$new_cases/cases$new_cases_national)^2



cases<-cases[!is.na(cases$region),]

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




