
rm(list=ls())

library(hhi)
library(readr)
library(tidyverse)
library(scales)
library(zoo)

combined_deaths <- read_csv("data/Cases/combined_deaths.csv", guess_max = 10000) %>% select(-c(6:25))


combined_deaths <- combined_deaths[!duplicated(combined_deaths),]
combined_deaths <- combined_deaths %>% filter(region!="sum_deaths") 

# Fixing Canton issue and France issue (around July)

combined_deaths$deaths[combined_deaths$country=="Switzerland" & combined_deaths$date<="2020-02-25"]<-0
combined_deaths$deaths[combined_deaths$country=="Switzerland" & combined_deaths$deaths==0 & combined_deaths$date>="2020-02-25"]<-NA
combined_deaths$deaths[combined_deaths$country=="France" & combined_deaths$date>="2020-06-26" & combined_deaths$date<="2020-07-01"]<-NA
#combined_deaths$deaths[combined_deaths$country=="France" & combined_deaths$date=="2020-03-26"]<-NA
#combined_deaths$deaths[combined_deaths$country=="France" & combined_deaths$date=="2020-03-08"]<-NA


combined_deaths <- combined_deaths %>% 
  group_by(country, region) %>% 
  mutate(deaths = ceiling(na.approx(deaths, maxgap = Inf, rule = 2,na.rm=FALSE)))


combined_deaths<-combined_deaths %>% group_by(region) %>% 
  mutate(
    new_deaths = deaths - lag(deaths),
    new_deaths_national = deaths_national - lag(deaths_national)
  )%>%
  filter(region!="National")%>%
  drop_na(region)

#Fix case reporting problems for cantons (2 times interpolation required) 

combined_deaths$deaths[combined_deaths$new_deaths<0]<-NA

a <- combined_deaths %>% 
  group_by(country, region) %>% 
  mutate(deaths_2 = ceiling(na.approx(deaths, maxgap = Inf, rule = 2,na.rm=FALSE)))%>%
  select(-deaths,-new_deaths,-new_deaths_national)

#Rerun with new interpolated new deaths
combined_deaths<-a %>% group_by(region) %>% 
  mutate(
    new_deaths = deaths_2 - lag(deaths_2),
    new_deaths_national = deaths_national - lag(deaths_national)
  )%>%
  filter(region!="National")%>%
  drop_na(region)
combined_deaths$deaths_2[combined_deaths$new_deaths<0]<-NA
a <- combined_deaths %>% 
  group_by(country, region) %>% 
  mutate(deaths = ceiling(na.approx(deaths_2, maxgap = Inf, rule = 2,na.rm=FALSE)))%>%
  select(-deaths_2,-new_deaths,-new_deaths_national)


#re-calculate national values based on interpolated regional values
deaths_nat <- a %>% dplyr::group_by(date, country) %>% dplyr::summarize(deaths_national =sum(deaths,na.rm = T))
a<-a%>%select(-deaths_national) %>% left_join(.,deaths_nat, by=c("country","date"))

combined_deaths<- a %>% dplyr:: group_by(region) %>% 
  dplyr::mutate(
    new_deaths = deaths - lag(deaths),
    new_deaths_national = deaths_national - lag(deaths_national))




sum_pop<- combined_deaths %>% group_by(country,date)%>% summarise(sum_pop=sum(eurostat_total_population_2019, na.rm=T))%>% ungroup()

# Removing negative values for new deaths (might be due to re-estimation)

deaths<-left_join(combined_deaths,sum_pop,by=c("date","country"))

#deaths$deaths_pop_sub = sub national deaths / sub national population
#deaths$deaths_pop_nat = national deaths / national population

deaths$deaths_pop_sub<-deaths$deaths/deaths$eurostat_total_population_2019
deaths$deaths_pop_nat<-deaths$deaths_national/deaths$sum_pop


#Ratio of sub national to national deaths by population 
# deaths$ratio_pop_cum = subnational cumulative deaths per population - national cumulative deaths per population
deaths$ratio_pop_cum <- deaths$deaths_pop_sub - deaths$deaths_pop_nat


# Ratio to overall deaths

deaths$ratio_cum <- (deaths$deaths/deaths$deaths_national)
deaths$ratio_new <- (deaths$new_deaths/deaths$new_deaths_national)


# Quadratic Ratio
deaths$ratio_cum2 <- (deaths$deaths/deaths$deaths_national)^2
deaths$ratio_new2 <- (deaths$new_deaths/deaths$new_deaths_national)^2



library(zoo)
deaths <- deaths %>%
  unique() %>%
  group_by(country, region) %>%
  mutate(rolling_average = rollapply(deaths, 7, mean, fill=NA)) %>%
  mutate(past_average = rollapply(deaths, 7, mean, align="right", fill=NA)) %>%
  mutate(rolling_average_new_deaths = rollapply(new_deaths, 7, mean, fill=NA)) %>%
  mutate(past_average_new_deaths = rollapply(new_deaths, 7, mean, align="right", fill=NA)) %>%
  mutate(past_average_new_deaths_national = rollapply(new_deaths_national, 7, mean, align="right", fill=NA)) 

deaths$measure_H1_H2_deaths<- (deaths$past_average_new_deaths_national/deaths$sum_pop)*100
deaths$measure_H3_deaths_a<- ((deaths$past_average_new_deaths/deaths$eurostat_total_population_2019)*100) / (deaths$measure_H1_H2_deaths)
deaths$measure_H3_deaths_b<- (((deaths$past_average_new_deaths/deaths$eurostat_total_population_2019)*100) - (deaths$measure_H1_H2_deaths))/ (deaths$measure_H1_H2_deaths)


deaths<-deaths[!is.na(deaths$region),]

write_csv(deaths,"data/Cases/deaths.csv")


hhi<- deaths %>% group_by(country,date)%>% summarise(hhi_cumulative_deaths=sum(ratio_cum2,na.rm=T),
                                                    hhi_new_deaths=sum(ratio_new2,na.rm=T)
)
write.csv(hhi,"Measures/hhi_deaths.csv")

# Merge case and deaths data



deaths <- read_csv("data/Cases/deaths.csv", guess_max = 10000) %>% select(1:2,6:9,11:12,21:25)

cases <- read_csv("data/Cases/cases.csv", guess_max = 10000) %>% select(1:5,8:12,25:30)

final_cases<-left_join(cases,deaths,by=c("region","date"))

write.csv(final_cases,"data/Cases/final_cases.csv")

names(final_cases)
