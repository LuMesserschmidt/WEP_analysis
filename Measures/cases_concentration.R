
rm(list=ls())

library(hhi)
library(readr)
library(tidyverse)
library(scales)
library(zoo)

combined_cases <- read_csv("data/Cases/combined_cases.csv", guess_max = 10000) 


combined_cases <- combined_cases[!duplicated(combined_cases),]
combined_cases <- combined_cases %>% filter(region!="sum_cases") 

# Fixing Canton issue and France issue (around July)

combined_cases$cases[combined_cases$country=="Switzerland" & combined_cases$date<="2020-02-25"]<-0
combined_cases$cases[combined_cases$country=="Switzerland" & combined_cases$cases==0 & combined_cases$date>="2020-02-25"]<-NA
combined_cases$cases[combined_cases$country=="France" & combined_cases$date>="2020-06-26" & combined_cases$date<="2020-07-01"]<-NA
#combined_cases$cases[combined_cases$country=="France" & combined_cases$date=="2020-03-26"]<-NA
#combined_cases$cases[combined_cases$country=="France" & combined_cases$date=="2020-03-08"]<-NA

 
combined_cases <- combined_cases %>% 
  group_by(country, region) %>% 
  mutate(cases = ceiling(na.approx(cases, maxgap = Inf, rule = 2,na.rm=FALSE)))


combined_cases<-combined_cases %>% group_by(region) %>% 
  mutate(
    new_cases = cases - lag(cases),
    new_cases_national = cases_national - lag(cases_national)
  )%>%
  filter(region!="National")%>%
  drop_na(region)

#Fix case reporting problems for cantons (2 times interpolation required) 

combined_cases$cases[combined_cases$new_cases<0]<-NA

a <- combined_cases %>% 
  group_by(country, region) %>% 
  mutate(cases_2 = ceiling(na.approx(cases, maxgap = Inf, rule = 2,na.rm=FALSE)))%>%
  select(-cases,-new_cases,-new_cases_national)

#Rerun with new interpolated new cases
combined_cases<-a %>% group_by(region) %>% 
  mutate(
    new_cases = cases_2 - lag(cases_2),
    new_cases_national = cases_national - lag(cases_national)
  )%>%
  filter(region!="National")%>%
  drop_na(region)
combined_cases$cases_2[combined_cases$new_cases<0]<-NA
a <- combined_cases %>% 
  group_by(country, region) %>% 
  mutate(cases = ceiling(na.approx(cases_2, maxgap = Inf, rule = 2,na.rm=FALSE)))%>%
  select(-cases_2,-new_cases,-new_cases_national)


#re-calculate national values based on interpolated regional values
cases_nat <- a %>% dplyr::group_by(date, country) %>% dplyr::summarize(cases_national =sum(cases,na.rm = T))
a<-a%>%select(-cases_national) %>% left_join(.,cases_nat, by=c("country","date"))

combined_cases<- a %>% dplyr:: group_by(region) %>% 
  dplyr::mutate(
    new_cases = cases - lag(cases),
    new_cases_national = cases_national - lag(cases_national))




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



library(zoo)
cases <- cases %>%
  unique() %>%
  group_by(country, region) %>%
  mutate(rolling_average = rollapply(cases, 7, mean, fill=NA)) %>%
  mutate(past_average = rollapply(cases, 7, mean, align="right", fill=NA)) %>%
  mutate(rolling_average_new_cases = rollapply(new_cases, 7, mean, fill=NA)) %>%
  mutate(past_average_new_cases = rollapply(new_cases, 7, mean, align="right", fill=NA)) %>%
  mutate(past_average_new_cases_national = rollapply(new_cases_national, 7, mean, align="right", fill=NA)) 

cases$measure_H1_H2_cases<- (cases$past_average_new_cases_national/cases$sum_pop)*100
cases$measure_H3_cases<- ((cases$past_average_new_cases/cases$eurostat_total_population_2019)*100) / (cases$measure_H1_H2_cases)


cases<-cases[!is.na(cases$region),]
library(data.table)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data$dateRep <- format(as.Date(data$dateRep, format="%d/%m/%Y"),"%Y-%m-%d")%>% as.character()
data$dateRep <- as.Date(data$dateRep)
case<- data %>% filter(`countriesAndTerritories` %in% c("Germany", "France", "Italy", "Switzerland"))%>%
  select(1,5,7)%>%
  rename(country = `countriesAndTerritories`, date = dateRep, new_cases_national_ECDC = cases)%>%
  arrange(date,country)

cased<- case%>%
  group_by(country)%>%
  mutate(cases_national_ECDC=cumsum(new_cases_national_ECDC))

case <- cased %>%
  unique() %>%
  group_by(country) %>%
  mutate(past_average_new_cases_national_ECDC = rollapply(new_cases_national_ECDC, 7, mean, align="right", fill=NA)) 

case<-left_join(cases,case,by=c("country","date")) %>% mutate(cases_national_ECDC)

case$measure_H1_H2_cases_ECDC<- (case$past_average_new_cases_national_ECDC/case$sum_pop)*100


write_csv(case,"data/Cases/cases.csv")


hhi<- cases %>% group_by(country,date)%>% summarise(hhi_cumulative=sum(ratio_cum2,na.rm=T),
                                                    hhi_new=sum(ratio_new2,na.rm=T)
                                                    )
write.csv(hhi,"Measures/hhi.csv")

