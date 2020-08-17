# R-Code: WEP Paper
# Descriptive & Bivariate Stats

# Roadmap: 

# Descriptive
# - Cases Plot
# - Map of Cases
# - Centrality Index
# - Heterogeneity Index
# - Heterogeneity Index
# - PAX
# - Table of Policies
# - Summary Stats


# Bivariate----
# - Cases vs Federal/Unitary Scores----
# - Correlation Matrix (by date): Centrality, PAX, Heterogeneity, Adoption, Authority Index, Federalism ----

# Roadmap: 

# Prepare data

rm(list=ls())

library(readr)
library(tidyverse)
library(scales)
library(sf)

df_cases <- read_csv("data/Cases/cases.csv", guess_max = 10000)
df_main <- read_csv("data/merged_final.csv", guess_max = 10000)
df_hhi <- read_csv("Measures/hhi.csv", guess_max = 10000)

# Descriptive----
# - Cases Plot----

gg1.1<- ggplot(df_cases %>% filter(region == "National") %>% select(date, country, region, cases, cases_national) %>% unique(), aes(x=date, y=cases, color=country)) + geom_line() + labs(x = "Date", y = "Cases", color="Country") + ggtitle("Cumulative COVID-19 Cases per Country")
gg1.2<- ggplot(df_main %>% filter(region == "National") %>% select(date, country, region, cases, past_average_new_cases_national) %>% unique(), aes(x=date, y=past_average_new_cases_national, color=country)) + geom_line() + labs(x = "Date", y = "New Cases (7 days avg)", color="Country") + ggtitle("Past 7 Days Average of New COVID-19 Cases per Country")

#Combine 
gg3<- ggplot(df_main %>% filter(region == "National") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40))+scale_color_manual(values=c('red','blue',"green","orange"))
gg1.3 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases"))

# For each country individual

gg3<- ggplot(df_main %>% filter(region == "National", country=="Switzerland") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.4 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Switzerland")

gg3<- ggplot(df_main %>% filter(region == "National", country=="France") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.5 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases France")

gg3<- ggplot(df_main %>% filter(region == "National", country=="Italy") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.6 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Italy")

gg3<- ggplot(df_main %>% filter(region == "National", country=="Germany") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.7 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Germany")


# - Concentration ----

# Vizualization

gg2.1<- df_hhi %>%ggplot( aes(x=date, y=hhi_cumulative, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Relative Herfindahl Concentration (cumulative cases)") +
  xlab("")

gg2.2<- df_hhi %>%ggplot(aes(x=date, y=hhi_new, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylim(0,1)+
  ylab("Relative Herfindahl Concentration (new cases)") +
  xlab("")


# - Map of Cases----

library(sf)
library(tidyverse)

gerpath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_1.shp/NUTS_RG_01M_2021_4326_LEVL_1.shp"
italypath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_2.shp/NUTS_RG_01M_2021_4326_LEVL_2.shp"
schweizpath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_3.shp/NUTS_RG_01M_2021_4326_LEVL_3.shp"

shape <- st_read(dsn = gerpath)
ch_shape <- st_read(dsn = schweizpath)
it_shape <- st_read(dsn = italypath)

nuts1 <- shape$NUTS_ID %>% .[str_detect(.,"DE|FR")] %>% str_remove_all("FRY")
nuts2 <- it_shape$NUTS_ID %>% .[str_detect(.,"IT")]
nuts3 <- ch_shape$NUTS_ID %>% .[str_detect(.,"CH")]

corona <- df_cases

df <- corona %>%
  select("date","cases","new_cases","ratio_cum","ratio_new","code", "cases_pop_sub") %>% 
  mutate(country = str_sub(code,1,2))%>%
  filter(date=="2020-03-15" |
           date=="2020-04-15" |
           date=="2020-05-15" |
           date=="2020-06-01"       
  )%>%
  filter(!code%in% c("FRY1","FRY2","FRY3","FRY4","FRY5"))


merged_shape <- 
  left_join(bind_rows(shape,ch_shape,it_shape),
            df,
            by = c("NUTS_ID" = "code"))

library(viridis)

gg3.1 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(ratio_cum))) + 
  geom_sf(aes(alpha = ratio_cum,
              fill = country),size = 0.1) + 
  scale_fill_manual(values = c("green","red","blue","orange"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Cases by Region")

gg3.1
gg3.2 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(cases_pop_sub))) + 
  geom_sf(aes(alpha = cases_pop_sub,
              fill = country),size = 0.1) + 
  scale_fill_manual(values = c("green","red","blue","orange"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Cases by Region (relative by sub-national population)")


# - Centrality Index----
# - Heterogeneity Index----
# - Heterogeneity Index----
# - PAX----
# - Table of Policies----

df_main %>%
  filter(!is.na(type)) %>%
  filter(region != "National") %>%
  unique() %>%
  group_by(type, region) %>%
  tally() %>%
  pivot_wider(names_from = region, values_from = n, values_fill=0)

# - Summary Stats----

# Bivariate----
# - Cases vs Federal/Unitary Scores----
# - Correlation Matrix (by date): Centrality, PAX, Heterogeneity, Adoption, Authority Index, Federalism ----

