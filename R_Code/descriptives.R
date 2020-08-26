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

# Prepare data & Packages

rm(list=ls())

library(readr)
library(tidyverse)
library(scales)
library(ggrepel)
library(sf)
library(lubridate)
library(gghighlight)
library(kableExtra)
library(knitr)


df_deaths <- read_csv("data/Cases/deaths.csv", guess_max = 10000)
df_cases <- read_csv("data/Cases/cases.csv", guess_max = 10000)
df_ECDC <- read_csv("data/Cases/final_cases.csv", guess_max = 10000)
df_main <- read_csv("data/merged_final.csv", guess_max = 10000)
df_policy <- read_csv("data/CoronaNet/coronanet_internal.csv", guess_max = 10000)
df_heterogeneity <- read_csv("data/heterogeneity_analyses.csv")
df_centrality <- readRDS("~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds")
df_hhi <- read_csv("Measures/hhi.csv", guess_max = 10000)
df_hhi_deaths <- read_csv("Measures/hhi_deaths.csv", guess_max = 10000)
df_PAX <- readRDS("~/Documents/github/corona_private/data/get_est.rds")
df_fed <- read_csv("Measures/fed.csv", guess_max = 10000)
df_selected<-df_main %>%
  filter(type%in%c("Closure and Regulation of Schools","Lockdown","Restrictions of Mass Gatherings","Social Distancing")
  )
# Descriptive----
# - Cases Plot----



gg1.1<- ggplot(df_ECDC, aes(x=date, y=cases_national_ECDC, color=country)) + geom_line() + labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw()+
          ggsave(filename = "results/Descriptives/gg1_1.jpg",
         height = 7)

gg1.2<- ggplot(df_ECDC, aes(x=date, y=past_average_new_cases_national_ECDC, color=country)) + geom_line() + labs(x = "Date", y = "New Cases (7 days avg)", color="Country") + ggtitle("Past 7 Days Average of New COVID-19 Cases per Country") +theme_bw()+
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg1_2.jpg",
         height = 7)

gg1.3<- ggplot(df_deaths, aes(x=date, y=deaths_national, color=country)) + geom_line() + labs(x = "Date", y = "Deaths", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Deaths per Country") +theme_bw()+
  ggsave(filename = "results/Descriptives/gg1_3.jpg",
         height = 7)
gg1.4<- ggplot(df_deaths, aes(x=date, y=past_average_new_deaths_national, color=country)) + geom_line() + labs(x = "Date", y = "New Deaths (7 days avg)", color="Country") + ggtitle("Past 7 Days Average of New COVID-19 Deaths per Country") +theme_bw()+
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg1_4.jpg",
         height = 7)




#Combine 
gg3<- ggplot(df_ECDC, aes(x=date,color=country))+ geom_line(aes(y =past_average_new_cases_national_ECDC),size=0.5, alpha=0.4)+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+ylab("7 Days Average of New Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_deaths_national),linetype="dashed")
gg1.5 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "7 Days Average of New Deaths (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_5.jpg",height = 7)

names(df_ECDC)
gg3<- ggplot(df_ECDC, aes(x=date,color=country))+ geom_line(aes(y =cases_national_ECDC),size=0.5, alpha=0.4)+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+ylab("Cumulative Cases")
gg3<- gg3+ geom_line(aes(y = deaths_national),linetype="dashed")
gg1.0 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "Cumulated Deaths (dashed)"))+theme_bw()+ggtitle("Cumulative Development of Cases and Deaths by COVID-19")+
  ggsave(filename = "results/Descriptives/gg1_0.jpg",height = 7)

# For each country individual

gg3<- ggplot(df_ECDC%>% filter(country=="Switzerland"), aes(x=date))+ geom_line(aes(y =past_average_new_cases_national_ECDC),size=0.5, alpha=0.4,color="#006699")+ ggtitle("COVID-19 Cases Switzerland")+ylab("7 Days Average of New Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_deaths_national),linetype="dashed",color="#006699")
gg1.6 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "7 Days Average of New Deaths (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_6.jpg",height = 7)

gg3<- ggplot(df_ECDC%>% filter(country=="France"), aes(x=date))+ geom_line(aes(y =past_average_new_cases_national_ECDC),size=0.5, alpha=0.4,color="#00CC33")+ ggtitle("COVID-19 Cases France")+ylab("7 Days Average of New Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_deaths_national),linetype="dashed",color="#00CC33")
gg1.7 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "7 Days Average of New Deaths (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_7.jpg",height = 7)

gg3<- ggplot(df_ECDC%>% filter(country=="Germany"), aes(x=date))+ geom_line(aes(y =past_average_new_cases_national_ECDC),size=0.5, alpha=0.4,color="#CC0000")+ ggtitle("COVID-19 Cases Germany")+ylab("7 Days Average of New Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_deaths_national),linetype="dashed",color="#CC0000")
gg1.8 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "7 Days Average of New Deaths (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_8.jpg",height = 7)

gg3<- ggplot(df_ECDC%>% filter(country=="Italy"), aes(x=date))+ geom_line(aes(y =past_average_new_cases_national_ECDC),size=0.5, alpha=0.4,color="#E69F00")+ ggtitle("COVID-19 Cases Italy")+ylab("7 Days Average of New Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_deaths_national),linetype="dashed",color="#E69F00")
gg1.9 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~., name = "7 Days Average of New Deaths (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_9.jpg",height = 7)


# - Concentration ----

# Vizualization

gg2.1<- df_hhi %>%ggplot( aes(x=date, y=hhi_cumulative, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Relative Herfindahl Concentration (cumulative cases)") +
  xlab("")+
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg2_1.jpg",height = 7)

gg2.2<- df_hhi %>%ggplot(aes(x=date, y=hhi_new, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylim(0,1)+
  ylab("Relative Herfindahl Concentration (new cases)") +
  xlab("")+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg2_2.jpg",height = 7)

gg2.3<- df_hhi_deaths %>%ggplot( aes(x=date, y=hhi_cumulative_deaths, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Relative Herfindahl Concentration (cumulative deaths)") +
  xlab("")+
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg2_3.jpg",height = 7)

gg2.4<- df_hhi_deaths %>%ggplot(aes(x=date, y=hhi_new_deaths, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylim(0,1)+
  ylab("Relative Herfindahl Concentration (new deaths)") +
  xlab("")+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg2_4.jpg",height = 7)



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
corona$code<-corona$geo

df <- corona %>%
  select("date","cases","new_cases","ratio_cum","ratio_new","code", "cases_pop_sub","past_average_new_cases","past_average") %>% 
  mutate(country = str_sub(code,1,2))%>%
  filter(date=="2020-03-15" |
           date=="2020-03-15" |
           date=="2020-04-01"    
  )%>%
  filter(!code%in% c("FRY1","FRY2","FRY3","FRY4","FRY5"))


merged_shape <- 
  left_join(bind_rows(shape,ch_shape,it_shape),
            df,
            by = c("NUTS_ID" = "code"))

library(viridis)


merged_shape<- merged_shape%>%mutate(country=recode(country,
                                                    "CH"="Switzerland",
                                                    "DE"="Germany",
                                                    "FR"="France",
                                                    "IT"="Italy"))
gg3.1 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(ratio_cum))) + 
  geom_sf(aes(alpha = ratio_cum,
              fill = country),size = 0.1) + 
  scale_fill_manual(name="Country",values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(name="Relative Cases per Capita",range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Cases by Region")+
  ggsave(filename = "results/Descriptives/gg3_1.jpg",height = 7)

gg3.2 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(cases_pop_sub))) + 
  geom_sf(aes(alpha = cases_pop_sub,
              fill = country),size = 0.1) + 
  scale_fill_manual(name="Country",values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(name="Relative Cases per Capita",range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Cases by Region (per Capita)")+
  ggsave(filename = "results/Descriptives/gg3_2.jpg",height = 7)

figure <- ggpubr::ggarrange(gg3.1, gg3.2,
                    nrow = 2)+
  ggsave(filename = "results/Descriptives/gg3_3.jpg",height = 7)


# Map of Deaths

names(df_deaths)
corona <- df_deaths
corona$code<-corona$geo

df <- corona %>%
  select("date","deaths","new_deaths","ratio_cum","ratio_new","code", "deaths_pop_sub","past_average_new_deaths","past_average") %>% 
  mutate(country = str_sub(code,1,2))%>%
  filter( date=="2020-03-20" |
           date=="2020-04-05"    
  )%>%
  filter(!code%in% c("FRY1","FRY2","FRY3","FRY4","FRY5"))


merged_shape <- 
  left_join(bind_rows(shape,ch_shape,it_shape),
            df,
            by = c("NUTS_ID" = "code"))

library(viridis)


merged_shape<- merged_shape%>%mutate(country=recode(country,
                                                    "CH"="Switzerland",
                                                    "DE"="Germany",
                                                    "FR"="France",
                                                    "IT"="Italy"))
gg3.4 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(ratio_cum))) + 
  geom_sf(aes(alpha = ratio_cum,
              fill = country),size = 0.1) + 
  scale_fill_manual(name="Country",values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(name="Relative Deaths per Capita",range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Deaths by Region")+
  ggsave(filename = "results/Descriptives/gg3_4.jpg",height = 7)

gg3.5 <- 
  ggplot(merged_shape %>% 
           filter(!is.na(deaths_pop_sub))) + 
  geom_sf(aes(alpha = deaths_pop_sub,
              fill = country),size = 0.1) + 
  scale_fill_manual(name="Country",values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(name="Relative Deaths per Capita",range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Deaths by Region (per Capita)")+
  ggsave(filename = "results/Descriptives/gg3_5.jpg",height = 7)

figure <- ggpubr::ggarrange(gg3.4, gg3.5,
                    nrow = 2)+
  ggsave(filename = "results/Descriptives/gg3_6.jpg",height = 7)



# - Centrality Index----
gg4.1<- ggplot(df_centrality, aes(x=date, y=centDegStd, color=country)) + geom_smooth() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Centrality Index (smoothed)") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg4_1.jpg",
         height = 7)

gg4.2<- ggplot(df_centrality, aes(x=date, y=centDegStd, color=country)) + geom_line() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Centrality Index") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg4_2.jpg",
         height = 7)

# - Heterogeneity Index----
gg5.1<- ggplot(df_heterogeneity, aes(x=date, y=hetero, color=country)) + geom_smooth() +
  ylim(0, 1) +
  labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Heterogeneity Index (smoothed)") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg5_1.jpg",
         height = 7)

gg5.2<- ggplot(df_heterogeneity, aes(x=date, y=hetero, color=country)) + geom_line() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Heterogeneity Index") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg5_2.jpg",
         height = 7)

gg5.3<- ggplot(df_heterogeneity, aes(x=date, y=hetero_mean, color=country)) + geom_smooth() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Heterogeneity Index (average)") +theme_bw()+
  ggsave(filename = "results/Descriptives/gg5_3.jpg",
         height = 7)

figure <- ggpubr::ggarrange(gg4.2, gg5.3,
                            nrow = 1,common.legend = T,legend="bottom")+
  ggsave(filename = "results/Descriptives/gg5_5.jpg",height = 7)

# - PAX----


get_est_sum <- df_PAX %>%
  ungroup %>%
  mutate(estimate=(estimate-min(estimate))/(max(estimate)-min(estimate))*100,
         date_announced=ymd(as.character(date_announced))) %>%
  dplyr::group_by(country,date_announced) %>%
  dplyr::summarize(med_est=median(estimate),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05)) %>%
  group_by(date_announced) %>%
  mutate(`Country Rank`=rank(med_est))

df <- get_est_sum
df_PAX <- get_est_sum

gg7.1<- ggplot(df,aes(y=med_est,x=date_announced)) +
  geom_line(data=df[df$country%in% c("France","Germany","Italy","Switzerland"),], aes(color=country),size=0.7,show.legend=T)+
  scale_color_manual(name="Country",values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  theme_bw() +
  xlab("") +
  ylab("Policy Activity Index")+
  ggtitle("Policy Activity Index")+
  ggsave(filename = "results/Descriptives/gg7_1.jpg",
         height = 7)


# - Table of Policies----

df_selected %>% 
  dplyr::group_by(type,init_country_level,country) %>% 
  dplyr::summarize(`Total Number of Policies`=n()) %>% 
  dplyr::select(Type="type",Level="init_country_level",everything()) %>% 
  filter(!is.na(Type)) %>% 
  arrange(country)  %>% 
  ungroup() %>%
  knitr::kable("html",booktabs=T,
               caption="Selected Policies by level") %>% 
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))%>% 
  kableExtra::column_spec(1,width="4cm") %>% 
  kableExtra::column_spec(2:4,width="1.5cm")


# - National Distribution Plot ----

#National Distribution Plot

# National vs Sub-National per country
## For selected Policies
gg9.1<- df_selected %>% 
  filter(!is.na(type)) %>% 
  group_by(type,date,init_country_level,country) %>% 
  dplyr::summarize(Policies=length(unique(policy_id))) %>% 
  arrange(type,date) %>% 
  mutate(Policies=cumsum(Policies)) %>% 
  ungroup %>% 
  ggplot(aes(x = date, fill = init_country_level)) + geom_density(alpha = 0.5) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  facet_wrap(~country)+ theme_bw()+ggtitle("Selected Policies")+
  ggsave(filename = "results/Descriptives/gg9_1.jpg",
         height = 7)


# - Summary Stats----
## Statistics Summary of Model


df_fed<-df_fed%>% rename(country="Jurisdiction Name")%>%select(2,5,9:10)%>% filter(country%in%c("France","Germany","Italy","Switzerland"))
df_hhi_deaths<-df_hhi_deaths%>% select(-"X1")
df_heterogeneity<-df_heterogeneity%>% select(1:3,"hetero","hetero_mean")
df_heterogeneity<-spread(df_heterogeneity, type, hetero)%>%rename("Het_Mean"=3,
                                                                  "Het_Lockdown"=4,
                                                                  "Het_Mask"=5,
                                                                  "Het_Mass"=6,
                                                                  "Het_School"=7)

df_centrality<-df_centrality%>% select(1:3,6)
df_centrality<-spread(df_centrality, type, centDegStd)%>%rename("Cent_School"=3,
                                                                    "Cent_Lockdown"=4,
                                                                    "Cent_Mask"=5,
                                                                    "Cent_Mass"=6)

df_PAX<-df_PAX%>%filter(country%in%c("France","Germany","Italy","Switzerland"))%>%rename("date"=2,"PAX"=3)%>% select(1:3)


df_merge<- left_join(df_ECDC,df_fed,by=c("country"="country"))
df_merge<- left_join(df_merge,df_centrality,by=c("country","date"))
df_merge<- left_join(df_merge,df_heterogeneity,by=c("country","date"))
df_merge<- left_join(df_merge,df_PAX,by=c("country","date"="date"))
df_merge<- left_join(df_merge,df_hhi_deaths,by=c("country","date"))




#Adjust Variables
df_main_summary <- df_merge%>%select("date",
                                     "country",
                                     "Self",                                
                                     "RAI",                                 
                                     "Cent_School",
                                     "Cent_Lockdown",                      
                                     "Cent_Mask",
                                     "Cent_Mass",
                                     "Het_Mean",
                                     "Het_Lockdown",
                                     "Het_Mask" ,                           
                                     "Het_Mass",
                                     "Het_School" ,                         
                                     "PAX",
                                     "hhi_cumulative_deaths",
                                     "measure_H1_H2_deaths",
                                     "measure_H3_deaths_a")
library(stargazer)
stargazer(df_main_summary, type="html", float=F,covariate.labels = c("Centrality",
                                                                      "Heterogeneity",
                                                                      "PAX",
                                                                      "Authority",
                                                                      "Federalism",
                                                                      "hhi",
                                                                      "measure_H1_H2"),
          title = "Summary Statistics", style = "commadefault",decimal.mark=".",out="Results/summary_statistics_model.html")

# Distribution H measures

names(df_ECDC)
gg11_1<- df_ECDC %>%ggplot( aes(x=date, y=measure_H1_H2_cases_ECDC, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H1 H2 Cases ECDC") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_1.jpg",
         height = 7)

gg11_2<- df_ECDC %>%ggplot( aes(x=date, y=measure_H1_H2_deaths, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H1 H2 Deaths") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_2.jpg",
         height = 7)

gg11_3<- df_ECDC %>%ggplot(aes(x=date, y=measure_H3_deaths_a, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H3 Deaths (Version a)") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_3.jpg",
         height = 7)

gg11_4<- df_ECDC %>%ggplot( aes(x=date, y=measure_H3_deaths_b, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H3 Deaths (Version b)") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_4.jpg",
         height = 7)




# Bivariate----
# - Cases vs Federal/Unitary Scores----
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data$dateRep <- format(as.Date(data$dateRep, format="%d/%m/%Y"),"%Y-%m-%d")%>% as.character()
data$dateRep <- as.Date(data$dateRep)
case<- data %>%
  select(1,5,7,10)%>%
  rename(country = `countriesAndTerritories`, date = dateRep, new_cases_national_ECDC = cases,pop=popData2019)%>%
  arrange(date,country)

cased<- case%>%
  group_by(country)%>%
  mutate(cases_national_ECDC=cumsum(new_cases_national_ECDC),
         cases_relative=cases_national_ECDC/pop*100)
cases_national<-cased[!is.na(cased$cases_relative),]


dat<- left_join(cases_national,df_fed,by=c("country"="country"))
dat$fed<-as.character(dat$HueglinFennaFederalPolity) %>% as.factor()

gg12.1<- dat%>% filter(!is.na(fed),date=="2020-08-24") %>% ggplot(aes(x=fed, y=(cases_relative))) +
  geom_boxplot()+ theme_bw()+ggtitle("Boxplot of Relative Cases by Federalism")+
  ggsave(filename = "results/Descriptives/gg12_1.jpg",
         height = 7)


gg12.2<-ggplot(dat%>% filter(!is.na(fed),date=="2020-08-24"), aes(y = log(cases_relative)))+
  geom_smooth(aes(x = RAI, colour = "Regional Authority Index"))+
  geom_smooth(aes(x = Self, colour = "Self-Rule"))+
  ggtitle("Regional Authority Indices")+
  ylab("Cases (log)")+
  xlab("Index Score (log)")+
  theme_bw()+
  ggsave(filename = "results/Descriptives/gg12_2.jpg",
         height = 7)


# - Correlation Matrix (by date): Centrality, PAX, Heterogeneity, Adoption, Authority Index, Federalism ----
library(corrplot)
##Subset 

df_main_cor <- df_main_summary[,-c(1:2)]

## Rename
#df_main_cor$"Concentration"<-df_main_cor$hhi
#df_main_cor$"Measure_H1_H2"<-df_main_cor$measure_H1_H2
#df_main_cor$"Measure_H3"<-df_main_cor$measure_H3
#df_main_cor<- df_main_cor[,-c(6:8)]

## Correlation Function (calculates p-values). Source: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
A<-corstars(df_main_cor)
library(xtable)
xtable(A)

M <- cor(df_main_cor,use="pairwise.complete.obs")

library(tableHTML)

write_tableHTML(tableHTML(A), file = 'Results/Correlation_Matrix.html')

