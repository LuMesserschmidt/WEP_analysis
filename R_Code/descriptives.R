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


df_cases <- read_csv("data/Cases/cases.csv", guess_max = 10000)
df_main <- read_csv("data/merged_final.csv", guess_max = 10000)
df_policy <- read_csv("data/CoronaNet/coronanet_internal.csv", guess_max = 10000)
df_heterogeneity <- read_csv("data/heterogeneity_analyses.csv")
df_centrality <- readRDS("~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds")
df_hhi <- read_csv("Measures/hhi.csv", guess_max = 10000)
df_PAX <- readRDS("~/Documents/github/corona_private/data/get_est.rds")
df_fed <- read_csv("Measures/fed.csv", guess_max = 10000)
df_selected<-df_main %>%
  filter(type%in%c("Closure and Regulation of Schools","Lockdown","Restrictions of Mass Gatherings","Social Distancing")
  )
# Descriptive----
# - Cases Plot----


gg1.1<- ggplot(df_cases, aes(x=date, y=cases_national, color=country)) + geom_line() + labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw()+
          ggsave(filename = "results/Descriptives/gg1_1.jpg",
         height = 7)
gg1.2<- ggplot(df_cases, aes(x=date, y=past_average_new_cases_national, color=country)) + geom_line() + labs(x = "Date", y = "New Cases (7 days avg)", color="Country") + ggtitle("Past 7 Days Average of New COVID-19 Cases per Country") +theme_bw()+
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggsave(filename = "results/Descriptives/gg1_2.jpg",
         height = 7)


#Combine 
gg3<- ggplot(df_cases, aes(x=date,color=country))+ geom_line(aes(y =cases_national))+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40),linetype="dashed")+scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
gg1.3 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+theme_bw()+  
  ggsave(filename = "results/Descriptives/gg1_3.jpg",height = 7)

# For each country individual

gg3<- ggplot(df_cases %>% filter(country=="Switzerland"), aes(x=date))+ geom_line(aes(y = cases_national),color="#006699")+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed", color="#006699")
gg1.4 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Switzerland")+theme_bw()+ggsave(filename = "results/Descriptives/gg1_4.jpg",height = 7)

gg3<- ggplot(df_cases%>% filter(country=="France"), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")+scale_color_manual(values=c('#00CC33'))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") +scale_color_manual(values=c('#00CC33'))
gg1.5 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases France")+theme_bw()+ggsave(filename = "results/Descriptives/gg1_5.jpg",height = 7)

gg3<- ggplot(df_cases%>% filter(country=="Italy"), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")+scale_color_manual(values=c('#CC0000'))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") +scale_color_manual(values=c('#CC0000'))
gg1.6 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Italy")+theme_bw()+ggsave(filename = "results/Descriptives/gg1_6.jpg",height = 7)

gg3<- ggplot(df_cases%>% filter(country=="Germany"), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")+scale_color_manual(values=c('#E69F00'))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") +scale_color_manual(values=c('#E69F00'))
gg1.7 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Germany")+theme_bw()+ggsave(filename = "results/Descriptives/gg1_7.jpg",height = 7)


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
  scale_fill_manual(values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
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
  scale_fill_manual(values = c('#00CC33','#E69F00','#CC0000',"#006699"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")+
  ggtitle("Relative Cumulative Cases by Region (relative by sub-national population)")+
  ggsave(filename = "results/Descriptives/gg3_2.jpg",height = 7)


# - Centrality Index----
gg4.1<- ggplot(df_centrality, aes(x=date, y=centDegStd, color=country)) + geom_smooth() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg4_1.jpg",
         height = 7)

gg4.2<- ggplot(df_centrality, aes(x=date, y=centDegStd, color=country)) + geom_line() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg4_2.jpg",
         height = 7)

# - Heterogeneity Index----
gg5.1<- ggplot(df_heterogeneity, aes(x=date, y=hetero, color=country)) + geom_smooth() +
  ylim(0, 1) +
  labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg5_1.jpg",
         height = 7)

gg5.2<- ggplot(df_heterogeneity, aes(x=date, y=hetero, color=country)) + geom_line() +
  ylim(0, 1) +labs(x = "Date", y = "Cases", color="Country") +
  scale_color_manual(values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw() + facet_wrap(~type)+
  ggsave(filename = "results/Descriptives/gg5_2.jpg",
         height = 7)

# - PAX----


get_est_sum <- df_PAX %>%
  ungroup %>%
  mutate(estimate=(estimate-min(estimate))/(max(estimate)-min(estimate))*100,
         date_announced=ymd(as.character(date_announced))) %>%
  group_by(country,date_announced) %>%
  summarize(med_est=median(estimate),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05)) %>%
  group_by(date_announced) %>%
  mutate(`Country Rank`=rank(med_est))

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
  group_by(type,init_country_level,country) %>% 
  summarize(`Total Number of Policies`=n()) %>% 
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
  summarize(Policies=length(unique(policy_id))) %>% 
  arrange(type,date) %>% 
  mutate(Policies=cumsum(Policies)) %>% 
  ungroup %>% 
  ggplot(aes(x = date, fill = init_country_level)) + geom_density(alpha = 0.5) + facet_wrap(~country)+ theme_bw()+ggtitle("Selected Policies")+
  ggsave(filename = "results/Descriptives/gg9_1.jpg",
         height = 7)


gg9.2<- df_main %>% 
  filter(!is.na(type)) %>% 
  group_by(type,date,init_country_level,country) %>% 
  summarize(Policies=length(unique(policy_id))) %>% 
  arrange(type,date) %>% 
  mutate(Policies=cumsum(Policies)) %>% 
  ungroup %>% 
  ggplot(aes(x = date, fill = init_country_level)) + geom_density(alpha = 0.5) + facet_wrap(~country)+ theme_bw()+ggtitle("All Policies")+
  ggsave(filename = "results/Descriptives/gg9_2.jpg",
         height = 7)



# - Summary Stats----
## Statistics Summary of Model


df_fed<-df_fed%>% rename(country="Jurisdiction Name")%>%select(2,5,9:11)%>% filter(country%in%c("France","Germany","Italy","Switzerland"))
df_hhi<-df_hhi%>% select(-"X1")
df_heterogeneity<-df_heterogeneity%>% select(1:3,"hetero")
df_heterogeneity<-spread(df_heterogeneity, type, hetero)%>%rename("Het_Lockdown"=3,
                                                                  "Het_Mask"=4,
                                                                 "Het_Mass"=5,
                                                                  "Het_School"=6)

df_centrality<-df_centrality%>% select(1:3,6)
df_centrality<-spread(df_centrality, type, centDegStd)%>%rename("Cent_School"=3,
                                                                    "Cent_Lockdown"=4,
                                                                    "Cent_Mask"=5,
                                                                    "Cent_Mass"=6)

df_PAX<-df_PAX%>%filter(country%in%c("France","Germany","Italy","Switzerland"))%>%rename("date"=2,"PAX"=3)%>% select(1:3)


df_merge<- left_join(df_cases,df_fed,by=c("country"))
df_merge<- left_join(df_merge,df_centrality,by=c("country","date"))
df_merge<- left_join(df_merge,df_heterogeneity,by=c("country","date"))
df_merge<- left_join(df_merge,df_PAX,by=c("country","date"))
df_merge<- left_join(df_merge,df_hhi,by=c("country","date"))
df_merge<- left_join(df_merge,df_fed,by=c("country","date"))

names(df_main_summary)

#Adjust Variables
df_main_summary <- df_merge%>%select(1,3,7,8,9:13)

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

gg11_1<- df_cases %>%ggplot( aes(x=date, y=measure_H1_H2, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H1 H2") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_1.jpg",
         height = 7)


gg11_2<- df_cases %>%ggplot( aes(x=date, y=measure_H3, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Measure H3") +
  xlab("")+
  ggsave(filename = "results/Descriptives/gg11_2.jpg",
         height = 7)


# Bivariate----
# - Cases vs Federal/Unitary Scores----

cases_national <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

cases_national <- cases_national %>%
  filter(is.na(`Province/State`)) %>%
  select(-`Province/State`, -Lat, -Long) %>%
  data.table::melt(id.vars=c("Country/Region")) %>%
  rename(country = `Country/Region`, date = variable, cases = value) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%y"))%>%
  filter(date=="2020-07-15")%>%
  filter(!is.na(cases))
cases_national$region <- "National"

dat<- left_join(cases_national,df_fed,by=c("country"="country"))
dat$fed<-as.character(dat$HueglinFennaFederalPolity) %>% as.factor()

gg12.1<- dat%>% filter(!is.na(fed)) %>% ggplot(aes(x=fed, y=log(cases), color=fed)) +
  geom_boxplot()+
  ggsave(filename = "results/Descriptives/gg12_1.jpg",
         height = 7)



a<-ggplot(dat, aes(x = log(cases)))+
  geom_smooth(aes(y = RAI, colour = "Regional Authority Index"))+
  geom_smooth(aes(y = Self, colour = "Self-Rule"))+
  ggtitle("Regional Authority Indices")+
  ylab("Score")+
  xlab("Cases (log)")+
  theme_bw()
gg11.2<- a+labs(color='Indices')+
  ggsave(filename = "results/Descriptives/gg12_2.jpg",
         height = 7)


# - Correlation Matrix (by date): Centrality, PAX, Heterogeneity, Adoption, Authority Index, Federalism ----
library(corrplot)
##Subset 
y<- c("Centrality",
      "Heterogeneity",
      "PAX",
      "Authority",
      "Federalism",
      "hhi",
      "measure_H1_H2",
      "measure_H3") 
df_main_cor <- df_main[,cbind(y)]

## Rename
df_main_cor$"Concentration"<-df_main_cor$hhi
df_main_cor$"Measure_H1_H2"<-df_main_cor$measure_H1_H2
df_main_cor$"Measure_H3"<-df_main_cor$measure_H3
df_main_cor<- df_main_cor[,-c(6:8)]

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

C<-head(round(M,2))