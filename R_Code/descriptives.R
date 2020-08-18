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
df_hhi <- read_csv("Measures/hhi.csv", guess_max = 10000)
df_PAX <- readRDS("~/Documents/github/corona_private/data/get_est.rds")
df_fed <- read_csv("Measures/fed.csv", guess_max = 10000)
df_selected<-df_main %>%
  filter(type%in%c("Closure and Regulation of Schools","Lockdown","Restrictions of Mass Gatherings")
  )

# Descriptive----
# - Cases Plot----

gg1.1<- ggplot(df_cases %>% filter(region == "National") %>% select(date, country, region, cases, cases_national) %>% unique(), aes(x=date, y=cases, color=country)) + geom_line() + labs(x = "Date", y = "Cases", color="Country") + ggtitle("Cumulative COVID-19 Cases per Country") +theme_bw()
gg1.2<- ggplot(df_main %>% filter(region == "National") %>% select(date, country, region, cases, past_average_new_cases_national) %>% unique(), aes(x=date, y=past_average_new_cases_national, color=country)) + geom_line() + labs(x = "Date", y = "New Cases (7 days avg)", color="Country") + ggtitle("Past 7 Days Average of New COVID-19 Cases per Country") +theme_bw()

gg1.2
#Combine 
gg3<- ggplot(df_main %>% filter(region == "National") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date,color=country))+ geom_line(aes(y =cases_national))
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40),linetype="dashed")+scale_color_manual(values=c('red','blue',"green","orange"))
gg1.3 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+theme_bw()

gg1.3

# For each country individual

gg3<- ggplot(df_main %>% filter(region == "National", country=="Switzerland") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.4 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Switzerland")+theme_bw()

gg3<- ggplot(df_main %>% filter(region == "National", country=="France") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.5 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases France")+theme_bw()

gg3<- ggplot(df_main %>% filter(region == "National", country=="Italy") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.6 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Italy")+theme_bw()

gg3<- ggplot(df_main %>% filter(region == "National", country=="Germany") %>% select(date, country, region, cases, cases_national, past_average_new_cases_national) %>% unique(), aes(x=date))+ geom_line(aes(y = cases_national))+ labs(y="Cumulative National Cases")
gg3<- gg3+ geom_line(aes(y = past_average_new_cases_national*40), linetype="dashed") 
gg1.7 <- gg3 + scale_y_continuous(sec.axis = sec_axis(~./40, name = "7 Days Average of New Cases (dashed)"))+ ggtitle("COVID-19 Cases Germany")+theme_bw()


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
gg4

# - Heterogeneity Index----
gg5
# - Heterogeneity Index----
gg6
# - PAX----


get_est_sum <- get_est %>%
  ungroup %>%
  mutate(estimate=(estimate-min(estimate))/(max(estimate)-min(estimate))*100,
         date_announced=ymd(as.character(date_announced))) %>%
  group_by(country,date_announced) %>%
  summarize(med_est=median(estimate),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05)) %>%
  group_by(date_announced) %>%
  mutate(`Country Rank`=rank(med_est))


gg7.1<- get_est_sum %>% 
  ggplot(aes(y=med_est,x=date_announced)) +
  geom_line(data=df, aes(group=country), color="lightgrey", size=0.25,show.legend=FALSE) +
  geom_line(data=df[df$country%in% c("Germany","France","Italy","Switzerland"),], aes(color=country),size=0.7,show.legend=T)+
  scale_color_manual(name="Country",values=c('#00CC33','#E69F00','#CC0000',"#006699"))+
  theme_bw() +
  xlab("") +
  ylab("Policy Activity Index")+
  ggtitle("Policy Activity Index")


gg7.1

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
  ggplot(aes(x = date, fill = init_country_level)) + geom_density(alpha = 0.5) + facet_wrap(~country)+ theme_bw()+ggtitle("Selected Policies")
  

gg9.2<- df_main %>% 
  filter(!is.na(type)) %>% 
  group_by(type,date,init_country_level,country) %>% 
  summarize(Policies=length(unique(policy_id))) %>% 
  arrange(type,date) %>% 
  mutate(Policies=cumsum(Policies)) %>% 
  ungroup %>% 
  ggplot(aes(x = date, fill = init_country_level)) + geom_density(alpha = 0.5) + facet_wrap(~country)+ theme_bw()+ggtitle("All Policies")



# - Summary Stats----
## Statistics Summary of Model


df_main_summary <- na.omit(df_main[,cbind("Centrality","Heterogeneity","PAX","Authority","Federalism","hhi","measure_H1_H2","measure_H3")]) 


stargazer(df_main_summary, type="latex", float=F,covariate.labels = c("Centrality",
                                                                      "Heterogeneity",
                                                                      "PAX",
                                                                      "Authority",
                                                                      "Federalism",
                                                                      "hhi",
                                                                      "measure_H1_H2",
                                                                      "measure_H3"),
          title = "Summary Statistics", style = "commadefault",decimal.mark=".",out="Results/summary_statistics_model.tex")

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

dat<- left_join(cases_national,df_fed,by=c("country"="Jurisdiction Name"))
dat$fed<-as.character(dat$HueglinFennaFederalPolity) %>% as.factor()

p<- ggplot(dat,aes(x=fed, y=log(cases), color=fed)) +
  geom_boxplot()

p
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