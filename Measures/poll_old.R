# Polling Data

library(readr)
library(readxl)
YG_fear<- read_csv("data/YouGov/fear_of_catching.csv") #Percent of People who somewhat or heavily fear
YG_fear$DateTime <- format(as.Date(YG_fear$DateTime, format="%d/%m/%Y"),"%Y-%m-%d")%>% as.character()
YG_fear$DateTime <- as.Date(YG_fear$DateTime)
YG_fear$fear <- as.numeric(YG_fear$fear)
YG_fear<- YG_fear%>% gather(country, fear, France:Spain)
ST_fear<- read_xlsx("data/sotomo/gefaehrlichkeit_einschaetzung_2.xlsx") #«Vor welchen Folgen der Corona-Krise auf persönlicher Ebene fürchten Sie sich besonders?»
YG_f<-left_join(YG_fear,ST_fear, by=("DateTime"="date"))


YG_govt <- read_xlsx("data/YouGov/yougov-chart.xlsx")#Percent of People who think the govt does a somewhat or very good job
YG_govt$DateTime <- format(as.Date(YG_govt$DateTime),"%Y-%m-%d")%>% as.character()
YG_govt$DateTime <- as.Date(YG_govt$DateTime)
YG_govt<- YG_govt%>% gather(country, govt, France:Italy)


ST_govt<- read_xlsx("data/sotomo/gefaehrlichkeit_govt.xlsx")#«Massnahmen angemessen, die die persönliche Bewegungsfreiheit einschränken (Verbot von Ansammlungen mit mehr alsfünf Personen, Besuchsverbot usw.)


time<- df_cases%>% select(1,3)

test<- left_join(time,YG_fear, by=c("country","date"="DateTime"))
test<- left_join(test,YG_govt, by=c("country","date"="DateTime"))%>% unique()



time<- df_cases%>% select(1,3)
time$fear <- 0
time$govt <- 0

time$fear[time$fear]

