#Regional Authority Index & Federalims Measurement

library(readxl)

# Load RAI data 

rai <- read_excel("data/Federalims/RAI.xlsx") %>% filter(year==2010) %>% select(5,17:19) %>% mutate(Self=n_selfrule/18, Shared=n_sharedrule/12, RAI=n_RAI/30)

# Load fed data

fed <- read_excel("data/Federalims/FederalPoliticalSystems.xlsx")%>% select(-2,-3,-5)

# merge and save

df_fed<-left_join(fed,rai,by=c("Country Code"="abbr_country"))

write_csv(df_fed,"Measures/fed.csv")
