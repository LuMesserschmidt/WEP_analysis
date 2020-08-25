#Regional Authority Index & Federalims Measurement

library(readxl)

# Load RAI data 

rai <- read_excel("data/Federalims/RAI.xlsx") %>% filter(year==2010) %>% select(5,17:19) %>% mutate(Self=(n_selfrule-min(n_selfrule))/(max(n_selfrule)-min(n_selfrule)), 
                                                                                                    RAI=(n_RAI-min(n_RAI))/(max(n_RAI)-min(n_RAI)))


# Load fed data

fed <- read_excel("data/Federalims/FederalPoliticalSystems.xlsx")%>% select(-2,-3,-5)

# merge and save

df_fed<-left_join(fed,rai,by=c("Country Code"="abbr_country"))

write_csv(df_fed,"Measures/fed.csv")
