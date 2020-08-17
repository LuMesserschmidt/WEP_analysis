
rm(list=ls())

library(hhi)
library(readr)
library(tidyverse)
library(scales)
names(combined_cases)
combined_cases <- read_csv("data/merged_final.csv", guess_max = 10000)%>% select(1:24) 
combined_cases <- combined_cases[!duplicated(combined_cases),]

# Transform infinite numbers to NaN
combined_cases[sapply(combined_cases, is.infinite)] <- NaN


# Removing negative values for new cases (might be due to re-estimation)

combined_cases <- combined_cases[!combined_cases$new_cases<0,]

combined_cases <- combined_cases[!is.na(combined_cases$date),]



#cases$cases_pop_sub = sub national cases / sub national population
#cases$cases_pop_nat = national cases / national population

#Ratio of sub national to national cases by population 
# cases$ratio_pop_cum = subnational cumulative cases per population - national cumulative cases per population

# Ratio to overall cases

#cases$ratio_cum
#cases$ratio_new 


# Quadratic Ratio
#cases$ratio_cum2 
#cases$ratio_new2



cases<-combined_cases[!is.na(combined_cases$region),]

write_csv(cases,"data/Cases/cases.csv")

hhi<- combined_cases %>% group_by(country,date)%>% summarise(hhi_cumulative=sum(ratio_cum2,na.rm=T),
                                                    hhi_new=sum(ratio_new2,na.rm=T)
                                                    )
write.csv(hhi,"Measures/hhi.csv")


