
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

combined_cases <- combined_cases[!combined_cases$new_cases>=0,]

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

hhi<- cases %>% group_by(country,date)%>% summarise(hhi_cumulative=sum(ratio_cum2,na.rm=T),
                                                    hhi_new=sum(ratio_new2,na.rm=T)
                                                    )
write.csv(hhi,"Measures/hhi.csv")
# Vizualization

plot_hhi_cum<- hhi %>%ggplot( aes(x=date, y=hhi_cumulative, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Relative Herfindahl Concentration (cumulative cases)") +
  xlab("")

plot_hhi_new<- hhi %>%ggplot( aes(x=date, y=hhi_new, color=country)) +
  geom_line(size=0.15)+
  geom_point(size=0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylim(0,1)+
  ylab("Relative Herfindahl Concentration (new cases)") +
  xlab("")


ggsave(filename="Results/plot_hhi_cum.jpeg",
       plot=plot_hhi_cum,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)



ggsave(filename="Results/plot_hhi_new.jpeg",
       plot=plot_hhi_new,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)




