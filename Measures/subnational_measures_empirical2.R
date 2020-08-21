##########################################################
# CoronaNet measures of subnational policy fragmentation #
#             by JOAN                                    #
##########################################################

library(rstudioapi)
library(dplyr)

##Load Luca's subnational data

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#subnational_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/Cases/combined_cases.csv', sep=",")

subnational_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/Cases/cases.csv', sep=",")

#Load CoronaNet (latest version)

corona <- readRDS('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_Aug18.RDS')

#Create a base dataset based on policy types
province <- unique(subnational_data$region)
type <- unique(corona$type)
date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(province = province,
                    date = date,
                    type = type
)
base$country <- c(rep("France", 18), rep("Germany", 16), rep("Italy", 20), rep("Switzerland", 26))
base <- base[, c('country', 'province', 'date', 'type')]

corona_sel <- corona

corona_sel[which(corona_sel$target_province == 'Latium'),]$target_province <- rep("Lazio", length(corona_sel[which(corona_sel$target_province == 'Latium'),]$target_province))

#corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-06-02"),] 
#corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-04-13"),] 
#corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$date_start >= "2020-03-27" & corona_sel$date_end <= "2020-05-11"),] 
#corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & is.na(corona_sel$type_sub_cat)),] 

#corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "Provincial"),]
#corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "National" & corona_sel$date_start == "2020-03-05"),]

corona_sel <- corona_sel[-which(corona_sel$type == 'Lockdown' & corona_sel$type_text == "relax restrictions"),]

#corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-05-18"),]
corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-03-10" & is.na(corona_sel$type_sub_cat)),]

corona_sel[which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & 
                   corona_sel$init_country_level == "National" & corona_sel$date_start == "2020-03-10"),]$date_end <- as.Date("2020-05-04")

corona_sel <- corona_sel[-which(corona_sel$country == 'Germany' & corona_sel$type == 'Lockdown' &
                   corona_sel$init_country_level == "Provincial" & corona_sel$date_start == "2020-06-23"&
                   corona_sel$target_province == "North Rhine-Westphalia"),]

corona_sel <- corona_sel[-which(corona_sel$date_start > corona_sel$date_end),]
#corona_sel <- corona_sel[-which(corona_sel$target_country == 'Germany' & corona_sel$type == 'Closure and Regulation of Schools' & 
#                                  is.na(corona_sel$type_sub_cat)),]
#corona_sel <- corona_sel[-which(duplicated(corona_sel)),]

corona_sel <- corona_sel[-which(corona_sel$target_country == "Switzerland" & corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$date_start > "2020-03-13" & corona_sel$date_end < "2020-06-08"),]

corona_sel[which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$target_province == "Liguria"),]$date_end <- rep(as.Date("2020-03-04"), 9)

corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Secondary Schools allowed to open with conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Secondary Schools allowed to open with no conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Higher education institutions allowed to open with no conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Higher education institutions allowed to open with conditions"),]
corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Primary Schools allowed to open with no conditions"),]
corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Primary Schools allowed to open with conditions"),]
corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Preschool or childcare facilities allowed to open with conditions"),]
corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Preschool or childcare facilities allowed to open with no conditions"),]

corona_sel[which(corona_sel$target_country == "France" & corona_sel$type == 'Closure and Regulation of Schools' &
                   corona_sel$init_country_level == "Provincial" & corona_sel$target_province == "Corsica"),]$date_end <- rep(corona_sel[which(corona_sel$target_country == "France" & 
                                                                                                                                                 corona_sel$type == 'Closure and Regulation of Schools' &
                                                                                                 corona_sel$init_country_level == "National"),]$date_start[1],
                                                                              length(corona_sel[which(corona_sel$target_country == "France" & 
                                                                                                        corona_sel$type == 'Closure and Regulation of Schools' &
                                                                                                        corona_sel$init_country_level == "Provincial" &
                                                                                                        corona_sel$target_province == "Corsica"),]$date_end))


#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" &
#                   corona_sel$type_sub_cat == "Cancellation of a recreational or commercial event"),]

#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" &
#                                 corona_sel$type_sub_cat == "Postponement of a recreational or commercial event"),]

#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" &
#                                  corona_sel$type_sub_cat == "Annually recurring event allowed to occur with certain conditions"),]

#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" &
#                                  corona_sel$type_sub_cat == "Cancellation of an annually recurring event"),]

#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" &
#                                  corona_sel$type_sub_cat == "Postponement of an annually recurring event"),]

###restrictions above 500 as the cut-off point

corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "100"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "10"),]
#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Max 100 people per room, not more than 1000 in total (both excluding active participants)"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Outdoor event max of 350, indoor event max of 150"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "inside: 50, outside: 100"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "50"),]
#corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "more then 5"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "250"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "75 and 150"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Maximum of 10"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "2"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "20"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "6"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "3"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "200"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "5"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "75"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Max of 100 after May 27, max of 250 after June 10 (outdoors)"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "events: max 300 people             gatherings in public: max 30 people"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Max 75 people from June 10, max 150 from June 24 (indoors)"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Maximum five people or two households, in private/family areas, up to 50 people, if this is for imperative reasons."),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "Max of 10 people at sports training/competition"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "max. 5"),]
corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & corona_sel$type_mass_gathering == "No more than two households may now gather in public."),]

corona_sel <- corona_sel[-which(corona_sel$type == "Restrictions of Mass Gatherings" & is.na(corona_sel$type_mass_gathering)),]

corona_sel <- corona_sel[-which(corona_sel$compliance == "Voluntary/Recommended but No Penalties"),]

detach("package:plyr", unload = TRUE)

corona_sel <- corona_sel %>%
  group_by(target_country, type) %>%
  filter(!init_country_level == "Municipal") %>%
  select(target_country, init_country_level, target_province, type, type_sub_cat, date_start, date_end, target_city
         )

corona_sel <- corona_sel[which(is.na(corona_sel$target_city)),]
corona_sel <- corona_sel[-which(is.na(corona_sel$target_province)),]
corona_sel <- rbind(data.frame(corona_sel), 'Lombardy' = c('Italy', 'National', 'Lombardia', 'Lockdown', 'Lockdown applies to all people', '2020-03-08', '2020-03-10', NA))

corona_sel = corona_sel %>%
  mutate(gov = ifelse(init_country_level == "National" & is.na(target_province)
                      & is.na(target_city), target_country, target_province)) %>%
  select(-target_province)


corona_sel$gov <- ifelse(corona_sel$gov == "Abruzzo,Aosta Valley,Apulia,Basilicate,Calabria,Campania,Emilia-Romagna,Friuli Venezia Giulia,Latium,Liguria,Lombardy,Molise,Piedmont,Sardinia,Sicily,The Marches,Trentino-Alto Adige,Tuscany,Umbria,Veneto", "Italy", corona_sel$gov)
corona_sel$gov <- ifelse(corona_sel$gov == "Baden-Wuerttemberg,Bavaria,Berlin,Brandenburg,Bremen,Hamburg,Hesse,Lower Saxony,Mecklenburg-Vorpommern,North Rhine-Westphalia,Rheinland-Pfalz,Saarland,Saxony,Saxony-Anhalt,Schleswig-Holstein,Thuringia", "Germany", corona_sel$gov)
corona_sel$gov <- ifelse(corona_sel$gov == "Auvergne-Rhone-Alpes,Bourgogne-Franche-Comte,Brittany,Centre,Corsica,French Guiana,Grand Est,Guadeloupe,Hauts-de-France,Ile-de-France,Martinique,Mayotte,Normandy,Nouvelle-Aquitaine,Occitanie,Pays de la Loire,Provence-Alpes-Cote d'Azur,Reunion", "France", corona_sel$gov)
corona_sel$gov <- ifelse(corona_sel$gov == "Aargau,Appenzell Ausserrhoden,Appenzell Innerrhoden,Basel-City,Basel-Landschaft,Bern,Fribourg,Geneva,Glarus,Grisons,Jura,Lucerne,Neuchatel,Nidwalden,Obwalden,Saint Gallen,Schaffhausen,Schwyz,Solothurn,Thurgau,Ticino,Uri,Valais,Vaud,Zug,Zurich", "Switzerland", corona_sel$gov)

corona_sel <- corona_sel[-which(duplicated(corona_sel)),]

### end of coronanet recodings

corona_sel_active <- corona_sel[, c('gov', 'date_start', 'type')]
colnames(corona_sel_active) <- c('country', 'date', 'type')
corona_sel_active$active1 <- 1

corona_sel_active$date <- as.Date(corona_sel_active$date)

base2 <- merge(base, corona_sel_active, by = c('country', 'date', 'type'), all.x = TRUE)

base2$active1 <- ifelse(is.na(base2$active1), 0, base2$active1)

colnames(corona_sel_active) <- c('province', 'date', 'type', 'active')

base3 <- merge(base2, corona_sel_active, by = c('province', 'date', 'type'), all.x = TRUE)

detach("package:plyr", unload = TRUE)
base3 = base3 %>% 
  group_by(country, date, type, province) %>%
  mutate(policy_active = ifelse(active1 == 1, 1, ifelse(active == 1, 1, 0))) %>% 
  select(-active1, -active) %>% 
  slice(1) %>%
  ungroup

base3$policy_active <- ifelse(is.na(base3$policy_active), 0, base3$policy_active)

corona_sel_inactive <- corona_sel[, c('gov', 'date_end', 'type')]
colnames(corona_sel_inactive) <- c('country', 'date', 'type')
corona_sel_inactive$inactive <- -1

library('plyr')
base4 <- merge(base3, corona_sel_inactive, by = c('country', 'date', 'type'), all.x = TRUE)

colnames(corona_sel_inactive) <- c('province', 'date', 'type', 'inactive')

base5 <- merge(base4, corona_sel_inactive, by = c('province', 'date', 'type'), all.x = TRUE)
base5$inactive.x <- ifelse(is.na(base5$inactive.x), 0, base5$inactive.x)
base5$inactive.y <- ifelse(is.na(base5$inactive.y), 0, base5$inactive.y)

detach("package:plyr", unload = TRUE)
base5 = base5 %>%
  group_by(country, date, type, province) %>%
  mutate(policy_inactive = ifelse(inactive.x == -1, -1, ifelse(inactive.y == -1, -1, 0))) %>% 
  select(-inactive.x, -inactive.y) %>% 
  slice(1) %>%
  ungroup

base5 = base5 %>% group_by(country, province, type) %>%
  mutate(policy_inactive = cumsum(policy_inactive),
  policy_active = cumsum(policy_active))

base5$policy_activity <- base5$policy_active + base5$policy_inactive

base6 <- base5 %>% 
  select(-policy_active, -policy_inactive)

colnames(subnational_data)[c(1:5)] <- c('date', 'province', 'country', 'geo', 'population')

#subnational_data <- rbind(subnational_data, subnational_data[which(subnational_data$date == "2020-01-02"),])
#subnational_data[which(subnational_data$date == "2020-01-02"),][1:81,c('date')] <- "2020-01-01"

library(reshape)
subnational_data$date <- as.Date(subnational_data$date)
base6$date <- as.Date(base6$date)
base6 <- as.data.frame(base6)

library('plyr')
base_cases <- merge(base6, subnational_data, by = c('country', 'province', 'date'), x.all = TRUE)

base_cases$past_average_new_cases <- ifelse(is.na(base_cases$past_average_new_cases), 0, base_cases$past_average_new_cases)

base_cases <- base_cases %>%
  group_by(country, date, type, province) %>%
  slice(1) %>%
  ungroup

detach("package:plyr", unload = TRUE)
base_cases2 <- base_cases %>%
  group_by(country, date, type) %>%
  mutate(past_average_national = sum(as.numeric(past_average_new_cases)))

grouping_col <- base_cases[,c('country', 'date', 'type')]
grouping_col$past_average_national_new_cases <- ave(as.numeric(base_cases$past_average_new_cases),grouping_col,FUN=sum)
grouping_col <- grouping_col %>%
  group_by(country, date, type) %>%
 slice(1) %>%
  ungroup

base_cases2 <- merge(base_cases, grouping_col, x.all = TRUE)

#I divide the dataset by policy type

#Lockdown

base_lockdown <- base_cases2[which(base_cases2$type == 'Lockdown'),]

base_lockdown$policy_active <- ifelse(base_lockdown$policy_activity > 0, 1, 0)

hetero_lockdown <- base_lockdown %>%
  group_by(country, date) %>%
  mutate(hetero = -1*(abs((sum(policy_active)/length(policy_active)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_lockdown, aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

# Model specification (national level)

hetero_lockdown <- hetero_lockdown %>%
  group_by(country, date) %>%
  slice(1) %>%
  ungroup

hhi_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/Measures/hhi.csv', sep=",")
library(readr)
#merged <- read_csv("~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/merged_final.csv", guess_max = 10000)

national_data_lockdown <- merge(hetero_lockdown, hhi_data[,-1], by = c('country', 'date'))

national_data_lockdown$hhi_cumulative <- as.numeric(national_data_lockdown$hhi_cumulative)
national_data_lockdown$hhi_new <- as.numeric(national_data_lockdown$hhi_new)

summary(h2.a <- lm(hetero ~ as.factor(country) + poly(as.Date(date), 3), data = national_data_lockdown))
summary(h2.b <- lm(hetero ~ as.factor(country) + hhi_new + I(past_average_national_new_cases/sum_pop) + poly(as.Date(date), 3),
                   data = national_data_lockdown))

stargazer(h2.a, h2.b, digits = 2)

#################
#################
#Restrictions of mass gatherings

base_mass <- base_cases2[which(base_cases2$type == 'Restrictions of Mass Gatherings'),]

base_mass$policy_active <- ifelse(base_mass$policy_activity > 0, 1, 0)

hetero_mass <- base_mass %>%
  group_by(country, date) %>%
  mutate(hetero = -1*(abs((sum(policy_active)/length(policy_active)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_mass, aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()


# Model specification (national level)

hetero_mass <- hetero_mass %>%
  group_by(country, date) %>%
  slice(1) %>%
  ungroup

#merged <- read_csv("~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/merged_final.csv", guess_max = 10000)

national_data_mass <- merge(hetero_mass, hhi_data[,-1], by = c('country', 'date'))

national_data_mass$hhi_cumulative <- as.numeric(national_data_massn$hhi_cumulative)
national_data_mass$hhi_new <- as.numeric(national_data_mass$hhi_new)

summary(m2.a <- lm(hetero ~ as.factor(country) + poly(as.Date(date), 3), data = national_data_mass))
summary(m2.b <- lm(hetero ~ as.factor(country) + hhi_new + I(past_average_national_new_cases/sum_pop) + poly(as.Date(date), 3),
                   data = national_data_mass))

stargazer(m2.a, m2.b, digits = 2)

###############
###############
#Policy subtypes
###############
###############

#School closures

#Create a base dataset based on policy types
province <- unique(subnational_data$province)
subtype <- unique(corona$type_sub_cat)
date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(province = province,
                    date = date,
                    subtype = subtype
)
base$country <- c(rep("France", 18), rep("Germany", 16), rep("Italy", 20), rep("Switzerland", 26))
base <- base[, c('country', 'province', 'date', 'subtype')]

corona_sel_active <- corona_sel[, c('gov', 'date_start', 'type_sub_cat')]
colnames(corona_sel_active) <- c('country', 'date', 'subtype')
corona_sel_active$active1 <- 1

corona_sel_active$date <- as.Date(corona_sel_active$date)

base2 <- merge(base, corona_sel_active, by = c('country', 'date', 'subtype'), all.x = TRUE)

base2$active1 <- ifelse(is.na(base2$active1), 0, base2$active1)

colnames(corona_sel_active) <- c('province', 'date', 'subtype', 'active')

base3 <- merge(base2, corona_sel_active, by = c('province', 'date', 'subtype'), all.x = TRUE)

detach("package:plyr", unload = TRUE)
base3 = base3 %>% 
  group_by(country, date, subtype, province) %>%
  mutate(policy_active = ifelse(active1 == 1, 1, ifelse(active == 1, 1, 0))) %>% 
  select(-active1, -active) %>% 
  slice(1) %>%
  ungroup

base3$policy_active <- ifelse(is.na(base3$policy_active), 0, base3$policy_active)

corona_sel_inactive <- corona_sel[, c('gov', 'date_end', 'type_sub_cat')]
colnames(corona_sel_inactive) <- c('country', 'date', 'subtype')
corona_sel_inactive$inactive <- -1

library('plyr')
base4 <- merge(base3, corona_sel_inactive, by = c('country', 'date', 'subtype'), all.x = TRUE)

colnames(corona_sel_inactive) <- c('province', 'date', 'subtype', 'inactive')

base5 <- merge(base4, corona_sel_inactive, by = c('province', 'date', 'subtype'), all.x = TRUE)
base5$inactive.x <- ifelse(is.na(base5$inactive.x), 0, base5$inactive.x)
base5$inactive.y <- ifelse(is.na(base5$inactive.y), 0, base5$inactive.y)

detach("package:plyr", unload = TRUE)
base5 = base5 %>%
  group_by(country, date, subtype, province) %>%
  mutate(policy_inactive = ifelse(inactive.x == -1, -1, ifelse(inactive.y == -1, -1, 0))) %>% 
  select(-inactive.x, -inactive.y) %>% 
  slice(1) %>%
  ungroup

base5 = base5 %>% group_by(country, province, subtype) %>%
  mutate(policy_inactive = cumsum(policy_inactive),
         policy_active = cumsum(policy_active))

base5$policy_activity <- base5$policy_active + base5$policy_inactive

base6 <- base5 %>% 
  select(-policy_active, -policy_inactive)

colnames(subnational_data)[c(1:5)] <- c('date', 'province', 'country', 'geo', 'population')

#subnational_data <- rbind(subnational_data, subnational_data[which(subnational_data$date == "2020-01-02"),])
#subnational_data[which(subnational_data$date == "2020-01-02"),][1:81,c('date')] <- "2020-01-01"

library(reshape)
subnational_data$date <- as.Date(subnational_data$date)
base6$date <- as.Date(base6$date)
base6 <- as.data.frame(base6)

library('plyr')
base_cases <- merge(base6, subnational_data, by = c('country', 'province', 'date'), x.all = TRUE)

base_cases$past_average_new_cases <- ifelse(is.na(base_cases$past_average_new_cases), 0, base_cases$past_average_new_cases)

base_cases <- base_cases %>%
  group_by(country, date, subtype, province) %>%
  slice(1) %>%
  ungroup

detach("package:plyr", unload = TRUE)
base_cases2 <- base_cases %>%
  group_by(country, date, subtype) %>%
  mutate(past_average_national = sum(as.numeric(past_average_new_cases)))

grouping_col <- base_cases[,c('country', 'date', 'subtype')]
grouping_col$past_average_national_new_cases <- ave(as.numeric(base_cases$past_average_new_cases),grouping_col,FUN=sum)
grouping_col <- grouping_col %>%
  group_by(country, date, subtype) %>%
  slice(1) %>%
  ungroup

base_cases2 <- merge(base_cases, grouping_col, x.all = TRUE)

#Closure of Schools

base_school <- base_cases2[which(base_cases2$subtype == 'Preschool or childcare facilities (generally for children ages 5 and below)' |
                                 base_cases2$subtype == 'Primary Schools (generally for children ages 10 and below)' |
                                 base_cases2$subtype == 'Secondary Schools (generally for children ages 10 to 18)'),]

base_school$policy_active <- ifelse(base_school$policy_activity > 0, 1, 0)

hetero_school <- base_school %>%
  group_by(country, date, subtype) %>%
  mutate(hetero = -1*(abs((sum(policy_active)/length(policy_active)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_school[hetero_school$subtype == 'Preschool or childcare facilities (generally for children ages 5 and below)',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

ggplot(hetero_school[hetero_school$subtype == 'Primary Schools (generally for children ages 10 and below)',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

ggplot(hetero_school[hetero_school$subtype == 'Secondary Schools (generally for children ages 10 to 18)',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

hetero_school2 <- hetero_school %>%
  group_by(country, date) %>%
  mutate(hetero_mean = mean(hetero)) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_school2, aes(x = date, y = hetero_mean)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

# Model specification (national level)

#merged <- read_csv("~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/merged_final.csv", guess_max = 10000)

national_data_school <- merge(hetero_school2, hhi_data[,-1], by = c('country', 'date'))

national_data_school$hhi_cumulative <- as.numeric(national_data_school$hhi_cumulative)
national_data_school$hhi_new <- as.numeric(national_data_school$hhi_new)

summary(s2.a <- lm(hetero ~ as.factor(country) + poly(as.Date(date), 3), data = national_data_school))
summary(s2.b <- lm(hetero ~ as.factor(country) + hhi_new + I(past_average_national_new_cases/sum_pop) + poly(as.Date(date), 3),
                   data = national_data_school))

stargazer(s2.a, s2.b, digits = 2)


#Mask-wearing policies

base_mask <- base_cases2[which(base_cases2$subtype == 'Other Mask Wearing Policy' |
                                   base_cases2$subtype == 'Wearing Masks in all public spaces/everywhere' |
                                   base_cases2$subtype == 'Wearing Masks inside public or commercial building'),]

base_mask$policy_active <- ifelse(base_mask$policy_activity > 0, 1, 0)

hetero_mask <- base_mask %>%
  group_by(country, date, subtype) %>%
  mutate(hetero = -1*(abs((sum(policy_active)/length(policy_active)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_mask[hetero_mask$subtype == 'Other Mask Wearing Policy',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

ggplot(hetero_mask[hetero_mask$subtype == 'Wearing Masks in all public spaces/everywhere',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

ggplot(hetero_mask[hetero_mask$subtype == 'Wearing Masks inside public or commercial building',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

hetero_mask2 <- hetero_mask %>%
  group_by(country, date) %>%
  mutate(hetero_mean = mean(hetero)) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_mask2, aes(x = date, y = hetero_mean)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()


#All analyses

hetero_lockdown$hetero_mean <- hetero_lockdown$hetero
hetero_mass$hetero_mean <- hetero_mass$hetero
colnames(hetero_school2)[3] <- "type"
colnames(hetero_mask2)[3] <- "type"

colnames(hetero_lockdown)
colnames(hetero_mass)
colnames(hetero_school2)

hetero_all <- rbind(hetero_lockdown, hetero_mass, hetero_school2, hetero_mask2)

hetero_all$differentiating <- ifelse(hetero_all$type == "Lockdown", 1, ifelse(hetero_all$type == "Restrictions of Mass Gatherings", 1, 0))

##All analyses

hetero_all <- merge(hetero_all, hhi_data[,-1], by = c('country', 'date'))

hetero_all$hhi_cumulative <- as.numeric(hetero_all$hhi_cumulative)
hetero_all$hhi_new <- as.numeric(hetero_all$hhi_new)

summary(all.a <- lm(hetero_mean ~ as.factor(country) + differentiating + poly(as.Date(date), 3), data = hetero_all))
summary(all.b <- lm(hetero_mean ~ as.factor(country) + differentiating + hhi_new + I(past_average_national_new_cases/sum_pop) + poly(as.Date(date), 3),
                   data = hetero_all))

summary(all.c <- lm(hetero_mean ~ as.factor(country)*differentiating + poly(as.Date(date), 3), data = hetero_all))
summary(all.d <- lm(hetero_mean ~ as.factor(country)*differentiating + hhi_new + I(past_average_national_new_cases/sum_pop) + poly(as.Date(date), 3),
                    data = hetero_all))

stargazer(s2.a, s2.b, digits = 2)

