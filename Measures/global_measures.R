##Load dataset##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

corona <- read.csv("https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv",
                   sep = ",", stringsAsFactors = FALSE)[-c(1:2),]#round 1-main survey

corona_national <- corona[which(corona$country == corona$target_country &
               corona$init_country_level == "National" &
                 (is.na(corona$target_geog_level) | corona$target_geog_level == "One or more countries, but not all countries") &
                     is.na(corona$target_province)),]

first_policy <- corona_national %>%
  group_by(country, type) %>%
  select(country, init_country_level, target_geog_level, type, type_sub_cat, date_start, compliance, enforcer) %>%
  arrange(country, type, date_start) %>%
  slice(1) %>%
  ungroup

first_policy #first nation-wide policies implemented in a country by country, type

####

country <- unique(corona$country)
type <- unique(corona$type)
date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(country = country, 
                    date = date,
                    type = type
)

base_first <- merge(base, first_policy, by= c('country', 'type'), all.x = TRUE)

base_first <- base_first %>%
  arrange(country, date, type)

today <- format(Sys.Date(), format="%Y-%m-%d")

#create variable to indicate whether the policy implementation has ever been implemented (status)

base_first$status <- ifelse(is.na(base_first$date_start), 1, 2) #if observation is censored, then 1, if dead, then 2

#arrange covid data to create time variables

covid <- as.data.frame(read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
                                sep = ",", dec='.', header = TRUE, stringsAsFactors = FALSE))

covid_case1_date <- covid[which(covid$total_cases >= 1),] %>%
  group_by(location) %>%
  select(location, date, population, population_density, median_age, gdp_per_capita) %>%
  slice(1)

covid_case10_date <- covid[which(covid$total_cases >= 10),] %>%
  group_by(location) %>%
  select(location, date) %>%
  slice(1)

covid_case100_date <- covid[which(covid$total_cases >= 100),] %>%
  group_by(location) %>%
  select(location, date) %>%
  slice(1)

covid_death1_date <- covid[which(covid$total_deaths >= 1),] %>%
  group_by(location) %>%
  select(location, date) %>%
  slice(1)

covid_death10_date <- covid[which(covid$total_deaths >= 10),] %>%
  group_by(location) %>%
  select(location, date) %>%
  slice(1)

covid_death100_date <- covid[which(covid$total_deaths >= 100),] %>%
  group_by(location) %>%
  select(location, date) %>%
  slice(1)

covid_all <- merge(merge(merge(merge(merge(covid_case1_date, covid_case10_date, by = 'location', all.x = T), covid_case100_date, by = 'location', all.x = T),
                               covid_death1_date, by = 'location', all.x = T), covid_death10_date, by = 'location', all.x = T), covid_death100_date, by = 'location', all.x = T)
colnames(covid_all) <- c('country', 'case1', 'population', 'population_density', 'median_age', 'gdp_per_capita',
                         'case10', 'case100', 'death1', 'death10', 'death100')
covid_all <- covid_all[, c('country', 'case1', 'case10', 'case100', 'death1', 'death10', 'death100',
                           'population', 'population_density', 'median_age', 'gdp_per_capita')]

covid_all[,3][which(is.na(covid_all[,3]))] <- today #substitute values with today's date
covid_all[,4][which(is.na(covid_all[,4]))] <- today 
covid_all[,5][which(is.na(covid_all[,5]))] <- today 
covid_all[,6][which(is.na(covid_all[,6]))] <- today 
covid_all[,7][which(is.na(covid_all[,7]))] <- today 

#Change country labels for merging

base_first$country <- as.character(base_first$country)
covid_all$country[covid_all$country == 'United States'] <-
  as.character(base_first$country [base_first$country == 'United States of America'][1])

covid_all$country[covid_all$country == 'Democratic Republic of Congo'] <-
  as.character(base_first$country [base_first$country == 'Democratic Republic of the Congo'][1])

covid_all$country[covid_all$country == 'Congo'] <-
  as.character(base_first$country [base_first$country == 'Republic of Congo'][1])

covid_all$country[covid_all$country == 'Tanzania'] <-
  as.character(base_first$country [base_first$country == 'United Republic of Tanzania'][1])

covid_all$country[covid_all$country == 'Guinea-Bissau'] <-
  as.character(base_first$country [base_first$country == 'Guinea Bissau'][1])

covid_all$country[covid_all$country == 'Serbia'] <-
  as.character(base_first$country [base_first$country == 'Republic of Serbia'][1])

covid_all$country[covid_all$country == "Cote d'Ivoire"] <-
  as.character(base_first$country [base_first$country == 'Ivory Coast'][1])

covid_all$country[covid_all$country == "Bahamas"] <-
  as.character(base_first$country [base_first$country == 'The Bahamas'][1])

base <- merge(base_first, covid_all, by = 'country', all.x = T) %>%
  arrange(country, date, type)

#create variable to count days of policy implementation since January 1st

base$policy_since_jan1 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$date_start, 1, 0
  ),1)

#create variable to count days of policy implementation since the first infected case in the country

base$policy_since_case1 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$case1 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$case1, 1, 0
  ))

base$policy_since_case10 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$case10 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$case10, 1, 0
  ))

base$policy_since_case100 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$case100 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$case100, 1, 0
  ))

base$policy_since_death1 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$death1 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$death1, 1, 0
  ))

base$policy_since_death10 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$death10 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$death10, 1, 0
  ))

base$policy_since_death100 <- 
  ifelse(!is.na(base$date_start), ifelse(
    base$date < base$death100 | base$date > base$date_start, 0, 1
  ),ifelse(
    base$date > base$death100, 1, 0
  ))

#create time variables

surv_data <- base %>%
  group_by(country, type) %>%
  filter(!country == 'European Union') %>%
  mutate(time_jan1 = sum(policy_since_jan1),
         time_case1 = sum(policy_since_case1),
         time_case10 = sum(policy_since_case10),
         time_case100 = sum(policy_since_case100),
         time_death1 = sum(policy_since_death1),
         time_death10 = sum(policy_since_death10),
         time_death100 = sum(policy_since_death100)) %>%
  slice(1) %>%
  ungroup

timing_final <- surv_data %>%
  select(country, type, date_start, status,
         case1, case10, case100, death1, death10, death100,
         time_jan1, time_case1, time_case10, time_case100,
         time_death1, time_death10, time_death100,
         population, population_density, median_age, gdp_per_capita)

write.csv2(timing_final, 'CoronaNet_time.csv')

#####################
#####################

first_policy <- corona %>%
  group_by(country, type, type_sub_cat) %>%
  select(country, init_country_level, target_geog_level, type, type_sub_cat, date_start, compliance, enforcer) %>%
  arrange(country, type, type_sub_cat, date_start) %>%
  slice(1) %>%
  ungroup

first_policy$type_all <- ifelse(is.na(first_policy$type_sub_cat), first_policy$type, first_policy$type_sub_cat)

country <- unique(corona$country)
type_all <- unique(first_policy$type_all)
date_start <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(country = country, 
                    date = date,
                    type_all = type_all
)
base <- base %>%
  arrange(country, date, type_all)

base_first <- merge(base, first_policy[, c('country', 'type_all', 'date_start')], by= c('country', 'type_all'), all.x = TRUE)

base_first <- base_first %>%
  group_by(country, date, type_all) %>%
  arrange(country, date, type_all) %>%
  slice(1) %>%
  ungroup

today <- format(Sys.Date(), format="%Y-%m-%d")

base_first$policy_after <- ifelse(is.na(base_first$date_start), 0, ifelse(base_first$date < base_first$date_start, 0, 1))

base_first <- base_first %>%
  group_by(country, date) %>%
  mutate(n_policies = sum(policy_after))

diversity_policies <- base_first %>%
  group_by(country, date) %>%
  select(country, date, n_policies) %>%
  slice(1) %>%
  ungroup

write.csv2(diversity_policies, 'CoronaNet_diversity.csv')

########
########
# Province policies for France

corona_france <- corona[which(corona$country == "France"),]


country_fr <- 'France'
province_fr <- c('Auvergne Rhone-Alpes', 'Bourgogne Franche-Comte', 'Brittany', 'Centre',
                                  'Corsica', 'Grand Est', 'Hauts-de-France',
                                  'Ile-de-France', 'Normandy', 'Nouvelle-Aquitaine',
                                  'Occitanie', 'Pays de la Loire', "Provence-Alps-Cote d'Azur")
type_fr <- unique(corona$type)[-20]

date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base_fr <- expand.grid(country = country_fr,
                    province = province_fr,
                    date = date,
                    type = type_fr
)

#covid cases in France #data on covid by regions is incomplete in the wiki, we need to find more up-to-date data to do this

covid_france <- as.data.frame(read.table("covid_france.txt", sep = ",", col.names = c("Date", "ARA","BFC","BRE","CVL","COR","GES","HDF","IDF","NOR","NAQ"
,"OCC","PDL","PAC","LRE","MF","BL","MQ","GUA","MAY","GF","New","Total","New","Total")))
covid_france <- covid_france[1:51,]
covid_france <- as.data.frame(apply(covid_france, 2, function(x) gsub("^$|^ $", 0, x)))

covid_france$Date <- as.Date(format(covid_france$Date, format="%Y-%m-%d"))
