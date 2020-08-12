
##########################################################
#Theoretical measures of subnational policy fragmentation#
#             by JOAN                                    #
##########################################################

###############
##Subnational##
###############

#Policy heterogeneity within a country

#A first measure we could use to characterize heterogeneity in subnational policy could be a simple formula that
#accounts for the deviation in the proportion of policies from the maximum unevenness in the implementation of the policies.

#For a binary policy i, the maximum level of unevenness happens when 50% of the states implement policy i, and the other half do not
#while very simple, a simple formula could get this:

data = data.frame(prov =c("state a","state b","state c","state d","state e","state f","state g","state h", "state i"),
                          policy_A = c(0,0,0,0,0,0,0,0,0),
                          policy_B = c(1,0,0,0,0,0,0,0,0),
                          policy_C = c(1,1,0,0,0,0,0,0,0),
                          policy_D = c(1,1,1,0,0,0,0,0,0),
                          policy_E = c(1,1,1,1,0,0,0,0,0),
                          policy_F = c(1,1,1,1,1,0,0,0,0),
                          policy_G = c(1,1,1,1,1,1,0,0,0),
                          policy_H = c(1,1,1,1,1,1,1,0,0),
                          policy_I = c(1,1,1,1,1,1,1,1,0),
                          policy_J = c(1,1,1,1,1,1,1,1,1))

prop.policies <- NULL
fragmentation_policies <- NULL
for (i in 2:ncol(data)){
  prop.policies[i-1] <- sum(data[,i])/length(data[,i])
  fragmentation_policies[i-1] <- -1*(abs((prop.policies[i-1]-0.5)*2))+1
}

plot(prop.policies, fragmentation_policies)
mean(fragmentation_policies) #mean policy heterogeneity within a country

# maximum heterogeneity --> 50% of the states implement policy i
# minimum heterogeneity --> 0% or 100% of the states implement policy i

################################################
####A Measure of Policy Agreement across Regions
################################################

# A measure that could take into account the categories of the policies that are implemented across the regions may work
# by creating a vector of policies for each region and, then, compute the Krippendorff's alpha coefficient for multiple raters.
# is a statistical measure of the agreement achieved when coding a set of units of analysis in terms of the values of a variable.
# This measure could not only take into account the *number* but also what *policies* are similarly implemented across different regions

kripp.alpha(t(data[,c(2,6)]), method=c("nominal"))

kripp.alpha(t(data[,c(2,3)]), method=c("nominal"))
kripp.alpha(t(data[,c(2,4)]), method=c("nominal"))
kripp.alpha(t(data[,c(2,5)]), method=c("nominal"))
kripp.alpha(t(data[,c(2,6)]), method=c("nominal"))
kripp.alpha(t(data[,c(2,7)]), method=c("nominal"))
kripp.alpha(t(data[,c(2,8)]), method=c("nominal"))
kripp.alpha(t(data[,c(2:9)]), method=c("nominal"))


#############################################


#######################################
#######################################

#Policy heterogeneity within a country

#Based on the number of policies, we could create a measure of the diversity in the number of policies across regions,
#and the evenness in the number of policies across regions
#The big limitation is that it would not take into account if regions have different policies but the same number of policies

#######################################

#For this, we could use the Shannon-Wiener Index, H=∑[(pi)×ln(pi)]:

#For a country with 8 regions

library(RAM)

data = data.frame(prov =c("state a","state b","state c","state d","state e","state f","state g","state h", "state i"),
                  diversity1 = c(2,3,5,20,1,0,4,0,0),
                  diversity2 = c(5,5,5,5,5,5,5,5,5),
                  diversity3 = c(1,1,1,1,1,1,1,1,1))

shannon <- function(vector){
  s <- sum(vector)
  prop <- lprop <- prod <- NULL
  for (i in 1:length(vector)){
    prop[i] <- vector[i]/s
    lprop[i] <- log(prop[i])
  }
  lprop <- ifelse(is.infinite(lprop), 0, lprop)
  for (i in 1:length(vector)){
    prod[i] <- abs(prop[i]*lprop[i])
  }
  hmax <- log(length(vector))
  h <- sum(prod)
  evenness <- h/length(vector)
  print(h)
  print(evenness)
}


#############################################
### Measures of Adaptation to the COVID-19###
#############################################

covid_cases <- c(10,15,20,30,50,100)

covid_data <- data.frame(day =c("1","2","3","4","5","6","7","8","9","10"),
                         covid_a = c(covid_cases[1], rep(0,9)),
                         covid_b = c(covid_cases[2], rep(0,9)),
                         covid_c = c(covid_cases[3], rep(0,9)),
                         covid_d = c(covid_cases[4], rep(0,9)),
                         covid_e = c(covid_cases[5], rep(0,9)),
                         covid_f = c(covid_cases[6], rep(0,9))
)

for (i in 2:nrow(covid_data)){
  for (j in 2:ncol(covid_data)){
    covid_data[i,j] <- round(covid_data[i-1,j]^1.10)
  }
}
covid_data$covid_national <- rowSums(covid_data[,-1])

policies <- c(rep(0,8),1)
data <- data.frame(day =c("1","2","3","4","5","6","7","8","9","10"),
                   national = c(0, sample(policies)),
                   state_a_policy_random = c(0, sample(policies)),
                   state_b_policy_random = c(0, sample(policies)),
                   state_c_policy_random = c(0, sample(policies)),
                   state_d_policy_random = c(0, sample(policies)),
                   state_e_policy_random = c(0, sample(policies)),
                   state_f_policy_random = c(0, sample(policies)))

for (i in 1:nrow(data)){
  for (j in 2:ncol(data)){
    if(data[i,j] == 1){
      data[i:nrow(data),j] <- 1
    }
  }
}

data_all <- merge(covid_data, data, by="day")
data_all$day <-  as.numeric(data_all$day)

data_all <- data_all %>% arrange(day)
data_all

for (i in 1:nrow(data_all)){
  data_all$state_a_policy150[i] <- ifelse(data_all$covid_a[i] > 150, 1, 0)
  data_all$state_b_policy150[i] <- ifelse(data_all$covid_b[i] > 150, 1, 0)
  data_all$state_c_policy150[i] <- ifelse(data_all$covid_c[i] > 150, 1, 0)
  data_all$state_d_policy150[i] <- ifelse(data_all$covid_d[i] > 150, 1, 0)
  data_all$state_e_policy150[i] <- ifelse(data_all$covid_e[i] > 150, 1, 0)
  data_all$state_f_policy150[i] <- ifelse(data_all$covid_f[i] > 150, 1, 0)
  data_all$national_policy150[i] <- ifelse(data_all$covid_national[i] > 150*6, 1, 0)
}

#Adaptation measure by policy type: random timing case

state_random = national_tolerance = state_policy150 <- NA

for (i in 2:nrow(data_all)){
  for (j in 2:7){
    if(data_all[i,j+8] == 1 & data_all[i-1,j+8] == 0){
      state_random[j-1] <- data_all[i,j]
    }else{}
    
    if(data_all[i,j+14] == 1 & data_all[i-1,j+14] == 0){
      state_policy150[j-1] <- data_all[i,j]
    }else{}
    
    if(data_all[i,c('national')] == 1 & data_all[i-1,c('national')] == 0){
      national_tolerance[j-1] <- data_all[i,j]
    }else{}
}
}

data_all[,c(1:8,16:22)]

timing <- data.frame(state = c('a', 'b', 'c', 'd', 'e', 'f'),
           state_random = state_random,
           state_policy150 = state_policy150,
           national_tolerance = national_tolerance)


#mean(timing$state_random)
#sd(timing$state_random)

mean(timing$state_policy150)
sd(timing$state_policy150)

mean(timing$national_tolerance)
sd(timing$national_tolerance)


##########################################################
# CoronaNet measures of subnational policy fragmentation #
#             by JOAN                                    #
##########################################################

#Load subnational data

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
dat = read.csv("RKI_COVID19.csv")

#germanydata = fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
#datatesterr = germanydata$name

#germany_data_tester1 <- germanydata %>%
#  group_by(Meldedatum, Bundesland, IdBundesland ) %>%
#  select(IdBundesland, Bundesland, Meldedatum, AnzahlFall) %>%
#  summarise(cases = sum(AnzahlFall)) %>%
#  slice(1) %>%
#  ungroup

dat <- dat[, c('IdBundesland', 'Bundesland', 'Meldedatum', 'AnzahlFall')]

germany_data_tester1 <- dat %>%
  group_by(IdBundesland, Bundesland, Meldedatum) %>%
  mutate(cases = sum(AnzahlFall)) %>%
  select(Bundesland, Meldedatum, AnzahlFall) %>%
  slice(1) %>%
  ungroup

#write.csv(germany_data_tester1, "germanydata.csv")

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

italy_dat <- read.csv("https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv",
                      sep = ",", stringsAsFactors = FALSE)[-c(1:2),]


italy_data_tester = data.frame(
  date = italy_dat$data,
  country = italy_dat$stato,
  region = italy_dat$denominazione_regione,
  cases = italy_dat$totale_positivi,
  deceased = italy_dat$deceduti
)

#write.csv(italy_data_tester, "italydata.csv")

france_dat <- read.csv("https://www.data.gouv.fr/fr/datasets/r/0b66ca39-1623-4d9c-83ad-5434b7f9e2a4",
                       sep = ",", stringsAsFactors = FALSE)[-c(1:2),]

regionfrance = france_dat[france_dat$granularite == "region",]

francedata = ddply(regionfrance, .(date, maille_nom), summarise, 
                   cases = sum(cas_confirmes),
                   deaths = sum(deces))

#write.csv(francedata, "francedata.csv")


switzerland_dat <- read.csv("https://github.com/openZH/covid_19/raw/master/COVID19_Fallzahlen_CH_total_v2.csv",
                            sep = ",", stringsAsFactors = FALSE)[-c(1:2),]

switzerlanddata = data.frame(
  date = switzerland_dat$date,
  canton = switzerland_dat$abbreviation_canton_and_fl,
  cases = switzerland_dat$ncumul_conf,
  deaths = switzerland_dat$ncumul_deceased
)

#write.csv(switzerlanddata, "switzerlanddata.csv")


##Load Luca's subnational data

#subnational_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/Cases/combined_cases.csv', sep=",")

subnational_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/Cases/cases.csv', sep=",")

#Load CoronaNet (latest version)

##Province names to match CoronaNET: unique(corona[which(corona$country == "Switzerland"),]$province)

subnational_data[which(subnational_data$region == 'BE'),]$region <- 'Bern'
subnational_data$region[which(subnational_data$region == 'ZH')] <- 'Zurich'
subnational_data$region[which(subnational_data$region == 'LU')] <- 'Lucerne'
subnational_data$region[which(subnational_data$region == 'UR')] <- 'Uri'
subnational_data$region[which(subnational_data$region == 'SZ')] <- 'Schwyz'
subnational_data$region[which(subnational_data$region == 'OW')] <- 'Obwalden'
subnational_data$region[which(subnational_data$region == 'NW')] <- 'Nidwalden'
subnational_data$region[which(subnational_data$region == 'GL')] <- 'Glarus'
subnational_data$region[which(subnational_data$region == 'ZG')] <- 'Zug'
subnational_data$region[which(subnational_data$region == 'FR')] <- 'Fribourg'
subnational_data$region[which(subnational_data$region == 'SO')] <- 'Solothurn'
subnational_data$region[which(subnational_data$region == 'BS')] <- 'Basel-Stadt'
subnational_data$region[which(subnational_data$region == 'BL')] <- 'Basel-Landschaft'
subnational_data$region[which(subnational_data$region == 'SH')] <- 'Schaffhausen'
subnational_data$region[which(subnational_data$region == 'AR')] <- 'Appenzell Ausserrhoden'
subnational_data$region[which(subnational_data$region == 'AI')] <- 'Appenzell Innerrhoden'
subnational_data$region[which(subnational_data$region == 'SG')] <- 'Saint Gallen'
subnational_data$region[which(subnational_data$region == 'GR')] <- 'Grisons'
subnational_data$region[which(subnational_data$region == 'AG')] <- 'Aargau'
subnational_data$region[which(subnational_data$region == 'TG')] <- 'Thurgau'
subnational_data$region[which(subnational_data$region == 'TI')] <- 'Ticino'
subnational_data$region[which(subnational_data$region == 'VD')] <- 'Vaud'
subnational_data$region[which(subnational_data$region == 'VS')] <- 'Valais'
subnational_data$region[which(subnational_data$region == 'NE')] <- 'Neuchatel'
subnational_data$region[which(subnational_data$region == 'GE')] <- 'Geneva'
subnational_data$region[which(subnational_data$region == 'JU')] <- 'Jura'

#Create a base dataset
province <- unique(subnational_data$region)
type <- unique(corona$type)
date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(province = province,
                    date = date,
                    type = type
)
base$country <- c(rep("France", 18), rep("Germany", 16), rep("Italy", 20), rep("Switzerland", 26))
base <- base[, c('country', 'province', 'date', 'type')]

#corona <- read.csv("https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv",
#                   sep = ",", stringsAsFactors = FALSE)[-c(1:2),]

corona <- read.csv("coronanet_internal.csv",
                   sep = ",", stringsAsFactors = FALSE)

corona_sel <- corona[which(corona$target_country == "Germany" |
                             corona$target_country == "Switzerland" |
                             corona$target_country == "Italy" |
                             corona$target_country == "France"),]

corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-06-02"),] 
corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-05-11"),] 
corona_sel <- corona_sel[-which(corona_sel$target_country == 'France' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "Provincial"),]
corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "National" & corona_sel$date_start == "2020-03-05"),]
corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "National" & corona_sel$date_announced == "2020-03-09"),]
corona_sel[which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$init_country_level == "National" & corona_sel$date_start == "2020-03-10"),]$date_end <- "2020-05-04"

corona_sel <- corona_sel[-which(corona_sel$date_start > corona_sel$date_end),]
corona_sel <- corona_sel[-which(corona_sel$target_country == 'Germany' & corona_sel$type == 'Closure and Regulation of Schools' & 
                                  is.na(corona_sel$type_sub_cat)),]
#corona_sel <- corona_sel[-which(duplicated(corona_sel)),]

corona_sel <- corona_sel %>%
  group_by(target_country, type) %>%
  filter(!init_country_level == "Municipal") %>%
  select(target_country, init_country_level, target_province, target_city, type, type_sub_cat, date_start, date_end, update_type)

corona_sel <- corona_sel[which(is.na(corona_sel$target_city)),]
corona_sel <- rbind(data.frame(corona_sel), 'Lombardy' = c('Italy', 'National', 'Lombardy', NA, 'Lockdown', NA, '2020-03-08', '2020-03-10', 'new_entry'))


corona_sel = corona_sel %>% 
  mutate(gov = ifelse(init_country_level == "National" & is.na(target_province) & is.na(target_city), target_country, target_province)) %>% 
  select(-target_province)

corona_sel_active <- corona_sel[, c('gov', 'date_start', 'type')]
colnames(corona_sel_active) <- c('country', 'date', 'type')
corona_sel_active$active1 <- 1

base2 <- merge(base, corona_sel_active, by = c('country', 'date', 'type'), all.x = TRUE)

base2$active1 <- ifelse(is.na(base2$active1), 0, base2$active1)

colnames(corona_sel_active) <- c('province', 'date', 'type', 'active')

base3 <- merge(base2, corona_sel_active, by = c('province', 'date', 'type'), all.x = TRUE)

base3 = base3 %>% mutate(policy_active = ifelse(active1 == 1, 1, ifelse(active == 1, 1, 0))) %>% select(-active1, -active) 
base3$policy_active <- ifelse(is.na(base3$policy_active), 0, base3$policy_active)

corona_sel_inactive <- corona_sel[, c('gov', 'date_end', 'type')]
colnames(corona_sel_inactive) <- c('country', 'date', 'type')
corona_sel_inactive$inactive <- -1

base4 <- merge(base3, corona_sel_inactive, by = c('country', 'date', 'type'), all.x = TRUE)

colnames(corona_sel_inactive) <- c('province', 'date', 'type', 'inactive')

base5 <- merge(base4, corona_sel_inactive, by = c('province', 'date', 'type'), all.x = TRUE)
base5$inactive.x <- ifelse(is.na(base5$inactive.x), 0, base5$inactive.x)
base5$inactive.y <- ifelse(is.na(base5$inactive.y), 0, base5$inactive.y)

base5 = base5 %>% group_by(country, province, type) %>% 
                  mutate(policy_inactive = ifelse(inactive.x == -1, -1, ifelse(inactive.y == -1, -1, 0)),
                         policy_inactive = cumsum(policy_inactive),
                         policy_active = cumsum(policy_active)) %>% 
  select(-inactive.x, -inactive.y) 

base5$policy_activity <- base5$policy_active + base5$policy_inactive

base6 <- base5 %>% 
  group_by(country, province, type, date) %>% 
  select(-policy_active, -policy_inactive) %>% 
  slice(1) %>%
  ungroup

base7 <- base6 %>%
  group_by(country, province, type) %>%
  mutate(policy_active = cumsum(policy_activity)) %>%
  select(-policy_activity)

colnames(subnational_data)[1:5] <- c('date', 'province', 'cases', 'country', 'population')

subnational_data <- rbind(subnational_data, subnational_data[which(subnational_data$date == "2020-01-02"),])
subnational_data[which(subnational_data$date == "2020-01-02"),][1:80,c('date')] <- "2020-01-01"

base_cases <- merge(base6, subnational_data, by = c('country', 'province', 'date'), all.x = TRUE)

base_cases$cases <- ifelse(base_cases$date == "2020-01-01", 0, base_cases$cases) 
base_cases <- base_cases %>% fill(cases)

base_cases <- base_cases %>%
  group_by(country, date, type) %>%
  mutate(national_cases = sum(cases))

base_smallcases <- base_cases %>%
  filter(base_cases$policy_activity == 1) %>% 
  group_by(country, province, type) %>%
  slice(1) %>%
  ungroup


#I start dividing the dataset by policy type

#Lockdown

base_lockdown <- base6[which(base7$type == 'Lockdown'),]

base_lockdown$policy_active2 <- ifelse(base_lockdown$policy_activity > 0, 1, 0)

hetero_lockdown <- base_lockdown %>%
  group_by(country, date) %>%
  mutate(hetero = -1*(abs((sum(policy_active2)/length(policy_active2)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_lockdown, aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

# Model specification (national level)

hetero_data <- merge(hetero_lockdown,
                     base_cases[,c('country', 'date', 'type', 'sum_pop', 'national_cases')],
                     by = c('country', 'date', 'type'))

hetero_data <- hetero_data %>%
  group_by(country, date) %>%
  slice(1) %>%
  ungroup

hhi_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/Measures/hhi.csv', sep=",")

national_data <- merge(hetero_data, hhi_data[,-1], by = c('country', 'date'))

national_data$hhi_cumulative <- as.numeric(national_data$hhi_cumulative)

summary(lm(hetero ~ as.factor(country) + as.Date(date), data = national_data))

summary(lm(hetero ~ as.Date(date)*as.factor(country) - 1, data = hetero_data))

national_data$cases_pop <- (national_data$national_cases/national_data$sum_pop)*100000
national_data$federal <- ifelse(national_data$country == 'Germany' | national_data$country == 'Switzerland', 1, 0)

library(splines)
summary(lm(hetero ~ cases_pop + as.factor(country) + hhi_cumulative + poly(as.Date(date), 3), data = national_data))
summary(lm(hetero ~ cases_pop*as.factor(country) + hhi_cumulative*as.factor(country) + poly(as.Date(date), 3), data = national_data))
summary(lm(hetero ~ cases_pop*as.factor(country) + hhi_cumulative*as.factor(country) + bs(as.Date(date), df = 3), data = national_data))

summary(h2a <- lm(hetero ~ log(cases_pop+1) + as.factor(country) + hhi_cumulative + poly(as.Date(date), 3), data = national_data))
summary(h2b <-lm(hetero ~ log(cases_pop+1)*as.factor(country) + poly(as.Date(date), 3), data = national_data))
summary(h2c <-lm(hetero ~ log(cases_pop+1)*as.factor(country) + hhi_cumulative*as.factor(country) + poly(as.Date(date), 3), data = national_data))
#summary(h2c <-lm(hetero ~ log(cases_pop+1)*as.factor(country) + hhi_cumulative*as.factor(country) + bs(as.Date(date), df = 3), data = national_data))

stargazer(h2a, h2b, h2c, digits = 2)

#School closures

base_school <- base6[which(base6$type == 'Closure and Regulation of Schools'),]

base_school2 <- base_school %>%
  group_by(country, province, type) %>%
  mutate(policy_active = cumsum(policy_activity)) %>%
  select(-policy_activity)

base_school2$policy_active2 <- ifelse(base_school2$policy_active > 0, 1, 0)

hetero_school <- base_school2 %>%
  group_by(country, date) %>%
  mutate(hetero = -1*(abs((sum(policy_active2)/length(policy_active2)-0.5)*2))+1) %>% 
  select(-province, -policy_active, -policy_active2) %>%
  slice(1) %>%
  ungroup

ggplot(hetero_school, aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()


#Policy adaptation

base_smallcases[base_smallcases$type == "Lockdown",]

ggplot(base_smallcases[base_smallcases$country == "France" & base_smallcases$type == "Lockdown",], 
       aes(x= date, y= cases, colour="green", label=province))+
  xlim(as.Date("2020-03-07"), as.Date("2020-03-26")) +
  geom_point() +geom_text(aes(label=province),hjust=0, vjust=0) +
  theme_minimal() + 
  theme(legend.position = "none")

ggplot(base_smallcases[base_smallcases$country == "Italy" & base_smallcases$type == "Lockdown",], 
       aes(x= date, y= cases, colour="green", label=province)) +
  xlim(as.Date("2020-03-07"), as.Date("2020-03-16")) +
  geom_point() + geom_text(aes(label=province),hjust=0, vjust=0) +
  theme_minimal() + 
  theme(legend.position = "none")

ggplot(base_smallcases[base_smallcases$country == "Germany" & base_smallcases$type == "Lockdown",], 
       aes(x= date, y= cases, colour="green", label=province)) +
  xlim(as.Date("2020-03-07"), as.Date("2020-04-16")) +
  geom_point() + geom_text(aes(label=province),hjust=0, vjust=0) +
  theme_minimal() + 
  theme(legend.position = "none")

ggplot(base_smallcases[base_smallcases$country == "Switzerland" & base_smallcases$type == "Lockdown",], 
       aes(x= date, y= cases, colour="green", label=province)) +
  xlim(as.Date("2020-03-07"), as.Date("2020-04-16")) +
  geom_point() + geom_text(aes(label=province),hjust=0, vjust=0) +
  theme_minimal() + 
  theme(legend.position = "none")

