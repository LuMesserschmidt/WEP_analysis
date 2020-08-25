##########################################################
# CoronaNet measures of subnational policy fragmentation #
#             by JOAN                                    #
##########################################################

library(rstudioapi)
library(dplyr)

##Load Luca's subnational data

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

subnational_data <- read.csv2('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/Cases/cases.csv', sep=",")

corona <- readRDS('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_Aug15.RDS')
corona <- readRDS('~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_internal_sub_raw.RDS')

#corona$date_end <- as.Date(corona$date_end, "1970-01-01")

##Province names to match CoronaNET: unique(corona[which(corona$country == "Switzerland"),]$province)

subnational_data = mutate(subnational_data, 
                  province = recode(region,
                                    "Auvergne-Rhône-Alpes" = "Auvergne-Rhone-Alpes",
                                    "Bourgogne-Franche-Comté" = "Bourgogne-Franche-Comte" ,
                                    "Bretagne" = "Brittany" , 
                                    "Centre-Val de Loire" ="Centre" ,
                                    "Corse"  = "Corsica", 
                                    "Guyane" = "French Guiana", 
                                    "Île-de-France" = "Ile-de-France",
                                    "La Réunion" = "Reunion" , 
                                    "Normandie" = "Normandy" ,
                                    "Provence-Alpes-Côte d'Azur" = "Provence-Alpes-Cote d'Azur", 
                                    "Basilicata" = "Basilicate", 
                                    "Lazio" = "Latium" , 
                                    "Lombardia" = "Lombardy",
                                    "Marche"= "The Marches", 
                                    "Piemonte" = "Piedmont" , 
                                    "Puglia" =  "Apulia" , 
                                    "Sardegna" = "Sardinia" , 
                                    "Sicilia" = "Sicily" , 
                                    "Toscana" = "Tuscany", 
                                    "Valle d'Aosta" = "Aosta Valley"
                  ))

#Create a base dataset based on policy types
province <- unique(subnational_data$province)
type <- unique(corona$type)
date <- seq(as.Date("2020/1/1"), as.Date(format(Sys.Date(), format="%Y-%m-%d")), "days")

base <- expand.grid(province = province,
                    date = date,
                    type = type
)
base$country <- c(rep("France", 18), rep("Germany", 16), rep("Italy", 20), rep("Switzerland", 26))
base <- base[, c('country', 'province', 'date', 'type')]

corona_sel <- corona[which(corona$target_country == "Germany" |
                             corona$target_country == "Switzerland" |
                             corona$target_country == "Italy" |
                             corona$target_country == "France"),]


corona_sel[which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-03-09"),]$date_start <- 
  corona_sel[which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-03-09"),]$date_start - 1 

corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-06-22"),]
corona_sel <- corona_sel[-which(corona_sel$target_country == 'Italy' & corona_sel$type == 'Lockdown' & corona_sel$date_start == "2020-02-23"),]

corona_sel <- corona_sel[-which(corona_sel$target_country == 'Switzerland' & corona_sel$type == 'Lockdown' & is.na(corona_sel$province)),]

corona_sel <- corona_sel[-which(corona_sel$country == 'Germany' & corona_sel$type == 'Lockdown' &
                   corona_sel$init_country_level == "Provincial" & corona_sel$date_start == "2020-06-23"&
                   corona_sel$target_province == "North Rhine-Westphalia"),]

corona_sel <- corona_sel[-which(corona_sel$date_start > corona_sel$date_end),]

corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Primary Schools allowed to open with no conditions"),]
corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Primary Schools allowed to open with conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Secondary Schools allowed to open with conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Secondary Schools allowed to open with no conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Higher education institutions allowed to open with no conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Higher education institutions allowed to open with conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Preschool or childcare facilities allowed to open with conditions"),]
#corona_sel <- corona_sel[-which(corona_sel$type == 'Closure and Regulation of Schools' & corona_sel$school_status == "Preschool or childcare facilities allowed to open with no conditions"),]

detach("package:plyr", unload = TRUE)

corona_sel = corona_sel %>%
  mutate(gov = ifelse(init_country_level == "National" & is.na(target_province)
                      & is.na(target_city), target_country, target_province))

### end of coronanet recodings

corona_sel_active <- corona_sel[, c('gov', 'date_start', 'type')]
colnames(corona_sel_active) <- c('country', 'date', 'type')

corona_sel_active <- count(corona_sel_active, country, date, type)

corona_sel_active$date <- as.Date(corona_sel_active$date)

base2 <- merge(base, corona_sel_active, by = c('country', 'date', 'type'), all.x = TRUE)
colnames(base2)[5] <- "active1"

base2$active1 <- ifelse(is.na(base2$active1), 0, base2$active1)

colnames(corona_sel_active) <- c('province', 'date', 'type', 'active')

base3 <- merge(base2, corona_sel_active, by = c('province', 'date', 'type'), all.x = TRUE)

base3$active <- ifelse(is.na(base3$active), 0, base3$active)

detach("package:plyr", unload = TRUE)
base3 = base3 %>% 
  group_by(country, date, type, province) %>%
  mutate(policy_active = active1 + active) %>% 
  select(-active1, -active) %>% 
  slice(1) %>%
  ungroup

base3$policy_active <- ifelse(is.na(base3$policy_active), 0, base3$policy_active)

corona_sel_inactive <- corona_sel[, c('gov', 'date_end', 'type')]
colnames(corona_sel_inactive) <- c('country', 'date', 'type')

corona_sel_inactive <- count(corona_sel_inactive, country, date, type)
corona_sel_inactive$n <- -1*corona_sel_inactive$n

library('plyr')
base4 <- merge(base3, corona_sel_inactive, by = c('country', 'date', 'type'), all.x = TRUE)
colnames(base4)[6] <- "inactive1"

colnames(corona_sel_inactive) <- c('province', 'date', 'type', 'inactive')

base5 <- merge(base4, corona_sel_inactive, by = c('province', 'date', 'type'), all.x = TRUE)
base5$inactive1 <- ifelse(is.na(base5$inactive1), 0, base5$inactive1)
base5$inactive <- ifelse(is.na(base5$inactive), 0, base5$inactive)
base5$policy_inactive <- base5$inactive1 + base5$inactive

base5 = base5 %>% group_by(country, province, type) %>%
  mutate(policy_inactive = cumsum(policy_inactive),
         policy_active = cumsum(policy_active)) %>%
  select(-inactive1, -inactive)

base5$policy_activity <- base5$policy_active + base5$policy_inactive

base6 <- base5 %>% 
  select(-policy_active, -policy_inactive)

library('plyr')
base_cases <- merge(base6, subnational_data[,c(-2)], by = c('country', 'province', 'date'), x.all = TRUE)

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

ggplot(hetero_lockdown[hetero_lockdown$date < "2020-07-22",], aes(x = date, y = hetero)) + 
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

ggplot(hetero_mass[hetero_mass$date < "2020-07-22",], aes(x = date, y = hetero)) + 
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

corona_sel_active <- count(corona_sel_active, country, date, subtype)

corona_sel_active$date <- as.Date(corona_sel_active$date)

base2 <- merge(base, corona_sel_active, by = c('country', 'date', 'subtype'), all.x = TRUE)
colnames(base2)[5] <- "active1"

base2$active1 <- ifelse(is.na(base2$active1), 0, base2$active1)

colnames(corona_sel_active) <- c('province', 'date', 'subtype', 'active')

base3 <- merge(base2, corona_sel_active, by = c('province', 'date', 'subtype'), all.x = TRUE)

base3$active <- ifelse(is.na(base3$active), 0, base3$active)

detach("package:plyr", unload = TRUE)
base3 = base3 %>% 
  group_by(country, date, subtype, province) %>%
  mutate(policy_active = active1 + active) %>% 
  select(-active1, -active) %>% 
  slice(1) %>%
  ungroup

base3$policy_active <- ifelse(is.na(base3$policy_active), 0, base3$policy_active)

corona_sel_inactive <- corona_sel[, c('gov', 'date_end', 'type_sub_cat')]
colnames(corona_sel_inactive) <- c('country', 'date', 'subtype')

corona_sel_inactive <- count(corona_sel_inactive, country, date, subtype)
corona_sel_inactive$n <- -1*corona_sel_inactive$n

library('plyr')
base4 <- merge(base3, corona_sel_inactive, by = c('country', 'date', 'subtype'), all.x = TRUE)
colnames(base4)[6] <- "inactive1"

colnames(corona_sel_inactive) <- c('province', 'date', 'subtype', 'inactive')

base5 <- merge(base4, corona_sel_inactive, by = c('province', 'date', 'subtype'), all.x = TRUE)
base5$inactive1 <- ifelse(is.na(base5$inactive1), 0, base5$inactive1)
base5$inactive <- ifelse(is.na(base5$inactive), 0, base5$inactive)
base5$policy_inactive <- base5$inactive1 + base5$inactive

base5 = base5 %>% group_by(country, province, subtype) %>%
  mutate(policy_inactive = cumsum(policy_inactive),
         policy_active = cumsum(policy_active)) %>%
  select(-inactive1, -inactive)

base5$policy_activity <- base5$policy_active + base5$policy_inactive

base6 <- base5 %>% 
  select(-policy_active, -policy_inactive)


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

base_school <- base_cases2[which(#base_cases2$subtype == 'Preschool or childcare facilities (generally for children ages 5 and below)' |
                                 base_cases2$subtype == 'Primary Schools (generally for children ages 10 and below)' #|
                                 #base_cases2$subtype == 'Secondary Schools (generally for children ages 10 to 18)'
                                 ),]

base_school$policy_active <- ifelse(base_school$policy_activity > 0, 1, 0)

hetero_school <- base_school %>%
  group_by(country, date, subtype) %>%
  mutate(hetero = -1*(abs((sum(policy_active)/length(policy_active)-0.5)*2))+1) %>% 
  select(-province, -policy_activity) %>%
  slice(1) %>%
  ungroup

#ggplot(hetero_school[hetero_school$subtype == 'Preschool or childcare facilities (generally for children ages 5 and below)',], aes(x = date, y = hetero)) + 
#  geom_line(aes(color = country), size = 1) +
#  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
#  theme_minimal()

ggplot(hetero_school[hetero_school$subtype == 'Primary Schools (generally for children ages 10 and below)',], aes(x = date, y = hetero)) + 
  geom_line(aes(color = country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
  theme_minimal()

#ggplot(hetero_school[hetero_school$subtype == 'Secondary Schools (generally for children ages 10 to 18)',], aes(x = date, y = hetero)) + 
#  geom_line(aes(color = country), size = 1) +
#  scale_color_manual(values = c("#00AFBB", "#E7B800", "red", "green")) +
#  theme_minimal()

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

hetero_mask2 <- hetero_mask[-which(hetero_mask$subtype == 'Other Mask Wearing Policy'),] %>%
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

hetero_all <- rbind(hetero_lockdown, hetero_mass, hetero_school2[,c(-4)], hetero_mask2[,c(-4)])

hetero_all$differentiating <- ifelse(hetero_all$type == "Lockdown", 1, ifelse(hetero_all$type == "Restrictions of Mass Gatherings", 1, 0))

write_csv(corona_sel,"~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/coronanet_policies.csv")

##All analyses

hetero_all <- merge(hetero_all, hhi_data[,-1], by = c('country', 'date'))

write_csv(hetero_all,"~/Dropbox/Joan Barcelo/Present/NYUAD Assistant Professor/Research/Papers/Work in Progress/West European Politics Corona Article/WEP_analysis/data/heterogeneity_analyses.csv")

hetero_all$hhi_cumulative <- as.numeric(hetero_all$hhi_cumulative)
hetero_all$hhi_new <- as.numeric(hetero_all$hhi_new)

summary(all.a <- lm(hetero_mean ~ as.factor(country) + differentiating + poly(as.Date(date), 3), data = hetero_all))
summary(all.b <- lm(hetero_mean ~ as.factor(country) + differentiating + as.numeric(measure_H1_H2_cases) + poly(as.Date(date), 3),
                   data = hetero_all))

summary(all.c <- lm(hetero_mean ~ as.factor(country)*differentiating + poly(as.Date(date), 3), data = hetero_all))
summary(all.d <- lm(hetero_mean ~ as.factor(country)*differentiating + as.numeric(measure_H1_H2_cases) + poly(as.Date(date), 3),
                    data = hetero_all))

stargazer(all.a, all.b, all.c, all.d, digits = 2)

