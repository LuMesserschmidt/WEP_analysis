# Cases Merge
# By Anelia Petrova and Luca Messerschmidt
# August 19 2020

rm(list=ls())
library(plyr)
library(tidyverse)
library(data.table)
library(plotly)
library(reshape2)
library(eurostat)
require(gridExtra)
library(lubridate)


coronanet <- read_csv("https://raw.githubusercontent.com/LuMesserschmidt/WEP_analysis/master/data/CoronaNet/coronanet_internal.csv") %>%
  filter(country %in% c("France", "Germany", "Italy", "Switzerland"))

#Load merged Data
italy <- read_csv("data/Cases/Italy/italy_dashboard/data_regions.csv")
france <- read_csv("data/Cases/France/france_subnational.csv")
germany <- read_csv("https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rki-by-state.csv")

switzerland <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv")


name <- c("date", "Schleswig-Holstein", "Hamburg", "Lower Saxony", "Bremen",
          "North Rhine-Westphalia", "Hesse", "Rheinland-Pfalz", "Baden-Wuerttemberg",
          "Bavaria", "Saarland", "Brandenburg", "Mecklenburg-Vorpommern",
          "Saxony", "Saxony-Anhalt", "Thuringia", "Berlin", "sum_cases")
setnames(germany, name)

germany <- germany %>%
  melt(id.vars=c("date")) %>%
  setnames(c("date", "region", "cases"))%>%
  filter(region!="sum_cases")
germany$country<- "Germany"

# France Data
france$other_hospitalization <- france$hospitalises - france$reanimation
france <- france %>%
  select(date, nom, casConfirmes, deces, reanimation, other_hospitalization, gueris)
colsum <- france$deces + france$reanimation + france$other_hospitalization + france$gueris
france$estimated_cases <- sapply(1:nrow(france), function (i) { if (!is.na(france$casConfirmes[[i]])) {france$casConfirmes[i]} else {colsum[[i]]}})
france <- france %>%
  select(date, nom, estimated_cases)%>%
  setnames(c("date", "region", "cases"))
france$country<- "France"


# Italy Data
italy <- italy %>%
  select(data, denominazione_regione, totale_casi) %>%
  setnames(c("date", "region", "cases"))
italy$country<- "Italy"

#Switzerland 
switzerland <- switzerland %>%
  select(date, abbreviation_canton_and_fl, ncumul_conf) %>%
  setnames(c("date", "region", "cases"))%>%
  filter(region!="FL")%>%
  mutate(
    region = recode(
      region,
      "AG"="Aargau",
      "AI"="Appenzell Ausserrhoden",
      "AR"="Appenzell Innerrhoden",
      "BE"="Bern",
      "BL"="Basel-Landschaft",
      "BS"="Basel-City",
      "FR"="Fribourg",
      "GE"="Geneva",
      "GL"="Glarus",
      "GR"="Grisons",
      "JU"="Jura",
      "LU"="Lucerne",
      "NE"="Neuchatel",
      "NW"="Nidwalden",
      "OW"="Obwalden",
      "SG"="Saint Gallen",
      "SH"="Schaffhausen",
      "SO"="Solothurn",
      "SZ"="Schwyz",
      "TG"="Thurgau",
      "TI"="Ticino",
      "UR"="Uri",
      "VD"="Vaud",
      "VS"="Valais",
      "ZG"="Zug",
      "ZH"="Zurich",
      .default = region
    ))
switzerland$country<- "Switzerland"
#merge
cases_combined<- rbind(france,germany,italy,switzerland)

suedtirol <- cases_combined %>%
  filter(country == "Italy") %>%
  filter(region == "P.A. Bolzano" | region == "P.A. Trento")
suedtirol <- unique(suedtirol)
suedtirol <- aggregate(list(cases=suedtirol$cases), by=list(date=suedtirol$date), FUN=sum)
suedtirol$region <- "Trentino-Alto Adige"
suedtirol$country <- "Italy"
suedtirol <- setnames(suedtirol, c("date", "cases", "region", "country"))
suedtirol <- suedtirol %>%
  select(date, region, cases, country)

cases_combined <- rbind(unique(cases_combined), suedtirol) %>%
  filter(region != "P.A. Bolzano") %>%
  filter(region != "P.A. Trento") %>%
  arrange(country, region, date)



cases_germany <- cases_combined %>% filter(country == "Germany")
cases_france <- cases_combined %>% filter(country == "France")
cases_italy <- cases_combined %>% filter(country == "Italy")
cases_switzerland <- cases_combined %>% filter(country == "Switzerland")



days <- seq(from=as.Date('2020-01-01'), to=as.Date("2020-07-28"),by='days')
for (i in seq_along(days)) {
  for (x in unique(cases_combined$region)) {
    if (length(cases_combined[cases_combined$region == x & cases_combined$date == days[i],]$country) < 1) {
      cases_combined <- rbind(cases_combined, data.frame(date = days[i], region = x, cases = 0, country = cases_combined[cases_combined$region == x,]$country[1]))
    }
  }
}


write_csv(cases_combined, "data/Cases/combined_cases.csv")

# Eurostat data (TODO ANELIA)
geodict <- read_delim("data/EUROSTAT_NUTS2_controls/geo.dic", "\t", col_names = c("code", "region"))
agrdict <- read_csv("data/EUROSTAT_NUTS2_controls/itm_newa.csv") %>%
  select(Label, Notation)
nacer2dict <- read_csv("data/EUROSTAT_NUTS2_controls/nace_r2.csv") %>%
  select(Label, Notation)
indicsbdict <- read_csv("data/EUROSTAT_NUTS2_controls/indic_sb.csv") %>%
  select(Label, Notation)

# restrict data to subnational Germany, France, Italy
reg <- function(x) { grepl("^DE[1-9A-G]$", x) | grepl("^FR[1-9A-M]$|^FRY[1-5]$", x) | grepl("^IT[A-Z][1-9]$", x)}
# restrict data to national Germany, Switzerland, France, Italy
regex_national <- function(x) { grepl("^DE$", x) | grepl("^CH$", x) | grepl("^FR$", x) | grepl("^IT$", x)}

# fill in population data
# Germany, France, Italy (NUTS2)
population_total_GFI <- get_eurostat("demo_r_d2jan", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  filter(age == "TOTAL") %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ sex, fun.aggregate = median, value.var = "values") %>%
  select(geo, region, "T") %>%
  setnames(c("geo", "region", "eurostat_total_population_2019"))
# Switzerland (NUTS3)
population_total_swiss <- get_eurostat("demo_r_pjangrp3", time_format = "num") %>%
  filter(grepl("CH[0-9][0-9][0-9]", geo)) %>%
  filter(age == "TOTAL" & sex == "T" & time == "2019") %>%
  select(geo, eurostat_total_population_2019 = values) %>%
  left_join(geodict, by=c("geo" = "code"))
population_total <- rbind(population_total_GFI, population_total_swiss)

# merge population data with cases data
# fix names
population_total$region <- gsub("Baden-Württemberg", "Baden-Wuerttemberg", population_total$region)
population_total$region <- gsub("Bayern", "Bavaria", population_total$region)
population_total$region <- gsub("Hessen", "Hesse", population_total$region)
population_total$region <- gsub("Niedersachsen", "Lower Saxony", population_total$region)
population_total$region <- gsub("Nordrhein-Westfalen", "North Rhine-Westphalia", population_total$region)
population_total$region <- gsub("Sachsen", "Saxony", population_total$region)
population_total$region <- gsub("Thüringen", "Thuringia", population_total$region)
population_total$region <- gsub("Île de France", "Île-de-France", population_total$region)
population_total$region <- gsub("Centre - Val de Loire", "Centre-Val de Loire", population_total$region)
population_total$region <- gsub("Bourgogne - Franche-Comté", "Bourgogne-Franche-Comté", population_total$region)
population_total$region <- gsub("Nord-Pas-de-Calais - Picardie", "Hauts-de-France", population_total$region)
population_total$region <- gsub("Alsace - Champagne-Ardenne - Lorraine", "Grand Est", population_total$region)
population_total$region <- gsub("Pays-de-la-Loire", "Pays de la Loire", population_total$region)
population_total$region <- gsub("Aquitaine - Limousin - Poitou-Charentes", "Nouvelle-Aquitaine", population_total$region)
population_total$region <- gsub("Languedoc-Roussillon - Midi-Pyrénées", "Occitanie", population_total$region)
population_total$region <- gsub("Auvergne - Rhône-Alpes", "Auvergne-Rhône-Alpes", population_total$region)
population_total$region <- gsub("Valle d'Aosta/Vallée d'Aoste", "Valle d'Aosta", population_total$region)
population_total$region <- gsub("Genève", "Geneva", population_total$region)
population_total$region <- gsub("Freiburg", "Fribourg", population_total$region)
population_total$region <- gsub("Neuchâtel", "Neuchatel", population_total$region)
population_total$region <- gsub("St. Gallen", "Saint Gallen", population_total$region)
population_total$region <- gsub("Graubünden", "Grisons", population_total$region)
population_total$region <- gsub("Luzern", "Lucerne", population_total$region)
population_total$region <- gsub("Basel-Stadt", "Basel-City", population_total$region)
population_total$region <- gsub("Zürich", "Zurich", population_total$region)
population_total <- rbind(population_total, data.frame(geo=NA, region="Trentino-Alto Adige", eurostat_total_population_2019=531178 + 541098)) %>%
  filter(!region %in% c("Provincia Autonoma di Bolzano/Bozen", "Provincia Autonoma di Trento"))

# merge data
cases_combined <- cases_combined %>%
  left_join(population_total, by=c("region"="region"))

# ----- BEGIN TODO ANELIA -----
# other Eurostat data
activity_rates <- get_eurostat("lfst_r_lfp2actrtn", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  filter(citizen == "EU27_2020_FOR" & isced11 == "TOTAL" & age == "Y15-64" & sex == "T") %>%
  select(values, region) %>%
  setnames(c("eurostat_activity_rate_2019", "region"))
agriculture <- get_eurostat("agr_r_accts", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  filter(unit == "MIO_EUR") %>%
  filter(indic_ag == "PROD_BP") %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  inner_join(agrdict, by = c("itm_newa" = "Notation")) %>%
  filter(itm_newa %in% c("01000", "04000", "12000", "13000", "16000")) %>%
  dcast(geo + region ~ indic_ag + Label + unit, fun.aggregate = median, value.var = "values") %>%
  select(-geo) %>%
  setnames(c("region", "eurostat_prodbp_agricultural_output_mioeur_2018", 
             "eurostat_prodbp_animal_output_mioeur_2018", "eurostat_prodbp_animal_products_mioeur_2018", "eurostat_prodbp_cereals_seeds_mioeur_2018", "eurostat_prodbp_vegetables_horticultural_mioeur_2018"))
poverty_risk <- get_eurostat("ilc_li41", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  select(region, values) %>%
  setnames(c("region", "eurostat_poverty_risk_rate_2018"))
employment_education <- get_eurostat("lfst_r_lfe2eedu", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  filter(sex == "T" & isced11 == "TOTAL" & age == "Y_GE15") %>%
  select(region, values) %>%
  setnames(c("region", "eurostat_employment_thousands_2019"))
employment_sector_growth_2017 <- get_eurostat("sbs_r_nuts06_r2", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  filter(indic_sb %in% c("V11210", "V13320", "V16110", "V91290", "V94310")) %>%
  filter(nace_r2 %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "L",
                        "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                        "W", "X", "Y", "Z")) %>%
  inner_join(nacer2dict, by=c("nace_r2" = "Notation")) %>%
  inner_join(indicsbdict, by=c("indic_sb" = "Notation")) %>%
  dcast(geo + region ~ Label.x + Label.y, fun.aggregate = median, value.var = "values") %>%
  select(-geo)
employment_sector_activity <- get_eurostat("nama_10r_2emhrw", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  left_join(nacer2dict, by=c("nace_r2" = "Notation")) %>%
  dcast(geo + region ~ Label, fun.aggregate = median, value.var = "values") %>%
  select(-geo)
gdp <- get_eurostat("nama_10r_2gdp", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  filter(unit %in% c("MIO_EUR", "EUR_HAB")) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values") %>%
  select(region, MIO_EUR, EUR_HAB) %>%
  setnames(c("region", "eurostat_gdp_mio_eur_2018", "eurostat_gdp_eur_hab_2018"))
health_personnel <- get_eurostat("hlth_rs_prsrg", time_format = "num") %>%
  filter(reg(geo)) %>%
  # not enough info otherwise
  filter(time == "2018") %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values") %>%
  select(region, HAB_P, NR, P_HTHAB) %>%
  setnames(c("region", "eurostat_inhabitants_per_doctor_2018", "eurostat_doctors_nr_2018", "eurostat_doctors_per_100000_inhabitants_2018"))
hospital_beds <- get_eurostat("hlth_rs_bdsrg", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values") %>%
  select(region, HAB_P, NR, P_HTHAB) %>%
  setnames(c("region", "eurostat_inhabitants_per_hospital_beds_2018", "eurostat_hospital_beds_nr_2018", "eurostat_hospital_beds_per_100000_inhabitants_2018"))
household_income <- get_eurostat("nama_10r_2hhinc", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == "2017") %>%
  filter(unit %in% c("MIO_EUR", "EUR_HAB")) %>%
  filter(na_item %in% c("B5N", "B7N")) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values") %>%
  select(region, MIO_EUR, EUR_HAB) %>%
  setnames(c("region", "eurostat_household_income_mioeur_B5N_2017", "eurostat_household_income_eur_per_inhabitant_B5N_2017"))
long_term_unemployment <- get_eurostat("lfst_r_lfu2ltu", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values") %>%
  select(region, PC_ACT, PC_UNE, THS) %>%
  setnames(c("region", "eurostat_lt_unemp_percentage_active_2019", "eurostat_lt_unemp_percentage_unemp_2019", "eurostat_lt_unemp_thousands_2019"))
percentage_unemployment_gender <- get_eurostat("lfst_r_lfu3rt", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  filter(age == "Y_GE15") %>%
  select(region, values) %>%
  setnames(c("region", "eurostat_percentage_unemployment_2019"))
population_education <- get_eurostat("edat_lfse_04", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  filter(sex == "T") %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ isced11, fun.aggregate = median, value.var = "values") %>%
  select(region, "ED0-2", "ED3-8", "ED3_4", "ED5-8") %>%
  setnames(c("region", "eurostat_percentage_ED0-2_2019", "eurostat_percentage_ED3-8_2019", "eurostat_percentage_ED3_4_2019",
             "eurostat_percentage_ED5-8_2019"))
population_sex <- get_eurostat("demo_r_d2jan", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  filter(age == "TOTAL") %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  dcast(geo + region ~ sex, fun.aggregate = median, value.var = "values") %>%
  select(region, "F", "M", "T") %>%
  setnames(c("region", "eurostat_female_population_2019", "eurostat_male_population_2019", "eurostat_total_population_2019"))
poverty_exclusion <- get_eurostat("ilc_peps11", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  select(region, values) %>%
  setnames(c("region", "eurostat_risk_poverty_or_social_exclusion_2019"))
severe_deprivation <- get_eurostat("ilc_mddd21", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  select(region, values) %>%
  setnames(c("region", "eurostat_severe_material_deprivation_rate_2018"))
unemployment_sex <- get_eurostat("lfst_r_lfu3pers", time_format = "num") %>%
  filter(reg(geo)) %>%
  filter(time == max(time)) %>%
  left_join(geodict, by=c("geo" = "code")) %>%
  filter(age == "Y_GE15") %>%
  dcast(geo + region ~ sex, fun.aggregate = median, value.var = "values") %>%
  select(region, "F", "M", "T") %>%
  setnames(c("region", "eurostat_female_unemployment_thousand_2019", "eurostat_male_unemployment_thousand_2019", "eurostat_total_unemployment_thousand_2019"))

eurostat_data <- activity_rates %>%
  left_join(agriculture, by="region") %>%
  left_join(poverty_risk, by="region") %>%
  left_join(employment_education, by="region") %>%
  left_join(employment_sector_growth_2017, by="region") %>%
  left_join(employment_sector_activity, by="region") %>%
  left_join(gdp, by="region") %>%
  left_join(health_personnel, by="region") %>%
  left_join(hospital_beds, by="region") %>%
  left_join(household_income, by="region") %>%
  left_join(long_term_unemployment, by="region") %>%
  left_join(percentage_unemployment_gender, by="region") %>%
  left_join(population_education, by="region") %>%
  left_join(population_sex, by="region") %>%
  left_join(poverty_exclusion, by="region") %>%
  left_join(severe_deprivation, by="region") %>%
  left_join(unemployment_sex, by="region")

write_csv(eurostat_data, "data/eurostat_data_national.csv")

activity_rates %>%
  dcast(geo + region ~ isced11 + age + sex, fun.aggregate = median, value.var = "values")
agriculture %>%
  select(-unit) %>%
  dcast(geo + region ~ indic_ag + itm_newa, fun.aggregate = median, value.var = "values")
poverty_risk %>%
  select(-unit)
employment_education %>%
  select(-unit) %>%
  dcast(geo + region ~ isced11 + age + sex, fun.aggregate = median, value.var = "values")
employment_sector_growth %>%
  dcast(geo + region ~ nace_r2 + indic_sb, fun.aggregate = median, value.var = "values")
employment_sector_activity %>%
  #mutate(values = values * 1000) %>%
  select(-unit) %>%
  dcast(geo + region ~ wstatus + nace_r2, fun.aggregate = median, value.var = "values")
gdp %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values")
health_personnel %>%
  dcast(geo + region ~ isco08, fun.aggregate = median, value.var = "values")
hospital_beds %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values")
household_income %>%
  dcast(geo + region ~ unit + direct + na_item, fun.aggregate = median, value.var = "values")
long_term_unemployment %>%
  dcast(geo + region ~ unit, fun.aggregate = median, value.var = "values")
percentage_unemployment_gender %>%
  dcast(geo + region ~ age + sex, fun.aggregate = median, value.var = "values")
population_education %>%
  dcast(geo + region ~ age + sex, fun.aggregate = median, value.var = "values")
population_gender %>%
  dcast(geo + region ~ age + sex, fun.aggregate = median, value.var = "values")
poverty_exclusion %>%
  select(-unit)
severe_deprivation %>%
  select(-unit)
unemployment_gender %>%
  select(-unit) %>%
  dcast(geo + region ~ age + sex, fun.aggregate = median, value.var = "values")

#activity_rates <- activity_rates %>%
#  select(-citizen, -unit, -time)
activity_rates2 <- activity_rates %>%
  #reshape(idvar = c("geo", "region"), timevar = "isced11", direction = "wide")
  melt(id=c("geo", "region", "values")) %>%
  dcast(geo + region + values ~ value)
#dcast(geo + region ~ isced11 + age + sex)
education <- activity_rates2 %>%
  select(geo, region, values, `ED0-2`, ED3_4, `ED5-8`, NRP, TOTAL) %>%
  group_by(geo, region, values) %>%
  #mutate_each(funs(an = as.numeric(as.character(.)))) %>%
  gather(education, val, -geo, -region, -values) %>%
  filter(val == 1) %>%
  select(-val) %>%
  arrange(geo, region, values)
age <- activity_rates2 %>%
  select(geo, region, values, `Y15-64`, `Y20-64`, `Y25-54`, `Y55-64`) %>%
  group_by(geo, region, values) %>%
  gather(age, val, -geo, -region, -values) %>%
  filter(val == 1) %>%
  select(-val) %>%
  arrange(geo, region, values)
sex <- activity_rates2 %>%
  select(geo, region, values, `F`, `M`, `T`) %>%
  group_by(geo, region, values) %>%
  gather(sex, val, -geo, -region, -values) %>%
  filter(val == 1) %>%
  select(-val) %>%
  arrange(geo, region, values)

education %>%
  inner_join(sex, by=c("geo" = "geo", "region" = "region", "values" = "values")) %>%
  inner_join(age, by=c("geo" = "geo", "region" = "region", "values" = "values"))

merged <- read_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_v2.csv")

eurostat  <- eurostat %>%
  filter(!code %in% c("CH040", "CH070", "DE30", "DE300", "DE_CAP", "DE40",
                      "DE50", "DE60", "DE600", "DE60000001", "DE80", "DEC0",
                      "DEE0", "DEF0", "DEG0", "FR10", "FRB0", "FRG0", "FRH0",
                      "FRL0", "FRM0", "FRY10", "FRY20", "FRY30", "FRY40", "FRY50",
                      "ITC20"))

eurostat$region <- gsub("Provincia Autonoma di Trento", "Trentino-Alto Adige", eurostat$region)

eurostat %>%
  group_by(region) %>%
  tally()

cases_combined %>%
  group_by(region) %>%
  tally()

cases_combined %>%
  right_join(coronanet %>% filter(init_country_level != "National"), by=c("country" = "country", "region" = "province", "date" = "date_announced")) %>%
  left_join(eurostat, by=c("region" = "region")) #%>%
#write_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_v1.csv")

cases_combined %>%
  filter(country == "Switzerland") %>%
  group_by(region) %>%
  tally()



coronanet_national <- coronanet %>%
  filter(init_country_level == "National")
coronanet_regional <- coronanet %>%
  filter(init_country_level != "National")



eurostat_d <- read_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/eurostat_data_national.csv")




merged_national <- cases_combined %>%
  filter(region == "National") %>%
  full_join(coronanet_national, by=c("country" = "country", "date" = "date_announced")) %>%
  left_join(eurostat_d, by=c("region" = "region")) #%>%
#left_join(swiss, by = c("region" = "region"))




merged_regional <- cases_combined %>%
  filter(region != "National") %>%
  full_join(coronanet_regional, by=c("country" = "country", "region" = "province", "date" = "date_announced")) %>%
  left_join(eurostat, by=c("region" = "region")) %>%
  left_join(swiss, by = c("region" = "region"))



write_csv(merged_national, "C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_national.csv")
write_csv(merged_regional, "C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_regional.csv")



merged_regional <- merged_regional %>%
  select(-eurostat_population_2019)



merged_national <- merged_national %>%
  select(-`Water supply; sewerage, waste management and remediation activities_Share of employment in manufacturing total - percentage`)




setdiff(names(merged_regional), names(merged_national))




merged_regional$eurostat_total_population_2019 <- sapply(1:nrow(merged_regional), function (i) { if (!is.na(merged_regional$eurostat_total_population_2019[[i]])) {merged_regional$eurostat_total_population_2019[[i]]} else {merged_regional$eurostat_population_2019[[i]]}})



merged$NUTS_code <- sapply(1:nrow(merged), function (i) { if (!is.na(merged$code[[i]])) {merged$code[[i]]} else {merged$geo[[i]]}})



merged <- merged %>%
  select(-geo, -code, -eurostat_population_2019, -eurostat_total_population_2019)



write_csv(merged, "C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_v5.csv")


merged %>%
  group_by(date, region) %>%
  tally()
#----

'

cases_national <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")



cases_national <- cases_national %>%
  filter(`Country/Region` %in% c("Germany", "France", "Italy", "Switzerland")) %>%
  filter(is.na(`Province/State`)) %>%
  select(-`Province/State`, -Lat, -Long) %>%
  melt(id.vars=c("Country/Region")) %>%
  rename(country = `Country/Region`, date = variable, cases = value) %>%
  mutate(date = as.Date(as.character(date), format = "%m/%d/%y"))
cases_national$region <- "National"



days <- seq(from=as.Date('2020-01-01'), to=as.Date("2020-07-28"),by='days')
for (i in seq_along(days)) {
  for (x in unique(cases_national$country)) {
    if (length(cases_national[cases_national$country == x & cases_national$date == days[i],]$country) < 1) {
      cases_national <- rbind(cases_national, data.frame(date = days[i], country = x, cases = 0, region = "National"))
    }
  }
'
#---- END TODO ANELIA

cases_combined<- read_csv("data/Cases/combined_cases.csv")


cases_combined <- cases_combined %>%
  arrange(date, country, region)

cases_national <- cases_combined %>% dplyr::group_by(date, country) %>% dplyr::summarize(cases_national =sum(cases,na.rm = T))


cases_national <- cases_national %>%
  arrange(date, country)

cases_combined <- left_join(cases_combined, cases_national,by=c("country","date")) 

cases_combined <- cases_combined %>%
  filter(date <= "2020-07-22")


write_csv(cases_combined, "data/Cases/combined_cases.csv")

#----
merged_total <- rbind(merged_national, merged_regional) %>%
  arrange(date, country, region)



merged_total %>%
  filter(region != "Principality of Liechtenstein") %>%
  write_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_v7.csv")




write_csv(merged_total, "C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_final.csv")

#----


merged_total<-cases_combined




merged_total$cases_national <- sapply(1:nrow(merged_total), function (i) { 
  res <- merged_national %>%
    filter(region == "National") %>%
    filter(date == merged_total$date[i]) %>%
    filter(country == merged_total$country[i])
  res$cases[1]
})



col_id <- grep("^cases_national$", names(merged_total))
merged_total <- merged_total[, c(col_id, (1:ncol(merged_total))[-col_id])]
col_id2 <- grep("^cases$", names(merged_total))
merged_total <- merged_total[, c(col_id2, (1:ncol(merged_total))[-col_id2])]
col_id3 <- grep("^region$", names(merged_total))
merged_total <- merged_total[, c(col_id3, (1:ncol(merged_total))[-col_id3])]
col_id4 <- grep("^date$", names(merged_total))
merged_total <- merged_total[, c(col_id4, (1:ncol(merged_total))[-col_id4])]



merged_total <- read_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/merged_v8.csv")




merged_total$code <- sapply(1:nrow(merged_total), function (i) { if (!is.na(merged_total$code[[i]])) {merged_total$code[[i]]} else {
  res <- swiss_dict %>%
    filter(region == merged_total$region[i])
  res$code[1]
}})



codes <- read_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/codes.csv")




merged_total$code <- sapply(1:nrow(merged_total), function (i) { if (!is.na(merged_total$code[[i]])) {merged_total$code[[i]]} else {
  res <- codes %>%
    filter(country == merged_total$country[i])
  res$code[1]
}})





merged_total <- merged_total %>%
  filter(region != "Principality of Liechtenstein")



merged_total$code <- gsub("CH04", "CH040", merged_total$code)
merged_total$code <- gsub("CH07", "CH070", merged_total$code)



merged_total %>%
  group_by(code, region) %>%
  tally()





swiss_dict <- geodict %>%
  filter(grepl("^CH[0-9][0-9][0-9]$", code))
swiss_dict$region <- gsub("GenÃ¨ve", "Geneva", swiss_dict$region)
swiss_dict$region <- gsub("NeuchÃ¢tel", "Neuchatel", swiss_dict$region)
swiss_dict$region <- gsub("Basel-Stadt", "Basel-City", swiss_dict$region)
swiss_dict$region <- gsub("Freiburg", "Fribourg", swiss_dict$region)
swiss_dict$region <- gsub("ZÃ¼rich", "Zurich", swiss_dict$region)
swiss_dict$region <- gsub("St. Gallen", "Saint Gallen", swiss_dict$region)
swiss_dict$region <- gsub("GraubÃ¼nden", "Grisons", swiss_dict$region)
swiss_dict$region <- gsub("Luzern", "Lucerne", swiss_dict$region)



nyt_france <- read_csv("C:/Users/Anelia/Desktop/projects/coronanet/WEP/WEP_analysis/data/NYT/nyt_france_clean.csv")


nyt_france %>%
  group_by(display_name) %>%
  tally()




part <- nyt_france %>%
  filter(display_name == "Provence-Alpes-CÃ´te d'Azur")
part$date <- as.Date('2020-04-13')
days <- seq(from=as.Date('2020-04-13'), to=as.Date("2020-07-19"),by='days')
for (i in seq_along(days)) {
  part$date[i] <- days[i]
}



merged %>%
  select(date, region, cases, cases_national, country) %>%
  unique()



merged_national <- read_csv("data/merged_national.csv")
merged_regional <- read_csv("data/merged_regional.csv")

#----

library(zoo)
coronanet <- readRDS("data/CoronaNet/coronanet_internal_sub_clean.RDS") %>%
  filter(country %in% c("France", "Germany", "Italy", "Switzerland"))

cases_combined<- read_csv("data/Cases/combined_cases.csv")

names(df_main)
df_main <- read_csv("data/merged_final.csv")


merged_regional<- cases_combined
merged_regional <- merged_regional %>% 
  group_by(country, region) %>% 
  mutate(
    new_cases = cases - lag(cases),
    new_cases_national = cases_national - lag(cases_national)
  )%>%
  drop_na(region)

# compute columns as described in cases_concentration.R



merged_regional <- merged_regional %>%
  select(date, country, region, cases, new_cases, new_cases_national) %>%
  unique() %>%
  group_by(country, region) %>%
  mutate(rolling_average = rollapply(cases, 7, mean, fill=NA)) %>%
  mutate(past_average = rollapply(cases, 7, mean, align="right", fill=NA)) %>%
  mutate(rolling_average_new_cases = rollapply(new_cases, 7, mean, fill=NA)) %>%
  mutate(past_average_new_cases = rollapply(new_cases, 7, mean, align="right", fill=NA)) %>%
  mutate(past_average_new_cases_national = rollapply(new_cases_national, 7, mean, align="right", fill=NA)) %>%
  left_join(df_euro, by=c("region"="region","country"="country"))

merged_regional$measure_H1_H2<- merged_regional$past_average_new_cases_national/merged_regional$sum_pop
merged_regional$measure_H3<- (merged_regional$past_average_new_cases/merged_regional$eurostat_total_population_2019) - (merged_regional$measure_H1_H2)

merged_regional$eurostat_total_population_2019 <- sapply(1:nrow(merged_regional), function(i) {if (merged_regional$country[i] == "Germany") {83019213} else if (merged_regional$country[i] == "France") {67012883} else if (merged_regional$country[i] == "Italy") {59287270} else {8544527}})
merged_regional$sum_pop <- sapply(1:nrow(merged_regional), function(i) {if (merged_regional$country[i] == "Germany") {83019213} else if (merged_regional$country[i] == "France") {67012883} else if (merged_regional$country[i] == "Italy") {59287270} else {8544527}})

eurostat_regional <- read_csv("data/eurostat_data.csv") %>%
  select(-eurostat_total_population_2019)



