# clean coronanet data for WEP countries

## things to do
#  DONE: clean province names

# resolve orphans
# incorporate updates strengthening/relaxing
# DONE: clean whether the target is outside of the country or not
# DONE: incorporate updates; update/end
# DONE incorporate target_same variable

# filter types
# double check that there are no false negatives when you reduce by sub-type

#swiss health monitoring national miscoded, 
# why still missing data after 7-7??

mutate_cond <-
  function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition,] <- .data[condition,] %>% mutate(...)
    .data
  }

'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

# load packages
library(qdapRegex)
library(readr)
library(magrittr)
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
# set working directory
setwd('~/Dropbox/West European Politics Corona Article/')

# load data
qualtrics = read_csv("WEP_analysis/data/CoronaNet/coronanet_internal.csv")

qualtrics %>% filter(grepl("2170661", policy_id)) %>% data.frame()
# define parameters
countries = c("Switzerland", "Germany", "France", "Italy") 
policies = c('Lockdown', 'Closure and Regulation of Schools', 'Restrictions of Mass Gatherings')

# -------------------
# clean  CoronaNet data
# -------------------

# Filter policies to WEP countries
sub_data = qualtrics %>% dplyr:::filter(country %in% countries) 

# retrieve and add 'missing' policies
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona_missing.R')
sub_data = rbind(sub_data %>% mutate(missingDum = 0), missing)

# check and recode 'other' policy data
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona.R')


# fill in target country with country if not an external border restriction  
sub_data = sub_data %>% mutate(target_country = ifelse(is.na(target_country) &
                                                         type !='External Border Restrictions', country, target_country))
 

# fill in target_country by hand if type is external border restriction and target_country is na
sub_data = sub_data %>% mutate(target_country = case_when(
  policy_id == 8387693 ~ 'All countries',
  policy_id == 9415937 ~ 'Schengen Area',
  TRUE ~target_country
))

# Filter policies to only national or provincial level policies
sub_data = sub_data %>%
  filter(init_country_level %in% c("National", "Provincial")) %>%
  filter(target_country %in% countries) 


## remove irrelevant policies

sub_data = sub_data %>% filter(record_id %!in% c("R_3kzcET6o5pQyvP0NA",  # February 28,2020 Switzerland Council of Basel is cancelling the carnival of Basel ( target date 02-03-20 till 04-03-20)
                                                 "R_1lxIGQvx9vZZYM8NA", # In Switzerland based on Art. 6 und 7c CO-VID-19-Verordnung 2 (mass gathering at events and gatherings in public) from the 2nd of April on the new COVID law allows the police of Aargau to monitor public spaces in Aargau through cameras and use imaging devices of third parties without the permission of the federal commissioner for data protection and public issues.
                                                 "R_1LSBNoVwpZakIfZNA",# currently miscoded --- add back in when its fixed
                                                 'R_sb9IyocQDRID7b3NA',# currently miscoded --- add back in when its fixed
                                                 'R_1eCZx9LHWfawhbdNA',# currently miscoded As of May 19, the préfet of Haute-Corse closed all beaches, coasts, and coastal pathways in the department until March 31, prohibiting access to pedestrians, cyclists, and non-motorized vehicle traffic.
                                                 'R_qLC2DxYYk0CEQ25NA', # currently miscoded As of May 19, the préfet of Haute-Corse closed all beaches, coasts, and coastal pathways in the department until March 31, prohibiting access to pedestrians, cyclists, and non-motorized vehicle traffic.
                                                 'R_3ku0aYbVr9CHtEVNA',# currently miscoded  The Canton Zurich (Switzerland) recommends not to hold any event with participants from Italy, China, South Korea or Iran, with close bodily contact in closed facilities (eg clubs) or events with external visitors in care facilities and nursing homes.
                                               'R_2tM6ncrVSTXJyJUNA',# curently micoded, should be a city level policy 
                                               'R_1CHDPKjV4cVUREBNA',# miscoded, shouldn't be restrictions of mass gatherings
                                               'R_sHdwM3uydrZkLvzNA',# The Canton Zurich (Switzerland) recommends not to hold any event with participants from Italy, China, South Korea or Iran, with close bodily contact in closed facilities (eg clubs) or events with external visitors in care facilities and nursing homes.
                                               'R_reT4y2kUxvffllLNA',# In Italy Ligury region says Sampdoria-Verona match to be played "behind closed doors" on March 2
                                               'R_pLy55nCKN6rp7gdNA',# On February 25, the Swiss canton of St. Gallen announced that there are no restrictions on mass gatherings and no events will be canceled. Organizers should provide the canton with lists of participants and contact details; especially international participants
                                               'R_1q9AXImZ4raUmOCNA',# On 3rd of March the canton of Zurich in Switzerland recommends to cancel events that include people from Italy, China, South Korea and Iran.
                                               'R_3mlDLgfh7yZRBlaNA', # On the 28th of May, the Berlin Senate announced that from the 30th of May onwards, public demonstrations in the open would be allowed again, without any limitation to the number of participants, provided that the minimum distance of 1.5 meters and other hygiene rules are observed.
                                               ' R_1MX0RRHcaCgiTXaNA',# From March 9, 2020, the prefects of Corsica and Corse-du-Sud in France announced an obligation for sports events to take place behind closed doors.
                                               'R_blUCYuaQoqSHAl3NA',  # Switzerland, Geneva--"The cantonal police, with the support of the Civil Protection (PCi), proceeded this morning, Saturday March 28, to a cordon of the lakeside surroundings in order to prohibit the parking of private vehicles.
                                                'R_1JUu6XJWWJSvPJ3NA', # From March 9, 2020, the prefects of Corsica and Corse-du-Sud in France announced the closing of public baths except for closed-door competitions.
                                                'R_3G8uMNTTO2IBD7SNA', # In the German state of Rheinland-Pfalz, different households will now be allowed to take residence with each other in public spaces as of May 13
                                                'R_vBNxXQwwxlUCukNNA', # The Canton of Zug allows ice hockey games in the Bossard Arena to continue without spectators on February 28th 2020
                                                'R_1MX0RRHcaCgiTXaNA',# From March 9, 2020, the prefects of Corsica and Corse-du-Sud in France announced an obligation for sports events to take place behind closed doors.
                                               'R_xlQQ2Wj92uVqOjfNA', # The Italian government is banning all sporting matches and public events until next month in several regions of Northern Italy (restrictions of mass gatherings).\n\nThe new measures, approved last night, extends the urgent steps the government is taking for the containment of the coronavirus outbreak outside the exclusion zone. \n\nThe decree bans all events and sport matches in public and private locations from Febuary 26 until March 1.
                                                'R_1mJaJpu5i50Y5myNA', # In Italy Ligury region suspends check-in and guest entrance to university dormitories from March 1 till the midnight of March 8
                                               'R_reT4y2kUxvffllLNA')) # In Italy Ligury region says Sampdoria-Verona match to be played "behind closed doors" on March 2

# select only desired policies
sub_data = sub_data %>% filter(type %in% policies| 
                                 grepl('Wearing Mask|Mask Wearing', type_sub_cat))

# remove policies that are targeted toward particular populations
sub_data = sub_data %>% filter(grepl("No special population targeted", type_who_gen)|
                                 is.na(type_who_gen))

# remove smaller restrictions on mass gatherings
sub_data = sub_data %>% filter(type_sub_cat %!in%  c('Annually recurring event allowed to occur with certain conditions',
                                                     'Attendance at religious services restricted (e.g. mosque/church closings)',
                                                     'Cancellation of a recreational or commercial event',
                                                     'Cancellation of an annually recurring event',
                                                     'Events at private residencies restricted (e.g. parties held at home)',
                                                     'Postponement of a recreational or commercial event',
                                                     'Postponement of an annually recurring event',
                                                     'Prison population reduced (e.g. early release of prisoners)',
                                                     'Higher education institutions (i.e. degree granting institutions)'))

# select gatherings that restrict gatherings of 500 or more people
sub_data = sub_data %>%
  mutate(type_mass_gathering = as.numeric(str_trim(type_mass_gathering)))

sub_data = sub_data %>% filter( type %in% c('Social Distancing',
                                     'Lockdown', 'Closure and Regulation of Schools')|
                           type_mass_gathering > 500|is.na(type_mass_gathering))
 
 
 
# keep orphaned records for now --- investigate later
(orphans = names(which(unlist(lapply(split(sub_data$entry_type, sub_data$policy_id), function(x){
  all(unique(x) == 'update')}))) == TRUE) %>% unique()) 
 
sub_data = sub_data %>% mutate(orphanDum = ifelse(policy_id %in% orphans, 1, 0))


# replace end date with max last end date for now if end date is missing
# and remove observations that come after that end date
# end_date = max(sub_data$date_end, na.rm = TRUE)
end_date = "2020-07-15"
sub_data = sub_data %>% dplyr:::mutate(date_end = if_else(is.na(date_end), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
  # dplyr:::mutate(date_end = if_else(date_end > as.Date(end_date, "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
  filter(date_start<=as.Date(end_date, "%Y-%m-%d"))


# if the policy type is 'health resources', make the date end one day after the date start
# if the policy type is 'public awareness campaigns' or 'anti disinformaito campains', make the date end 14 days after the date start
sub_data = sub_data %>% mutate(date_end = dplyr:::if_else(is.na(date_end) & type == 'Health Resources', date_start + days(1), date_end),
                               date_end = dplyr:::if_else(is.na(date_end) & type %in% c( 'Public Awareness Campaigns', 'Anti-Disinformation Measures'), date_start + days(14), date_end))


# fill in appropriate province names
regions = read_csv("WEP_analysis/data/CoronaNet/country_region_clean.csv")
regions = regions %>% dplyr::: filter(Country %in% countries) %>% dplyr:::select(-ISO2)
regions$`0` = regions$Country
regions = regions %>% gather(reg_num, region, -Country)
regions = regions %>% dplyr:::filter(!is.na(region))

regions = regions %>% 
  group_by(Country) %>%
  expand(gov = region, target_province = region) %>%
  filter(c(gov %in% countries & gov != target_province )|c(gov==target_province  & gov %!in% countries))
names(regions)[1] = 'country'

 
# expand/fill in 'target_province' with name of all provinces if it is a national level policy
sub_data= sub_data %>%mutate(target_province = ifelse(country %in% 'Switzerland' & init_country_level %in%  "National" & is.na(target_province), paste(regions[which(regions$country == 'Switzerland'), 'target_province'] %>% distinct %>% pull(), collapse = ';') , 
                                        ifelse(country %in%  'Germany' & init_country_level %in%  "National" & is.na(target_province), paste(regions[which(regions$country == 'Germany'), 'target_province'] %>% distinct %>% pull(), collapse = ';') ,
                                               ifelse(country %in%  'France' & init_country_level %in%  "National" & is.na(target_province), paste(regions[which(regions$country == 'France'), 'target_province']  %>% distinct%>% pull(), collapse = ';') , 
                                                       ifelse(country %in%  'Italy' & init_country_level %in%  "National" & is.na(target_province), paste(regions[which(regions$country == 'Italy'), 'target_province']  %>% distinct%>% pull(), collapse = ';') , target_province)))))

# correct province names
sub_data = sub_data %>% mutate(target_province = ifelse(record_id %in% c("R_3L4g6LrQ8bdQvtvNA"), 'Lower Saxony', target_province  ))

sub_data = sub_data %>% 
                # mutate(target_province = gsub("\\;$|\\,$", "", target_province))%>%
                separate_rows(target_province, sep = ';|\\,') %>%
                # mutate(target_province = str_trim(gsub( "\\(Italy\\)|\\(Itay\\)|Departement|Canton|Canton of|Kanton|Department|Provinces of|Region", "",  target_province)),
                mutate(target_province = str_trim(target_province))%>%
                mutate(target_province = recode(target_province,
                        # "Baden-Württemberg" = "Baden-Wuerttemberg",
                        # "Zürich" = "Zurich",
                         #"Venice" = 'Veneto',
                         #"Venezia" = 'Veneto',
                         #"Venedig" = "Veneto",
                         'Emilia Romagna' = 'Emilia-Romagna'
                        # "Bayern" = "Bavaria",
                         #"Basilicata" = "Basilicate"
                       ))  
 
# remove 'end of policy' policies
sub_data = sub_data %>% filter(update_type %!in% 'End of Policy')


# save raw data 
saveRDS(sub_data, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")
 
# -------------------
# reshape CoronaNet data from raw data to long format 
# -------------------

# load data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")

## reshape coronanet data to long format by  date-gov-target_province-type

# make new variable called gov which is the same as 'province' but takes on the name of country if it is a national level policy
sub_data = sub_data %>% mutate(gov = ifelse(init_country_level == "National" & is.na(province), country, province)) %>% select(-province)

# aggregate sub_data by type such that you collapse over type_sub_cat
sub_data = sub_data %>% group_by(policy_id) %>%
  arrange(date_start, entry_type) %>%
  mutate(type_sub_cat = paste(unique(type_sub_cat), collapse = '; '),
         date_start = min(date_start, na.rm = TRUE),
         date_end = max(date_end, na.rm = TRUE)) %>% 
  distinct(gov, init_country_level, target_province, type, .keep_all = TRUE) %>%
  ungroup()

# reshape data from wide to long format for dates
sub_data = sub_data %>% dplyr:::select(policy_id, record_id, entry_type, update_type,  update_level, type, type_sub_cat, school_status, init_country_level, country, gov,  target_country, target_geog_level, target_province, date_start, date_end)%>% 
  gather(date_type, date, -policy_id,-record_id, -entry_type, -update_type, -update_level,  -type, -type_sub_cat, -school_status,-init_country_level, -country, -gov, -target_country, -target_geog_level, -target_province)

# collapse such that there is a unique observation for each gov, target_province, type and date combination
# the number of policies (policy_count) are  saved in the policy_id
# and the record of the policy_id/record_id are aggregated
sub_data= sub_data %>% group_by(gov, target_province, type, date) %>%
  mutate(policy_count = length(unique(policy_id)) ,
         policy_id = paste(unique(policy_id), collapse = ';'),
         record_id = paste(unique(record_id), collapse = ';')
  ) %>% 
  distinct(gov, target_province, type, date, .keep_all = TRUE ) %>%
  ungroup()


### make data frame for full long format by date-gov-target_province-type
end_date = "2020-07-15"
dframe = expand_grid(date = seq(as.Date("2019-12-31", "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), by = "day"), regions , type = c("Social Distancing", policies))

# merge data
data_long = merge(dframe, sub_data, by = c( "date",  'country', "gov", "target_province", "type"), all.x = TRUE)


# fill in  for record_id, policy_id 
# https://stackoverflow.com/questions/35362575/equivalent-to-cumsum-for-string-in-r
data_long = data_long %>% 
  group_by(country, gov, target_province, type) %>%
  arrange(country, gov, target_province, type, date) %>%
  mutate(record_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(record_id), "", record_id)), accumulate = TRUE)))),
         policy_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(policy_id), "", policy_id)), accumulate = TRUE))))) %>%
  ungroup()

# remove duplicate record_id and policy_ids
data_long = data_long %>% group_by(country, gov, target_province, type) %>% 
  mutate(policy_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(policy_id, ';'), function(x) unique(x)))), collapse = ';')),
         record_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(record_id, ';'), function(x) unique(x)))), collapse = ';')))

# fill in type and type_sub_cat and init_country_level
data_long = data_long %>% 
  group_by(country, gov, target_province, type) %>%
  fill( type,  init_country_level, entry_type, target_country, target_geog_level, .direction = 'down')%>%
  ungroup()

# create measure of policy incidence (policy_count) and binary variable of whether a policy exists (policy_dum) by gov, target_province, date and type
data_long = data_long %>% 
  dplyr:::mutate(policy_dum = case_when(is.na(date_type)~0,
                                        date_type == 'date_start' & entry_type=='new_entry'  ~ as.numeric(policy_count),
                                        date_type == 'date_end' & entry_type=='new_entry' ~ as.numeric(policy_count) -1,
                                        TRUE ~ 0)) %>%
  group_by(country, gov, target_province, type ) %>%
  mutate(policy_count = cumsum(policy_dum)) %>%
  mutate(policy_dum = ifelse(policy_count>0, 1, 0)) %>%
  ungroup()  


# save reshaped data 
saveRDS(data_long, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long.RDS")
