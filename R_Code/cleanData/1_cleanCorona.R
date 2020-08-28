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


# define parameters
countries = c("Switzerland", "Germany", "France", "Italy") 
policies = c('Lockdown', 'Closure and Regulation of Schools', 'Restrictions of Mass Gatherings')
# policies = c('Lockdown', 'Restrictions of Mass Gatherings')

# -------------------
# clean  CoronaNet data
# -------------------
 
# Filter policies to WEP countries
sub_data = qualtrics %>% dplyr:::filter(country %in% countries & country_overwrite == 0) 

# retrieve and add 'missing' policies
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona_missing.R')
sub_data = rbind(sub_data %>% mutate(missingDum = 0), missing)

 
# check and recode 'other' policy data
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona.R')

  
# # fill in target country with country if not an external border restriction  
# sub_data = sub_data %>% mutate(target_country = ifelse(is.na(target_country) &
#                                                          type !='External Border Restrictions', country, target_country))
#  
# 
# # fill in target_country by hand if type is external border restriction and target_country is na
# sub_data = sub_data %>% mutate(target_country = case_when(
#   policy_id == 8387693 ~ 'All countries',
#   policy_id == 9415937 ~ 'Schengen Area',
#   TRUE ~target_country
# ))

# select only desired policies
sub_data = sub_data %>% filter(type %in% c("Lockdown", "Closure and Regulation of Schools", "Restrictions of Mass Gatherings")| 
                                 grepl('Wearing Mask|Mask Wearing', type_sub_cat))


# remove smaller restrictions on mass gatherings  
sub_data = sub_data %>% filter(type_sub_cat %!in%  c('Annually recurring event allowed to occur with certain conditions',
                                                     'Attendance at religious services restricted (e.g. mosque/church closings)',
                                                     'Cancellation of a recreational or commercial event',
                                                     'Cancellation of an annually recurring event',
                                                     'Events at private residencies restricted (e.g. parties held at home)',
                                                     'Postponement of a recreational or commercial event',
                                                     'Postponement of an annually recurring event',
                                                     'Prison population reduced (e.g. early release of prisoners)',
                                                     'Other mass gatherings not specified above restricted'))

# select only primary school data
sub_data = sub_data %>% filter( type %in% c("Lockdown",  'Restrictions of Mass Gatherings',  "Social Distancing")|
                                  type_sub_cat%in% 'Primary Schools (generally for children ages 10 and below)')

# only choose school closure policies
sub_data = sub_data %>% filter(school_status %!in% c('Primary Schools allowed to open with conditions',
                                                      'Primary Schools allowed to open with no conditions'))
 
# change sub type for lockdowns
sub_data = sub_data %>%  filter(type_sub_cat %!in% c('People in nursing homes/long term care facilities'))
sub_data = sub_data %>% mutate(type_sub_cat = ifelse(type == 'Lockdown',  'Lockdown', type_sub_cat))

 

# Filter policies to only national or provincial level policies
sub_data = sub_data %>%
  filter(init_country_level %in% c("National", "Provincial")) %>%
  filter(target_country %in% countries) 



# remove policies that are targeted toward particular populations
sub_data = sub_data %>% filter(grepl("No special population targeted", type_who_gen)|
                                 is.na(type_who_gen))

# select gatherings that restrict gatherings of 500 or more people
sub_data= sub_data %>%
  mutate(type_mass_gathering = as.numeric(str_trim(type_mass_gathering)))


sub_data = sub_data %>% filter( type %in% c("Lockdown", "Closure and Regulation of Schools", "Social Distancing")|
                                  type_mass_gathering >= 500|
                            is.na(type_mass_gathering) & type == 'Restrictions of Mass Gatherings')


 
# remove policies that are voluntary
sub_data = sub_data %>% filter(compliance %!in% "Voluntary/Recommended but No Penalties")


# add rows for other mask wearing policies and mask in public/commercial spaces if there is a policy for masks everwhere
sub_data = sub_data %>% mutate_cond(type_sub_cat == 'Wearing Masks in all public spaces/everywhere',
                         type_sub_cat = 'Wearing Masks in all public spaces/everywhere,Wearing Masks inside public or commercial building,Other Mask Wearing Policy')
 
sub_data = sub_data %>% separate_rows(type_sub_cat, sep = ',')
 
# keep orphaned records for now --- investigate later
(orphans = names(which(unlist(lapply(split(sub_data$entry_type, sub_data$policy_id), function(x){
  all(unique(x) == 'update')}))) == TRUE) %>% unique()) 
 
sub_data = sub_data %>% mutate(orphanDum = ifelse(policy_id %in% orphans, 1, 0))

 
# replace end date with max last end date for now if end date is missing
# and remove observations that come after that end date
# end_date = max(sub_data$date_end, na.rm = TRUE)
end_date = "2020-07-22"
sub_data = sub_data %>% 
   dplyr:::mutate(date_end = if_else(is.na(date_end), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
   filter(date_start<=as.Date(end_date, "%Y-%m-%d"))


# add some policies manually

source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona_manual.R') 

sub_data = rbind(sub_data, swiss_lockdown)
sub_data = rbind(sub_data, swiss_mass_gathering)
sub_data = rbind(sub_data, france_mass_gathering)
sub_data = rbind(sub_data, france_school_1)
sub_data = rbind(sub_data, france_school_2)
sub_data = rbind(sub_data, italy_mass_gathering)



# check type sub cat
sub_data %>% filter(is.na(type_sub_cat))

# save raw data 

saveRDS(sub_data, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_raw.RDS")
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_raw.RDS")

sub_data %>% filter(type == 'Social Distancing' & init_country_level == 'National') %>% data.frame


-.44+.25# fill in appropriate province names
regions = read_csv("WEP_analysis/data/CoronaNet/country_region_clean.csv")
regions = regions %>% dplyr::: filter(Country %in% countries) %>% dplyr:::select(-ISO2)
regions$`0` = regions$Country
regions = regions %>% gather(reg_num, region, -Country)
regions = regions %>% dplyr:::filter(!is.na(region))

regions = regions %>% 
  group_by(Country) %>%
  tidyr:::expand(gov = region, target_province = region) %>%
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
 


# # remove 'end of policy' policies
# sub_data = sub_data %>% filter(update_type %!in% 'End of Policy')




# save raw data 
saveRDS(sub_data, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")


# -------------------
# reshape CoronaNet data from raw data to long format for H1
# -------------------

# load data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")
 
## reshape coronanet data to long format by  date-gov-target_province-type

# make new variable called gov which is the same as 'province' but takes on the name of country if it is a national level policy
sub_data = sub_data %>% mutate(gov = ifelse(init_country_level == "National" & is.na(province), country, province)) %>% 
  dplyr:::select(-province)



# collapse over policy_id so that you don't double count updates
sub_data = sub_data %>% group_by(policy_id) %>%
  arrange(date_start, entry_type) %>%
  mutate( date_start = min(date_start, na.rm = TRUE),
         date_end = max(date_end, na.rm = TRUE),
         # type_sub_cat = paste(unique(type_sub_cat), collapse = '; '), # # aggregate sub_data by type such that you collapse over type_sub_cat
         ) %>%
  distinct(gov, init_country_level, target_province, type_sub_cat, .keep_all = TRUE) %>%
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
         record_id = paste(unique(record_id), collapse = ';'),

  ) %>% 
  distinct(gov, target_province, type_sub_cat, date, .keep_all = TRUE ) %>%
  ungroup()

# reshape such that each sub type gets its own column
# test = sub_data %>% 
#   spread(type_sub_cat, policy_count) %>% 
#   
#   group_by(gov, target_province, date) %>%
#   mutate(policy_id = paste(unique(policy_id), collapse = ';'),
#          record_id = paste(unique(record_id), collapse = ';')
#          'All/Unspecified mass gatherings' = unique(na.omit('All/Unspecified mass gatherings'))
#   ) %>% 
#   distinct(gov, target_province,  date, .keep_all = TRUE ) %>%
#   ungroup()


 
### make data frame for full long format by date-gov-target_province-type
dframe = expand_grid(date = seq(as.Date("2020-01-01", "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), by = "day"), 
                     regions, 
                     type_sub_cat = c("Primary Schools (generally for children ages 10 and below)", 
                               "Lockdown", 
                               "All/Unspecified mass gatherings", 
                               "Wearing Masks in all public spaces/everywhere", 
                               "Wearing Masks inside public or commercial building",
                               "Other Mask Wearing Policy" 
                               
))


# merge data
data_long = merge(dframe, sub_data, by = c( "date",  'country', "gov", "target_province", "type_sub_cat"), all.x = TRUE)
 
# fill in  for record_id, policy_id 
# https://stackoverflow.com/questions/35362575/equivalent-to-cumsum-for-string-in-r
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  arrange(country, gov, target_province, type_sub_cat, date) %>%
  mutate(record_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(record_id), "", record_id)), accumulate = TRUE)))),
         policy_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(policy_id), "", policy_id)), accumulate = TRUE))))) %>%
  ungroup()

# remove duplicate record_id and policy_ids
data_long = data_long %>% group_by(country, gov, target_province, type) %>% 
  mutate(policy_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(policy_id, ';'), function(x) unique(x)))), collapse = ';')),
         record_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(record_id, ';'), function(x) unique(x)))), collapse = ';')))

# fill in type and type_sub_cat and init_country_level
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  fill( type, type_sub_cat, init_country_level, entry_type, target_country, target_geog_level, .direction = 'down')%>%
  ungroup()

# create measure of policy incidence (policy_count) and binary variable of whether a policy exists (policy_dum) by gov, target_province, date and type
data_long = data_long %>% 
  group_by(gov, target_province, type_sub_cat ) %>%
  dplyr:::mutate(policy_dum = case_when(is.na(date_type)~0,
                                        date_type == 'date_start' ~ as.numeric(policy_count),
                                        date_type == 'date_end'  ~  -1*as.numeric(policy_count) ,
                                        TRUE ~ 0))%>%
  mutate(policy_count = cumsum(policy_dum)) %>%
  mutate(policy_dum = ifelse(policy_count>0, 1, 0)) %>%
  ungroup()  



 
# try to fix error, later but so far should be these policies that have negative number for policy_count
# 4231455;5762826;6739782
# 2421846;6947301
 # 4176590;9227362

data_long = data_long %>% mutate(policy_count = ifelse(policy_count == -1, 0, policy_count))

# save reshaped data 
saveRDS(data_long, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long.RDS")

data_long %>% filter(country == 'Germany' & date == '2020-04-30' & type == 'Restrictions of Mass Gatherings') %>% data.frame()



# -------------------
# reshape CoronaNet data from raw data to long format for H3
# -------------------

# load data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")


sub_data = sub_data  %>% 
  filter(record_id %!in% 'manual_add_1')  %>%         # remove swiss national lockdown
  filter(policy_id %!in% c(1186194, 1445515))  %>%    # remove campania and calabria lockdowns
  filter(policy_id %!in% c(5764479))  %>%                # remove north westphalia lockdoown
  filter(policy_id %!in% c(5139191)) 

## reshape coronanet data to long format by  date-gov-target_province-type

# make new variable called gov which is the same as 'province' but takes on the name of country if it is a national level policy
sub_data = sub_data %>% mutate(gov = ifelse(init_country_level == "National" & is.na(province), country, province)) %>% 
  dplyr:::select(-province)


# collapse over policy_id so that you don't double count updates
sub_data = sub_data %>% group_by(policy_id) %>%
  arrange(date_start, entry_type) %>%
  mutate( date_start = min(date_start, na.rm = TRUE),
          date_end = max(date_end, na.rm = TRUE),
          # type_sub_cat = paste(unique(type_sub_cat), collapse = '; '), # # aggregate sub_data by type such that you collapse over type_sub_cat
  ) %>%
  distinct(gov, init_country_level, target_province, type_sub_cat, .keep_all = TRUE) %>%
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
         record_id = paste(unique(record_id), collapse = ';'),
         
  ) %>% 
  distinct(gov, target_province, type_sub_cat, date, .keep_all = TRUE ) %>%
  ungroup()

# reshape such that each sub type gets its own column
# test = sub_data %>% 
#   spread(type_sub_cat, policy_count) %>% 
#   
#   group_by(gov, target_province, date) %>%
#   mutate(policy_id = paste(unique(policy_id), collapse = ';'),
#          record_id = paste(unique(record_id), collapse = ';')
#          'All/Unspecified mass gatherings' = unique(na.omit('All/Unspecified mass gatherings'))
#   ) %>% 
#   distinct(gov, target_province,  date, .keep_all = TRUE ) %>%
#   ungroup()



### make data frame for full long format by date-gov-target_province-type
dframe = expand_grid(date = seq(as.Date("2019-12-31", "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), by = "day"), 
                     regions, 
                     type_sub_cat = c("Primary Schools (generally for children ages 10 and below)", 
                                      "Lockdown", 
                                      "All/Unspecified mass gatherings", 
                                      "Wearing Masks in all public spaces/everywhere", 
                                      "Wearing Masks inside public or commercial building",
                                      "Other Mask Wearing Policy" 
                                      
                     ))


# merge data
data_long = merge(dframe, sub_data, by = c( "date",  'country', "gov", "target_province", "type_sub_cat"), all.x = TRUE)

# fill in  for record_id, policy_id 
# https://stackoverflow.com/questions/35362575/equivalent-to-cumsum-for-string-in-r
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  arrange(country, gov, target_province, type_sub_cat, date) %>%
  mutate(record_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(record_id), "", record_id)), accumulate = TRUE)))),
         policy_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(policy_id), "", policy_id)), accumulate = TRUE))))) %>%
  ungroup()

# remove duplicate record_id and policy_ids
data_long = data_long %>% group_by(country, gov, target_province, type) %>% 
  mutate(policy_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(policy_id, ';'), function(x) unique(x)))), collapse = ';')),
         record_id = gsub("^;", "", paste(unique(unlist(lapply(str_split(record_id, ';'), function(x) unique(x)))), collapse = ';')))

# fill in type and type_sub_cat and init_country_level
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  fill( type, type_sub_cat, init_country_level, entry_type, target_country, target_geog_level, .direction = 'down')%>%
  ungroup()

# create measure of policy incidence (policy_count) and binary variable of whether a policy exists (policy_dum) by gov, target_province, date and type
data_long = data_long %>% 
  group_by(gov, target_province, type_sub_cat ) %>%
  dplyr:::mutate(policy_dum = case_when(is.na(date_type)~0,
                                        date_type == 'date_start' ~ as.numeric(policy_count),
                                        date_type == 'date_end'  ~  -1*as.numeric(policy_count) ,
                                        TRUE ~ 0))%>%
  mutate(policy_count = cumsum(policy_dum)) %>%
  mutate(policy_dum = ifelse(policy_count>0, 1, 0)) %>%
  ungroup()  


# add back type

data_long = data_long %>% mutate(
  type = case_when(
    type_sub_cat == "All/Unspecified mass gatherings"  ~ 'Restrictions of Mass Gatherings', 
    type_sub_cat == "Lockdown" ~ 'Lockdown', 
    type_sub_cat == "Primary Schools (generally for children ages 10 and below)"  ~ 'Closure and Regulation of Schools',
    type_sub_cat == "Wearing Masks in all public spaces/everywhere"  ~ "Mask Wearing", 
    type_sub_cat ==  "Other Mask Wearing Policy" ~ "Mask Wearing", 
    type_sub_cat == "Wearing Masks inside public or commercial building" ~ "Mask Wearing") )

 
# try to fix error, later but so far should be these policies that have negative number for policy_count
# 4231455;5762826;6739782
# 2421846;6947301
# 4176590;9227362

data_long = data_long %>% mutate(policy_count = ifelse(policy_count == -1, 0, policy_count))

# save reshaped data 
saveRDS(data_long, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long_h3.RDS")

