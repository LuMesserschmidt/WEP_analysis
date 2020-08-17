# clean coronanet data for WEP countries
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

# -------------------
# clean  CoronaNet data
# -------------------
countries = c("Switzerland", "Germany", "France", "Italy") 

# Filter policies to WEP countries
sub_data = qualtrics %>% dplyr:::filter(country %in% countries) 

# retrieve and add 'missing' policies
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona_missing.R')
sub_data = rbind(sub_data %>% mutate(missingDum = 0), missing)

# check and recode 'other' policy data
source('WEP_analysis/R_Code/cleanData/1_cleanCorona_source/clean_corona.R')


# if the policy type is 'health resources', make the date end one day after the date start
# if the policy type is 'public awareness campaigns' or 'anti disinformaito campains', make the date end 14 days after the date start
sub_data = sub_data %>% mutate(date_end = dplyr:::if_else(is.na(date_end) & type == 'Health Resources', date_start + days(1), date_end),
                               date_end = dplyr:::if_else(is.na(date_end) & type %in% c( 'Public Awareness Campaigns', 'Anti-Disinformation Measures'), date_start + days(14), date_end))

 
# fill in target country with country if not an external border restriction  
sub_data = sub_data %>% mutate(target_country = ifelse(is.na(target_country) &
                                                         type !='External Border Restrictions', country, target_country))
 

# fill in target_country by hand if type is external border restriction and target_country is na

sub_data = sub_data %>% mutate(target_country = case_when(
  policy_id == 8387693 ~ 'All countries',
  policy_id == 9415937 ~ 'Schengen Area',
  TRUE ~target_country
))


# keep orphaned records for now --- investigate later
(orphans = names(which(unlist(lapply(split(sub_data$entry_type, sub_data$policy_id), function(x){
  all(unique(x) == 'update')}))) == TRUE) %>% unique()) 


# sub_data = sub_data %>% filter(policy_id %!in% orphans)


# replace end date with max last end date for now if end date is missing
# and remove observations that come after that end date
# end_date = max(sub_data$date_end, na.rm = TRUE)
end_date = "2020-07-15"
sub_data = sub_data %>% dplyr:::mutate(date_end = if_else(is.na(date_end), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
                       # dplyr:::mutate(date_end = if_else(date_end > as.Date(end_date, "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
                                      filter(date_start<=as.Date(end_date, "%Y-%m-%d"))
 
# Filter policies to only national or provincial level policies
sub_data = sub_data %>%
  filter(init_country_level %in% c("National", "Provincial")) %>%
  filter(target_country %in% countries) 


# select only desired policies
policies = c('Lockdown', 'Closure and Regulation of Schools', 'Restrictions of Mass Gatherings')
sub_data = sub_data %>% filter(type %in% policies| 
                                 grepl('Wearing Mask|Mask Wearing', type_sub_cat))


sub_data = sub_data %>% filter(type_sub_cat %!in%  c('Annually recurring event allowed to occur with certain conditions',
                                                     'Attendance at religious services restricted (e.g. mosque/church closings)',
                                                     'Cancellation of a recreational or commercial event',
                                                     'Cancellation of an annually recurring event',
                                                     'Events at private residencies restricted (e.g. parties held at home)',
                                                     'Higher education institutions (i.e. degree granting institutions)',
                                                     'Postponement of a recreational or commercial event',
                                                     'Postponement of an annually recurring event',
                                                     'Prison population reduced (e.g. early release of prisoners)'))


## remove policy
# February 28,2020 Switzerland Council of Basel is cancelling the carnival of Basel ( target date 02-03-20 till 04-03-20)
sub_data = sub_data %>% filter(record_id %!in% "R_3kzcET6o5pQyvP0NA")

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
 

# # collapse by policy_id
# test = sub_data %>% group_by(policy_id) %>%
#               mutate(date_start = min(date_start),
#                      date_end = max(date_end)) %>%
#               distinct(policy_id,
#                       date_start, 
#                        date_end, 
#                        country, 
#                        init_country_level, 
#                        province, 
#                        type, 
#                        type_sub_cat, 
#                        type_mass_gathering, 
#                        type_who_gen, 
#                        school_status, 
#                        target_country, 
#                        target_province, 
#                        target_city,
#                        target_geog_level, 
#                        update_level,
#                       # target_who_what,
#                       # target_direction,
#                       # compliance,
#                       # enforcer
#                        ) %>% ungroup()

 
saveRDS(sub_data, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")
 