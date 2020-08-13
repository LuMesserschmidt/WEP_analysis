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
# setwd('~/Dropbox/West European Politics Corona Article/')

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
sub_data = sub_data %>% mutate(date_end = ifelse(is.na(date_end) & type == 'Health Resources', date_start + days(1), date_end),
                               date_end = ifelse(is.na(date_end) & type %in% c( 'Public Awareness Campaigns', 'Anti-Disinformation Measures'), date_start + days(14), date_end))

 
# fill in target country with country if not an external border restriction  
sub_data = sub_data %>% mutate(target_country = ifelse(is.na(target_country) &
                                                         type %!in% c('External Border Restrictions'), country, target_country))


# Filter policies to only national or provincial level policies
sub_data = sub_data %>%
  filter(init_country_level %in% c("National", "Provincial")) %>%
  filter(target_country %in% countries)

 
# replace end date with max last end date for now if end date is missing
# and remove observations that come after that end date
# end_date = max(sub_data$date_end, na.rm = TRUE)
end_date = "2020-07-15"
sub_data = sub_data %>% dplyr:::mutate(date_end = ifelse(is.na(date_end), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
  filter(date_start<=as.Date(end_date, "%Y-%m-%d"))

# remove orphaned records for now --- investigate
(orphans = names(which(unlist(lapply(split(sub_data$entry_type, sub_data$policy_id), function(x){
  all(unique(x) == 'update')}))) == TRUE))

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
dframe = expand_grid(date = seq(as.Date("2019-12-31"), as.Date(end_date), by = "day"), regions , type = c("Quantine", "Lockdown"))

# expand/fill in 'target_province' with name of all provinces if it is a national level policy
sub_data = sub_data %>% mutate(target_province = 
                                 ifelse(country == 'Switzerland' & init_country_level == "National" & is.na(target_province), paste(regions[which(regions$country == 'Switzerland'), 'target_province'] %>% distinct %>% pull(), collapse = ',') , 
                                        ifelse(country == 'Germany' & init_country_level == "National" & is.na(target_province), paste(regions[which(regions$country == 'Germany'), 'target_province'] %>% distinct %>% pull(), collapse = ',') ,
                                               ifelse(country == 'France' & init_country_level == "National" & is.na(target_province), paste(regions[which(regions$country == 'France'), 'target_province']  %>% distinct%>% pull(), collapse = ',') , 
                                                      ifelse(country == 'Italy' & init_country_level == "National" & is.na(target_province), paste(regions[which(regions$country == 'Italy'), 'target_province']  %>% distinct%>% pull(), collapse = ',') , target_province)))))



saveRDS(sub_data, "WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")
 