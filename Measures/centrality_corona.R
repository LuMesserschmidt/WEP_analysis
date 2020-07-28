## things to do
# clean geographical target of a policy 
#  * sub_data %>% filter(is.na(target_country)) %>% select(target_geog_level) %>% pull %>% table()
#  * clean province names
# clean lockdown/quarantine
# resolve orphans
# incorporate updates strengthening/relaxing
# DONE: clean whether the target is outside of the country or not
# DONE: incorporate updates; update/end
# DONE incorporate target_same variable


# load packages
library(qdapRegex)
library(readr)
library(magrittr)
library(igraph)
library(tidyr)
library(data.table)
library(dplyr)
library(stringr)
'%!in%' <- function(x,y)!('%in%'(x,y))

mutate_cond <-
  function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition,] <- .data[condition,] %>% mutate(...)
    .data
  }

# set working directory
setwd('~/Documents/coronaNet/corona_private/')

# load data
qualtrics = read_csv("./data/CoronaNet/coronanet_release.csv")

# -------------------
# clean  CoronaNet data
# -------------------
# subset data to 
    # certain countries
    # quarantines and lockdowns
    # national and provincial level policies
countries = c("Switzerland", "Germany", "France", "Italy") 
sub_data = clean_data %>% dplyr:::filter(country %in% countries
                                         & type %in% c("Lockdown", "Quarantine")) %>%
                                filter(init_country_level %in% c("National", "Provincial")) %>%
                                filter(target_country %in% countries)


# incorporate data for end date for update --- end of policy
sub_data = sub_data %>% group_by(policy_id) %>%
    arrange(desc(date_end)) %>% 
    mutate(date_end = ifelse(any(update_type %in% "End of Policy"), rep(date_end[1], n()), date_end),
           date_end = lubridate:::as_date(date_end))

# replace end date with max last end date for now if end date is missing
  # and remove observations that come after that end date
end_date = max(sub_data$date_end, na.rm = TRUE)
sub_data = sub_data %>% dplyr:::mutate(date_end = dplyr:::if_else(is.na(date_end), as.Date(end_date, "%Y-%m-%d"), date_end)) %>%
                        filter(date_start<=as.Date(end_date, "%Y-%m-%d"))

 
# reshape data from wide to long format for dates
 
sub_data = sub_data %>% dplyr:::select(record_id, policy_id, type, type_sub_cat, init_country_level, country, province,  target_country, target_geog_level, target_province, date_start, date_end, entry_type) %>% 
          gather(date_type, date, -record_id, -policy_id, -type, -type_sub_cat,-init_country_level, -country, -province,-target_country, -target_geog_level, -target_province, -entry_type)


# make new variable called gov which is the same as 'province' but takes on the name of country if it is a national level policy
sub_data = sub_data %>% mutate(gov = ifelse(init_country_level == "National" & is.na(province), country, province)) %>% select(-province)

# remove orphaned records for now
(orphans = names(which(unlist(lapply(split(sub_data$entry_type, sub_data$policy_id), function(x){
  all(unique(x) == 'update')}))) == TRUE))

sub_data = sub_data %>% dplyr:::filter(policy_id %!in% orphans)

# aggregate by type
sub_data = sub_data %>% group_by(gov, type, date) %>%
  mutate(type_sub_cat = paste(unique(type_sub_cat), collapse = '; ')) %>% 
  distinct(gov, type, date, .keep_all = TRUE )
table(test$type_sub_cat) 


# -------------------
# transform data from event data into long format
# -------------------

regions = read_csv("data/regions/country_region_clean.csv")
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



sub_data = sub_data %>% separate_rows(target_province, sep = ',')

# merge data
data_long = merge(dframe, sub_data, by = c( "date",  'country', "gov", "target_province", "type"), all.x = TRUE)

# write a cumsum fill for record id and policy id
# https://stackoverflow.com/questions/35362575/equivalent-to-cumsum-for-string-in-r
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  mutate(record_id =  gsub("\\s", ",", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(record_id), "", record_id)), accumulate = TRUE)))),
         policy_id =  gsub("\\s", ",", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(policy_id), "", policy_id)), accumulate = TRUE)))))

# fill in record_id, policy_id, type and type_sub_cat and init_country_level
data_long = data_long %>% 
  group_by(country, gov, target_province, type_sub_cat) %>%
  fill( type,  init_country_level, entry_type, target_country, target_geog_level, .direction = 'down')


# create measure of policy incidence by country, region and type
data_long = data_long %>% 
  dplyr:::mutate(policy_dum =  ifelse(is.na(date_type), 0, ifelse(date_type == 'date_start', 1, -1))) %>%
  group_by(country, gov, target_province, type ) %>%
  mutate(policy_count = cumsum(policy_dum)) %>%
  mutate(policy_dum = ifelse(policy_count>0, 1, 0)) %>%
  ungroup()  

 
# -------------------
# create measure of policy centralization
# -------------------

type = c("Quarantine", "Lockdown" )

# separate policy centralization measure for each type separately
policyCentralization = do.call(rbind, lapply(countries, function(c){
  
  countrySlice = data_long %>% filter(country %in% c ) 
  nodes = countrySlice %>% select(gov) %>% distinct %>% pull
  nodes = c(nodes[which(nodes %in% c)], nodes[-which(nodes %in% c)])  %>% data.frame()
  
  rawTypeList = do.call(rbind, lapply(type, function(t){

    typeSlice = countrySlice %>% filter(type== t) %>% select(date, gov, target_province, policy_dum, policy_count)

    rawBaseList = do.call(rbind, lapply(unique(dframe$date), function (d){

      slice = typeSlice %>% filter(date %in% d)
      edges = slice %>% select(gov, target_province, policy_dum, policy_count)
      edges = rbind(edges[which(edges$gov %in% c),], edges[which(edges$gov %!in% c),])
      edges$policy_theory_dum =  c(rep(1, dim(edges)[1]/2), rep(0, dim(edges)[1]/2))    
      
      network_theory = graph_from_data_frame(edges[which(edges$policy_theory_dum ==1),], nodes, directed = TRUE)
      network_actual = graph_from_data_frame(edges[which(edges$policy_dum ==1),], nodes, directed = TRUE)
      
      centDegTheory = centr_degree(network_theory)$centralization
      centDegRaw = centr_degree(network_actual)$centralization
      centDegStd = centDegRaw/centDegTheory
      
      hubTheory = hub_score(network_theory)$value
      hubRaw = hub_score(network_actual)$value
      hubStd = hubRaw/hubTheory
      
      # hubTheoryWt = hub_score(network_theory, weights = edges[which(edges$policy_theory_dum ==1),'policy_count'] %>% pull)$value
        hubRawWt = hub_score(network_actual, weights = edges[which(edges$policy_dum ==1),'policy_count'] %>% pull)$value
      # hubStdWt = hubRawWt/hubTheoryWt
        
       subGraphTheory = subgraph_centrality(network_theory)[1]
       subGraphRaw = subgraph_centrality(network_actual)[1]
       subGraphStd = subGraphRaw/subGraphTheory
      
      hubWt = hubRawWt/hubRaw
      return(data.frame(centDegTheory, centDegRaw, centDegStd, hubTheory,  hubRaw,  hubStd, hubWt, subGraphTheory,subGraphRaw , subGraphStd, type = t, country = c))
  })) 
    
  rawBaseList$date = unique(dframe$date)
 
  return(rawBaseList)

  }))
  return(rawTypeList)
  }))


saveRDS(policyCentralization, file = "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 

# cor(policyCentralization %>% select(centDegStd, hubStd, hubWt, subGraphStd), use = 'complete.obs')
