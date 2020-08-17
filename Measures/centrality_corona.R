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


# double check that there are no false negatives when you reduce by sub-type

#swiss health monitoring national miscoded, 
# why still missing data after 7-7??


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

 
sub_data %>% filter(policy_id %in% c(9294153, 1644132)) %>% data.frame()

# load data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean.RDS")

# -------------------
# reformat CoronaNet data
# -------------------
# reshape data from wide to long format for dates
sub_data = sub_data %>% dplyr:::select(policy_id, record_id, entry_type, update_type,  update_level, type, type_sub_cat, school_status, init_country_level, country, province,  target_country, target_geog_level, target_province, date_start, date_end)%>% 
  gather(date_type, date, -policy_id,-record_id, -entry_type, -update_type, -update_level,  -type, -type_sub_cat, -school_status,-init_country_level, -country, -province,-target_country, -target_geog_level, -target_province)


# make new variable called gov which is the same as 'province' but takes on the name of country if it is a national level policy
sub_data = sub_data %>% mutate(gov = ifelse(init_country_level == "National" & is.na(province), country, province)) %>% select(-province)


# aggregate by type
sub_data = sub_data %>% group_by(policy_id) %>%
  mutate(type_sub_cat = paste(unique(type_sub_cat), collapse = '; ')) %>% 
  distinct(gov, type, date, .keep_all = TRUE ) %>%
  ungroup()

# aggregate by gov, target_province, type and date
sub_data= sub_data %>% group_by(gov, target_province, type, date) %>%
  mutate(policy_count = length(unique(policy_id)),
         policy_dum = 1,
         policy_id = paste(unique(policy_id), collapse = ';'),
         record_id = paste(unique(record_id), collapse = ';')) %>% 
  distinct(gov, target_province, type, date, .keep_all = TRUE ) %>%
  ungroup()

 
 
# aggregate by type
# sub_data = sub_data %>% group_by(gov, type, date) %>%
#   mutate(type_sub_cat = paste(unique(type_sub_cat), collapse = '; ')) %>% 
#   distinct(gov, type, date, .keep_all = TRUE ) %>%
#   ungroup()



# -------------------
# transform data from event data into long format
# -------------------
countries = c("Switzerland", "Germany", "France", "Italy") 
policies = c('Lockdown', 'Closure and Regulation of Schools', 'Restrictions of Mass Gatherings')

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
end_date = "2020-07-15"
dframe = expand_grid(date = seq(as.Date("2019-12-31", "%Y-%m-%d"), as.Date(end_date, "%Y-%m-%d"), by = "day"), regions , type = c("Social Distancing", policies))

# sub_data = sub_data %>% filter(type == 'Social Distancing')
# dframe = expand_grid(date = seq(as.Date("2019-12-31"), as.Date(end_date), by = "day"), regions , type = c("Social Distancing"))


# merge data
data_long = merge(dframe, sub_data, by = c( "date",  'country', "gov", "target_province", "type"), all.x = TRUE)

# https://stackoverflow.com/questions/35362575/equivalent-to-cumsum-for-string-in-r
data_long = data_long %>% 
  group_by(country, gov, target_province, type) %>%
  arrange(country, gov, target_province, type, date) %>%
  mutate(record_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(record_id), "", record_id)), accumulate = TRUE)))),
         policy_id =  gsub("\\s", ";", rm_white_multiple(str_trim(Reduce(paste, as.character(ifelse(is.na(policy_id), "", policy_id)), accumulate = TRUE))))) %>%
  ungroup()


# remove duplicate record_id and policy_ids
data_long = data_long %>% group_by(country, gov, target_province, type) %>% 
  mutate(policy_id = paste(unique(unlist(lapply(str_split(policy_id, ';'), function(x) unique(x)))), collapse = ';'),
         record_id = paste(unique(unlist(lapply(str_split(record_id, ';'), function(x) unique(x)))), collapse = ';'))

# fill in type and type_sub_cat and init_country_level
data_long = data_long %>% 
  group_by(country, gov, target_province, type) %>%
  fill( type,  init_country_level, entry_type, target_country, target_geog_level, .direction = 'down')%>%
  ungroup()

# create measure of policy incidence by country, region and type
data_long = data_long %>% 
  dplyr:::mutate(policy_dum_1 =  case_when(is.na(date_type) ~ 0, 
                                           date_type == 'date_start' ~ 1,
                                           date_type == 'date_end' ~ -1,
                                           TRUE ~ 0),
                 policy_dum_2 = case_when(is.na(date_type)~0,
                                          date_type == 'date_start' ~ policy_dum,
                                          date_type == 'date_end' ~ policy_dum -1,
                                          TRUE ~ 0)) %>%
  group_by(country, gov, target_province, type ) %>%
  mutate(policy_count = cumsum(policy_dum)) %>%
  mutate(policy_dum = ifelse(policy_count>0, 1, 0)) %>%
  ungroup()  

unique(data_long$policy_count) 
sub_data[which(duplicated(sub_data[, c( "date",  'country', "gov", "target_province", "type")])),c( "date",  'country', "gov", "target_province", "type")]

sub_data %>% filter(date == as.Date("2020-05-08", "%Y-%m-%d") & gov == "Saarland" & target_province == 'Saarland' & type == "Restrictions of Mass Gatherings")


qualtrics %>% filter(policy_id %in% c(9887956, 8218737, 4425840)) %>% arrange(policy_id, date_start) %>% select(policy_id, date_start, description)  %>% data.frame()
table(data_long$update_type)
table(sub_data$update_type)
max(dframe$date)
sub_data %>% filter(update_type == 'End of Policy') %>% select(date) %>% arrange(date) %>% data.frame()
sub_data %>% filter(update_type == 'Change of Policy') %>% select(date) %>% arrange(date) %>% data.frame()
table(sub_data$update_type)
data_long %>% mutate()
 
data_long %>% filter(!is.na(record_id) & date_type == 'date_end') %>% 
  select(policy_id, country, gov, target_province, type, date, date_type, record_id)

data_long %>% filter(policy_id == 9228860)%>% 
  select(policy_id, country, gov, target_province, type, date, date_type, update_level, school_status)
 
data_long %>% filter(country == 'Switzerland' &
                       type == 'Closure and Regulation of Schools' &
                       gov == 'Neuchatel' &
                       target_province == 'Neuchatel' &
                       date > as.Date("2020-03-01", "%Y-%m-%d") & date< as.Date("2020-07-05", "%Y-%m-%d") ) %>%
  select(policy_id, date, country, gov, target_province,   type, date_type) %>% data.frame()


data_long %>% filter(country == 'Switzerland' &
                       type == 'Closure and Regulation of Schools' &
                       gov == 'Neuchatel' &
                       target_province == 'Neuchatel' &
                       date > as.Date("2020-03-01", "%Y-%m-%d") & date< as.Date("2020-07-05", "%Y-%m-%d") ) %>%
  select(date, country, gov, target_province, policy_dum,  type, date_type) %>% data.frame()




sub_data %>% filter(policy_id == 47515)%>% 
  select(policy_id, country, gov, target_province, type, date, date_type, update_level, school_status)

data_long %>% filter(!is.na(policy_id))

data_long %>% filter(grepl("1316113|860229|1010075|4126406", policy_id)) %>% select(policy_id) %>% tail()
test %>% filter(policy_id != "") %>% head()
test[126720, c('policy_id', 'policy_id_2')]
str_split(c("", ""), ';')

lapply(str_split("", ';'), function(x) unique(x))

data_long$policy_id %>% head()
foo = data_long %>% filter(grepl('1010075;4126406;1316113;860229;741217;4407175;741217;1010075;1316113;4126406;860229', policy_id)) 

foo$policy_id
unlist(lapply(str_split('1010075;4126406;1316113;860229;741217;4407175;741217;1010075;1316113;4126406;860229', ';'), unique))


foo %>% data.frame()
table(test$policy_id_2)

# write a cumsum fill for record id and policy id


 table(data_long$policy_dum)
 table(data_long$policy_count)
data_long %>% filter(policy_count == 1) %>% head() %>% data.frame()


data_long %>% filter(policy_count < 0) %>% select(date, country, gov, target_province, policy_id) %>% data.frame()
table(data_long$policy_count)
data_long %>% filter(country == 'Switzerland' &
                       type == 'Social Distancing' &
                       gov == 'Switzerland' &
                       date > as.Date("2020-04-20", "%Y-%m-%d") & date< as.Date("2020-04-23", "%Y-%m-%d") ) %>%
  select(date, country, gov, target_province, policy_dum, type) %>% data.frame()




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
