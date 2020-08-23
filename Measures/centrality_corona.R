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

 
# load data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long.RDS")

# -------------------
# create measure of policy centralization
# -------------------
# type = c('Lockdown', 'Closure and Regulation of Schools', 'Restrictions of Mass Gatherings', "Social Distancing")
type = c('Lockdown',   'Restrictions of Mass Gatherings' )
 


# separate policy centralization measure for each type separately
policyCentralization = do.call(rbind, lapply(countries, function(c){
  
  countrySlice = data_long %>% filter(country %in% c ) 
  nodes = countrySlice %>% dplyr:::select(gov) %>% distinct %>% pull
  nodes = c(nodes[which(nodes %in% c)], nodes[-which(nodes %in% c)])  
  
  rawTypeList = do.call(rbind, lapply(type, function(t){

    typeSlice = countrySlice %>% filter(type== t) %>% dplyr:::select(date, gov, target_province, policy_dum, policy_count)

    rawBaseList = do.call(rbind, lapply(unique(dframe$date), function (d){

      slice = typeSlice %>% filter(date %in% d)
      edges = slice %>% dplyr:::select(gov, target_province, policy_dum, policy_count) %>% data.frame()
      edges = rbind(edges[which(edges$gov %in% c),], edges[which(edges$gov %!in% c),])
      edges$policy_theory_dum =  c(rep(1, dim(edges)[1]/2), rep(0, dim(edges)[1]/2))    
      
      # rename gov province names with 'Gov' in front of province name
      edges$gov[which(edges$gov != c)] = paste0('Gov ', edges$gov[which(edges$gov != c)])
      nodes = c(nodes, edges$gov[which(edges$gov != c)])%>% data.frame()
      
      network_theory = graph_from_data_frame(edges[which(edges$policy_theory_dum ==1),], nodes, directed = TRUE)
      network_actual = graph_from_data_frame(edges[which(edges$policy_dum ==1),], nodes, directed = TRUE)
      
      centDegTheory = centr_degree(network_theory)$centralization
      centDegRaw = centr_degree(network_actual)$centralization
      centDegStd = centDegRaw/centDegTheory
      
      hubTheory = hub_score(network_theory)$value
      hubRaw = hub_score(network_actual)$value
      hubStd = hubRaw/hubTheory

      hubTheoryWt = hub_score(network_theory, weights = edges[which(edges$policy_theory_dum ==1),'policy_count'])$value
      hubRawWt = hub_score(network_actual, weights = edges[which(edges$policy_dum ==1),'policy_count'] )$value
      hubStdWt = hubRawWt/hubTheoryWt

      subGraphTheory = subgraph_centrality(network_theory)[1]
      subGraphRaw = subgraph_centrality(network_actual)[1]
      subGraphStd = subGraphRaw/subGraphTheory

     hubWt = hubRawWt/hubRaw
     return(data.frame(centDegTheory, centDegRaw, centDegStd, hubTheory,  hubRaw,  hubStd, hubWt, subGraphTheory,subGraphRaw , subGraphStd, type = t, country = c))
     # return(data.frame(centDegTheory, centDegRaw, centDegStd, type = t, country = c))
      
      })) 
    
  rawBaseList$date = unique(dframe$date)
 
  return(rawBaseList)

  }))
  return(rawTypeList)
  }))


policyCentralization %>% filter(country == 'Germany' & type == 'Restrictions of Mass Gatherings') %>%
  dplyr:::select(centDegRaw, centDegTheory, centDegStd, date) %>%
  data.frame()
saveRDS(policyCentralization, file = "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 

# cor(policyCentralization %>% select(centDegStd, hubStd, hubWt, subGraphStd), use = 'complete.obs')
sub_data %>% filter(record_id %in% c("R_3KAZQ38rjTAOXJrCt",
                                     "R_3KAZQ38rjTAOXJrCu",
                                     "R_3KAZQ38rjTAOXJrDp",
                                     "R_217H9o9cRt6HRNnCt",
                                     "R_217H9o9cRt6HRNnCu",
                                     "R_217H9o9cRt6HRNnDp",
                                     "R_1H7gEKH44DEZOxJCt",
                                     "R_1H7gEKH44DEZOxJCu",
                                     "R_1H7gEKH44DEZOxJDp",
                                     "R_2xWAwvGSZ6opF08Ct",
                                     "R_2xWAwvGSZ6opF08Cu",
                                     "R_2xWAwvGSZ6opF08Dp",
                                     "R_QhSkmhJwtpfaRodCt",
                                     "R_QhSkmhJwtpfaRodCu",
                                     "R_QhSkmhJwtpfaRodDp",
                                     "R_2anxbcpERZkuqaVCt",
                                     "R_2anxbcpERZkuqaVCu",
                                     "R_2anxbcpERZkuqaVDp"))


# create a measure of how many updates a particular policy has so that you don't double count them later
sub_data = sub_data %>% group_by(record_id, date, target_province) %>%
  mutate(update_count = length(which(entry_type =='update'))) %>%
  ungroup()
# aggregate sub_data by gov, target_province, type and date
# need to do this so that you don't get duplicates when you merge with dframe
# the variable policy_count counts how many policies there were for a particular gov, target_provice, type and date, subtracting policy updates
test= sub_data %>% group_by(gov, target_province, type, date) %>%
  mutate(policy_count = length(unique(policy_id)) - update_count,
         policy_dum = 1,
         policy_id = paste(unique(policy_id), collapse = ','),
         record_id = paste(unique(record_id), collapse = ',')
  ) %>% 
  distinct(gov, target_province, type, date, .keep_all = TRUE ) %>%
  ungroup()



# aggregate by type
# sub_data = sub_data %>% group_by(gov, type, date) %>%
#   mutate(type_sub_cat = paste(unique(type_sub_cat), collapse = '; ')) %>% 
#   distinct(gov, type, date, .keep_all = TRUE ) %>%
#   ungroup()