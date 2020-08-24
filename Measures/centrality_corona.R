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
# type = c('Lockdown',   'Restrictions of Mass Gatherings' )
 
type_sub_cat = c("All/Unspecified mass gatherings", 
                   "Lockdown", 
                   "Primary Schools (generally for children ages 10 and below)",
                   "Wearing Masks in all public spaces/everywhere", 
                   "Other Mask Wearing Policy", 
                   "Wearing Masks inside public or commercial building")


# separate policy centralization measure for each sub type separately
policyCentralization = do.call(rbind, lapply(countries, function(c){
  
  countrySlice = data_long %>% filter(country %in% c ) 
  nodes = countrySlice %>% dplyr:::select(gov) %>% distinct %>% pull
  nodes = c(nodes[which(nodes %in% c)], nodes[-which(nodes %in% c)])  
  
  rawTypeList = do.call(rbind, lapply(type_sub_cat, function(t){
    
    typeSlice = countrySlice %>% filter(type_sub_cat== t) %>% dplyr:::select(date, gov, target_province, policy_dum, policy_count)
    
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
      return(data.frame(centDegTheory, centDegRaw, centDegStd, hubTheory,  hubRaw,  hubStd, hubWt, subGraphTheory,subGraphRaw , subGraphStd, type_sub_cat = t, country = c))
      # return(data.frame(centDegTheory, centDegRaw, centDegStd, type = t, country = c))
      
    })) 
    
    rawBaseList$date = unique(dframe$date)
    
    return(rawBaseList)
    
  }))
  return(rawTypeList)
}))

# add back type measure
policyCentralization = policyCentralization %>% mutate(
                              type = case_when(
                                type_sub_cat == "All/Unspecified mass gatherings"~ 'Restrictions of Mass Gatherings', 
                                type_sub_cat == "Lockdown" ~ 'Lockdown', 
                                type_sub_cat == "Primary Schools (generally for children ages 10 and below)" ~ 'Closure and Regulation of Schools',
                                type_sub_cat == "Wearing Masks in all public spaces/everywhere" ~ "Mask Wearing", 
                                type_sub_cat ==  "Other Mask Wearing Policy"~ "Mask Wearing", 
                                type_sub_cat == "Wearing Masks inside public or commercial building"~ "Mask Wearing"))


# average by type (needed for mask wearing)
policyCentralization = policyCentralization %>% select(-type_sub_cat) %>%
  group_by(country, type, date) %>%
  summarise_all(mean, na.rm = TRUE)


saveRDS(policyCentralization, file = "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 



# separate policy centralization measure for each type separately
# policyCentralization = do.call(rbind, lapply(countries, function(c){
#   
#   countrySlice = data_long %>% filter(country %in% c ) 
#   nodes = countrySlice %>% dplyr:::select(gov) %>% distinct %>% pull
#   nodes = c(nodes[which(nodes %in% c)], nodes[-which(nodes %in% c)])  
#   
#   rawTypeList = do.call(rbind, lapply(type, function(t){
#     
#     typeSlice = countrySlice %>% filter(type== t) %>% dplyr:::select(date, gov, target_province, policy_dum, policy_count)
#     
#     rawBaseList = do.call(rbind, lapply(unique(dframe$date), function (d){
#       
#       slice = typeSlice %>% filter(date %in% d)
#       edges = slice %>% dplyr:::select(gov, target_province, policy_dum, policy_count) %>% data.frame()
#       edges = rbind(edges[which(edges$gov %in% c),], edges[which(edges$gov %!in% c),])
#       edges$policy_theory_dum =  c(rep(1, dim(edges)[1]/2), rep(0, dim(edges)[1]/2))    
#       
#       # rename gov province names with 'Gov' in front of province name
#       edges$gov[which(edges$gov != c)] = paste0('Gov ', edges$gov[which(edges$gov != c)])
#       nodes = c(nodes, edges$gov[which(edges$gov != c)])%>% data.frame()
#       
#       network_theory = graph_from_data_frame(edges[which(edges$policy_theory_dum ==1),], nodes, directed = TRUE)
#       network_actual = graph_from_data_frame(edges[which(edges$policy_dum ==1),], nodes, directed = TRUE)
#       
#       centDegTheory = centr_degree(network_theory)$centralization
#       centDegRaw = centr_degree(network_actual)$centralization
#       centDegStd = centDegRaw/centDegTheory
#       
#       hubTheory = hub_score(network_theory)$value
#       hubRaw = hub_score(network_actual)$value
#       hubStd = hubRaw/hubTheory
#       
#       hubTheoryWt = hub_score(network_theory, weights = edges[which(edges$policy_theory_dum ==1),'policy_count'])$value
#       hubRawWt = hub_score(network_actual, weights = edges[which(edges$policy_dum ==1),'policy_count'] )$value
#       hubStdWt = hubRawWt/hubTheoryWt
#       
#       subGraphTheory = subgraph_centrality(network_theory)[1]
#       subGraphRaw = subgraph_centrality(network_actual)[1]
#       subGraphStd = subGraphRaw/subGraphTheory
#       
#       hubWt = hubRawWt/hubRaw
#       return(data.frame(centDegTheory, centDegRaw, centDegStd, hubTheory,  hubRaw,  hubStd, hubWt, subGraphTheory,subGraphRaw , subGraphStd, type = t, country = c))
#       # return(data.frame(centDegTheory, centDegRaw, centDegStd, type = t, country = c))
#       
#     })) 
#     
#     rawBaseList$date = unique(dframe$date)
#     
#     return(rawBaseList)
#     
#   }))
#   return(rawTypeList)
# }))
# 
