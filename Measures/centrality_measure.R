
## The following R code provides
# some illustrative examples of a proposed measure of 'centralization' of government policies
# where 'centralization' is conceptualized as the degree to which the national government has
# sole control over policy making in a country
# By: Cindy Cheng

## The proposed measure accounts for the following issues:
# issue 0: must account for subnational cases
# issue 1:  must account for cases when there is a national level policy that covers all subnational regions
# issue 2 : must account for cases when there is a national level policy that targets some subnational regions
# issue 3: must account for cases over time

# ------------------------
# Load libraries
# ------------------------
library(igraph)


# ------------------------
# create archetypal/illustrative network graph
# ------------------------
# In this example there are:
  # 1 national government
  # 16 provincial governments
  # 16 geographical provinces

# create nodes for geographical provinces
provs = paste0('Prov', 1:16)
nodes = data.frame(c('Govt National', paste0('Govt Prov', 1:16), provs ))

# create edges between nat/prov governments and geographical provinces
natGov = data.frame(gov = 'Govt National', provs)
provGov = data.frame(gov = paste0('Govt Prov', 1:16), provs)
edges = rbind(natGov, provGov)

edges

nodes
## create illustrative examples for cases where...

# the only policy that exists is at the national level which applies to all provinces; there are no provincial level policies
edges$link1 = c(rep(1, 16), rep(0, 16))

# there is both a national level policy that applies to all provinces and a provincial level policy for each respective province
edges$link2 = c(rep(1, 32))

# there is a national level policy that applies to all provinces and provincial level policies for some provinces
edges$link3 = c(rep(1, 16), sample(c(0,1), 16, replace = TRUE))

# there is a national level policiy that applies to some provinces and a provincial level policy that applies to some provinces
edges$link4 = sample(c(0,1), 32, replace = TRUE)

# there is no national level policy and a provincial level policy that applies for each respective provinces
edges$link5 = c(rep(0, 16), rep(1, 16))


# there is no policy
edges$link6 = c(rep(0, 32))

 
network1 = graph_from_data_frame(edges[which(edges$link1 ==1),], nodes, directed = TRUE)      
network2 = graph_from_data_frame(edges[which(edges$link2 ==1),], nodes, directed = TRUE)     
network3 = graph_from_data_frame(edges[which(edges$link3 ==1),], nodes, directed = TRUE)     
network4 = graph_from_data_frame(edges[which(edges$link4 ==1),], nodes, directed = TRUE)     
network5 = graph_from_data_frame(edges[which(edges$link5 ==1),], nodes, directed = TRUE)     

network6 = graph_from_data_frame(edges[which(edges$link6 ==1),], nodes, directed = TRUE)     


plot(network1)
plot(network2)
plot(network3)
plot(network4)
plot(network5)
plot(network6)

# First Proposed Measure: 
# estimate the degree centrality of each respective illustrative network
  # the proposed measure would use network1 as the baseline measure for the highest degree of centralization 
  # and all other networks would be judged relative to this baseline
  # This measure would thus range from 0 to 1, where 0 is no centralization and 1 is full centralization
  # e.g. centr_degree(network1, mode = 'all')$centralization/centr_degree(network1, mode = 'all')$centralization == 1; this would be the 'most centralized' degree of policymaking
  # centr_degree(network5, mode = 'all')$centralization/centr_degree(network1, mode = 'all')$centralization ; this would be relatively 'decentralized' state of policymaking

centr_degree(network1, mode = 'all')
centr_degree(network2, mode = 'all')
centr_degree(network3, mode = 'all')
centr_degree(network4, mode = 'all')
centr_degree(network5, mode = 'all')

centr_degree(network6, mode = 'all')$centralization

# Second Proposed Measure: 
# estiamte the subgraph centrality of each node
  # the proposed measure would use the subgraph centrality of the national government as the baseline
  # where the subgraph centrality of the national government in network1 would be the the baseline measure for the highest degree of centralization 
  # and all other networks would be judged relative to this baseline
  # This measure would thus range from 0 to 1, where 0 is no centralization and 1 is full centralization
  # e.g. subgraph_centrality(network1)[1]/subgraph_centrality(network1)[1] == 1; this would be the 'most' centralized' degree of policymaking
  # subgraph_centrality(network1)[5]/subgraph_centrality(network1)[1] ; this would be relatively 'decentralized' state of policymaking
subgraph_centrality(network1)
subgraph_centrality(network2)
subgraph_centrality(network3)
subgraph_centrality(network4)
subgraph_centrality(network6)[1]
 


