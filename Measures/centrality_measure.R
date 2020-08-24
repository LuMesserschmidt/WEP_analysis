
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
library(ggraph)

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


edges$link1_wt = ifelse(edges$link1 == 1, sample(seq(.1, 1, length = 10), length(which(edges$link1 == 1,)) , replace = TRUE), 0)

edges
# https://rdrr.io/cran/CINNA/src/R/CINNA.R
edges
network1 = graph_from_data_frame(edges[which(edges$link1 ==1),], nodes, directed = TRUE)      
network2 = graph_from_data_frame(edges[which(edges$link2 ==1),], nodes, directed = TRUE)     
network3 = graph_from_data_frame(edges[which(edges$link3 ==1),], nodes, directed = TRUE)     
network4 = graph_from_data_frame(edges[which(edges$link4 ==1),], nodes, directed = TRUE)     
network5 = graph_from_data_frame(edges[which(edges$link5 ==1),], nodes, directed = TRUE)     

network6 = graph_from_data_frame(edges[which(edges$link6 ==1),], nodes, directed = TRUE)     
 
network1

hub_score(network1)
hub_score(network3)


g1 = ggraph(network1, layout = 'kk') +
geom_edge_link(arrow = arrow(angle = 30, length = unit(0.15, "inches"),
                             ends = "last", type = "closed")) + 
  geom_node_point(colour = 'blue', size = 2)+
  geom_node_text(aes(label = name), repel=TRUE, size = 6)+
  theme_graph(background = 'white')


 
test = as_tbl_graph(network1) %>% 
        activate(nodes) %>%
          mutate(gov = 
                   case_when(grepl('Govt', name) ~ "Government", T ~ "Province"), 
                 gov= as.character(gov))
        #activate(edges)
 
ggraph(test, layout = 'kk')+
  geom_edge_link(arrow = arrow(angle = 30, length = unit(0.15, "inches"),
                               ends = "last", type = "closed")) + 
  geom_node_point(aes(colour = gov),  size = 2)+
  geom_node_text(aes(label = name), repel=TRUE, size = 4)+
  theme_graph(background = 'white')

rstat_nodes



library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(scales)


# example data
rstat_nodes <-
  data.frame(name = c("Hadley", "David", "Romain", "Julia"))
rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
                          to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))

gr <- tbl_graph(nodes = rstat_nodes, edges = rstat_edges)

gr %>% 
  activate(nodes) %>% # use dplyr on nodes
  mutate(David = 
           case_when(name == 'David' ~ 2, T ~ 0), 
         David = as.character(David))# %>% 
  activate(edges) %>% # same on edge list
  mutate(David = case_when(from == 2 ~ 1, T ~ 0), 
         David = as.character(David)) 

gr %>% 
  activate(nodes) %>% # use dplyr on nodes
  mutate(David = 
           case_when(name == 'David' ~ 2, T ~ 0), 
         David = as.character(David)) %>% 
  activate(edges) %>% # same on edge list
  mutate(David = case_when(from == 2 ~ 1, T ~ 0), 
         David = as.character(David)) %>% 
  ggraph(., layout = 'auto')+
  geom_edge_link(aes(color = David), 
                 width = 1)+
  geom_node_point(aes(color = David), 
                  size = 5)+
  geom_node_text(aes(label = name), 
                 nudge_x = .05, 
                 nudge_y = .05)

gr

ggsave("WEP_analysis/Graphics/network1.pdf", g1)
 

g2 = ggraph(network2, layout = "gem") +
  geom_edge_link(arrow = arrow(angle = 30, length = unit(0.15, "inches"),
                               ends = "last", type = "closed")) + 
  geom_node_point(colour = 'blue', size = 2)+
  geom_node_text(aes(label = name),size = 6, repel=TRUE)+
  theme_graph(background = 'white')
ggsave("WEP_analysis/Graphics/network2.pdf", g2)
 

g5 = ggraph(network5, layout ="graphopt") +
  geom_edge_link(arrow = arrow(angle = 30, length = unit(0.15, "inches"),
                               ends = "last", type = "closed")) + 
  geom_node_point(colour = 'blue', size = 2)+
  geom_node_text(aes(label = name), size = 6, repel=TRUE)+
  theme_graph(background = 'white')

g5
ggsave("WEP_analysis/Graphics/network5.pdf", g5)
 
?ggraph
igraph_layout_star()
?ggraph
plot(network1)
plot(network2)
plot(network5)
authority_score(network1)
hub_score(network1, weights = edges$link1_wt[1:16])

edges[which(edges$link1 ==1),]

centr_degree(network2)
hub_score(network5)
?hub_score
hub_score(network1)$vector[1]
hub_score(network2)$vector[1]
hub_score(network3)$vector[1]
hub_score(network4)$vector[1]
hub_score(network5)$vector[1]
hub_score(network5)
edges[which(edges$link1 ==1),1:3]
network1
data("zachary")
eigen_centrality(network1, directed = TRUE, weights = edges$link1_wt[1:16])
?centr_degree

eigen_centrality(network5)
?eigen_centrality
proper_centralities(network1)
calculate_centralities(network1, include = 'Degree Centrality')
calculate_centralities(zachary) 
plot(network6)
a = network1 %>% set_edge_attr( "weight", value = edges$link1_wt[1:16])

plot(a, edge.width = E(a)$weight)

 centr_eigen(network1)
## Run the programme
degree_w(network)

# }

 # https://cran.r-project.org/web/packages/CINNA/vignettes/CINNA.html
# https://www.sci.unich.it/~francesc/teaching/network/kleinberg.html

ifelse(edges$link1 == 1, sample(seq(.1, 1, length = 10), length(which(edges$link1 == 1)) , replace = TRUE), 0) %>% length()

edges
alpha.centrality(network1, weights =edges$link1_wt[1:16] )
?barycenter
g <- make_ring(10) %>%
  set_edge_attr("weight", value = 1:10) %>%
  set_edge_attr("color", value = "red")
g
plot(g, edge.width = E(g)$weight)
centr_degree(g)
?authority_score


edges
?centr_degree
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

centr_degree(network1)
plot(network1)
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
 


