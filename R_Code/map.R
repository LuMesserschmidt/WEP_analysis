library(sf)
library(tidyverse)

gerpath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_1.shp/NUTS_RG_01M_2021_4326_LEVL_1.shp"
italypath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_2.shp/NUTS_RG_01M_2021_4326_LEVL_2.shp"
schweizpath <- "data/map/NUTS_RG_01M_2021_4326_LEVL_3.shp/NUTS_RG_01M_2021_4326_LEVL_3.shp"

shape <- st_read(dsn = gerpath)
ch_shape <- st_read(dsn = schweizpath)
it_shape <- st_read(dsn = italypath)

nuts1 <- shape$NUTS_ID %>% .[str_detect(.,"DE|FR")] %>% str_remove_all("FRY")
nuts2 <- it_shape$NUTS_ID %>% .[str_detect(.,"IT")]
nuts3 <- ch_shape$NUTS_ID %>% .[str_detect(.,"CH")]

#corona <- read_csv("data/merged_final.csv") %>% select("region") %>% distinct()
#merged_v6 <- read_csv("data/merged_v5.csv") #%>% select("date","NUTS_code") %>% distinct()

corona <- read_csv("data/Cases/cases.csv") 


#corona<- left_join(merged_v5,cases,by=c("region","date"))

#table
#table(corona$date,corona$region)


df <- corona %>%
  select("date","cases","new_cases","ratio_cum","ratio_new","code", "cases_pop") %>% 
  mutate(country = str_sub(code,1,2))%>%
  filter(date=="2020-03-15" |
         date=="2020-04-15" |
         date=="2020-05-15" |
         date=="2020-06-01"       
    )%>%
 # distinct()%>%
  #drop_na(.,NUTS_code)%>%
  filter(!code%in% c("FRY1","FRY2","FRY3","FRY4","FRY5"))

             
merged_shape <- 
  left_join(bind_rows(shape,ch_shape,it_shape),
            df,
            by = c("NUTS_ID" = "code"))


library(viridis)

gg <- 
  ggplot(merged_shape %>% 
           filter(!is.na(ratio_cum))) + 
  geom_sf(aes(alpha = ratio_cum,
              fill = country),size = 0.1) + 
  scale_fill_manual(values = c("green","red","blue","orange"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  #scale_fill_continuous(colors = viridis_pal(option="B")(8), limits=c(0, 0.8),na.value = "grey")+
  #coord_sf(xlim = c(-5,20),ylim = c(40,55)) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right") + 
  ggsave(filename = "results/map_ratio.pdf",
         height = 7)

browseURL("results/map_ratio.pdf")


gg <- 
  ggplot(merged_shape %>% 
           filter(!is.na(cases_pop))) + 
  geom_sf(aes(alpha = cases_pop,
              fill = country),size = 0.1) + 
  scale_fill_manual(values = c("green","red","blue","orange"))+
  scale_alpha_continuous(range=c(0,1.5)) + 
  facet_wrap(~date,ncol=2) + 
  #scale_fill_continuous(colors = viridis_pal(option="B")(8), limits=c(0, 0.8),na.value = "grey")+
  #coord_sf(xlim = c(-5,20),ylim = c(40,55)) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right") + 
  ggsave(filename = "results/map.pdf",
         height = 7)

browseURL("results/map.pdf")

library(ggforce)


'
gg <- 
  ggplot(merged_shape %>% 
           filter(!is.na(ratio_cum))) + 
  geom_sf(aes(fill = ratio_cum),size = 0.3) + 
  #facet_grid(country~date) + 
  #coord_sf(xlim = c(-5,20),ylim = c(40,55)) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) + 
  ggsave(filename = "results/luca1.pdf")

browseURL("results/luca1.pdf")

'
