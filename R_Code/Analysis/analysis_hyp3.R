# analysis hyp3


# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(ggplot2)
# --------------
# load and format data
# --------------

## get coronanet data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long.RDS")

# collapse by date, province and policy type
sub_data_agg = sub_data %>% group_by(date, target_province, type) %>%
                          summarise(policy_count = sum(policy_count),
                                    policy_dum = sum(policy_dum)) %>%
                ungroup()%>%
                  mutate(policy_dum = if_else(policy_dum==0, 0, 1),
                         province = target_province) %>%
                dplyr:::select(-target_province)

 
# get cases data
df_cases <- read_csv("WEP_analysis/data/Cases/cases.csv", guess_max = 10000)

df_cases = mutate(df_cases, 
                  province = recode(region,
                                    "Auvergne-Rhône-Alpes" = "Auvergne-Rhone-Alpes",
                                    "Bourgogne-Franche-Comté" = "Bourgogne-Franche-Comte" ,
                                    "Bretagne" = "Brittany" , 
                                    "Centre-Val de Loire" ="Centre" ,
                                    "Corse"  = "Corsica", 
                                    "Guyane" = "French Guiana", 
                                    "Île-de-France" = "Ile-de-France",
                                    "La Réunion" = "Reunion" , 
                                    "Normandie" = "Normandy" ,
                                    "Provence-Alpes-Côte d'Azur" = "Provence-Alpes-Cote d'Azur", 
                                    "Basilicata" = "Basilicate", 
                                    "Lazio" = "Latium" , 
                                    "Lombardia" = "Lombardy",
                                    "Marche"= "The Marches", 
                                    "Piemonte" = "Piedmont" , 
                                    "Puglia" =  "Apulia" , 
                                    "Sardegna" = "Sardinia" , 
                                    "Sicilia" = "Sicily" , 
                                    "Toscana" = "Tuscany", 
                                    "Valle d'Aosta" = "Aosta Valley"
                                    ))



# merge data
df = merge(sub_data_agg %>% filter(date > "2019-12-31"), df_cases, by = c("province","date"), all.x = TRUE)

# make time variable
df$differentiating = ifelse(type %in% c("Lockdown", "Closure and Regulation of Schools"), 1, 0)
df  = df %>% group_by(province) %>% mutate(time = 1:n(),
                                          time2 = time^2,
                                          time3 = time^3) %>%
  ungroup()

# make France the reference country
df$country = factor(df$country, levels(factor(df$country))[c(1, 3, 2, 4)])
df = within(df, country <- relevel(country, ref = 'France'))
 
# make  Restrictions of Mass Gathering the reference
df$type = factor(df$type)
df = within(df, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))

# 
df = df %>% mutate(lpolicy_count = ifelse(policy_count == 0, 0, log(policy_count)))



# --------------
# eda
# --------------
hist(df$lpolicy_count)
hist(df$policy_count)
hist(log(df$policy_count+1))
hist(df$policy_dum)
hist(log(df$measure_H3 - min(df$measure_H3, na.rm = TRUE)+1))
hist(df$measure_H3)
 

# --------------
# analyze data
# --------------

# logit of policy dummy
model3a = glm(policy_dum ~ country*type*measure_H3 + time + time2, data = df, family = 'binomial')

model3a = glm(policy_dum ~ country*type*measure_H3 + time + time2+ province, data = df, family = 'binomial')
summary(model3a)
# poisson of policy count
model3b = glm(policy_count ~ country*type*measure_H3 + time + time2 +time3+ province, data = df, family = 'poisson')



# ols of policy count
model3c = lm(lpolicy_count ~ country*type*measure_H3 + time + time2 +time3+ province, data = df)
 
# ---------------------
# substantive effect plots
# --------------------

p3a<- plot_model(model3a, type = 'pred', terms = c( 'measure_H3 [-6.161898e-06, 6.192858e-07]', 'country', 'type'), transform = 'exp')+
  labs(y = 'Policy Dummy',
     x = 'Cases Measure',
     title = 'Predicted Values of Policy Incidence (dummy variable), \n Logit Model',
     colors = c('red', 'blue', 'green', 'purple'))+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3a<- plot_model(model3a, type = 'pred', terms = c( 'measure_H3', 'country', 'type'), transform = 'exp')+
  labs(y = 'Policy Dummy',
       x = 'Cases Measure',
       title = 'Predicted Values of Policy Incidence (dummy variable), \n Logit Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3a 
ggsave("WEP_analysis/Results/predicted_effects_h3_logit.pdf", p3a)


p3b<-plot_model(model3b,  type = 'pred', terms = c( 'measure_H3 [-6.161898e-06, 6.192858e-07]', 'country', 'type'))+
  labs(y = 'Policy Count',
       x = 'Cases Measure',
       title = 'Predicted Values of Policy Incidence (count variable), \n Poisson Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p3b
ggsave("WEP_analysis/Results/predicted_effects_h3_poisson.pdf", p3b)

p3c<-plot_model(model3c,  type = 'pred', terms = c( 'measure_H3 [-6.161898e-06, 6.192858e-07]', 'country', 'type'))+
  labs(y = 'Policy Count',
       x = 'Cases Measure',
       title = 'Predicted Values of Policy Incidence (count variable), \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

 p3c
ggsave("WEP_analysis/Results/predicted_effects_h3_ols.pdf", p3c)



# ---------------------
# format nice tables
# --------------------

coefMap = list(
  "typeRestrictions of Mass Gatherings" = 'Restrictions of Mass Gatherings Dum',
  'typeLockdown' = "Lockdown Dum",
  "countrySwitzerland" = "Switzerland",
  "countryGermany" = "Germany",
  "countryItaly" = "Italy",
  'typeRestrictions of Mass Gatherings:countrySwitzerland' = 'Restrictions of Mass Gatherings Dum * Switzerland',
  'typeRestrictions of Mass Gatherings:countryGermany' = 'Restrictions of Mass Gatherings Dum * Germany',
  'typeRestrictions of Mass Gatherings:countryItaly' = 'Restrictions of Mass Gatherings Dum * Italy',
  'typeLockdown:countrySwitzerland' = 'Lockdown Dum * Switzerland',
  'typeLockdown:countryGermany' = 'Lockdown Dum * Germany',
  'typeLockdown:countryItaly' = 'Lockdown Dum * Italy',
  
  'countryGermany:measure_H3' = 'Cases Measure * Germany',
  'countryItaly:measure_H3' = 'Cases Measure * Italy',
  'countrySwitzerland:measure_H3' = 'Cases Measure * Switzerland',
  
  'typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum',
  'countryGermany:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Germany',
  'countryItaly:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Italy',
  'countrySwitzerland:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Switzerland',

  'typeLockdown:measure_H3' = 'Cases Measure * Lockdown Dum',
  'countryGermany:typeLockdown:measure_H3' = 'Cases Measure * Lockdown Dum * Germany',
  'countryItaly:typeLockdown:measure_H3' = 'Cases Measure * Lockdown Dum * Italy',
  'countrySwitzerland:typeLockdown:measure_H3' = 'Cases Measure * Lockdown Dum * Switzerland',
  
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
)

texreg(list(model3a, model3b, model3c),
       custom.coef.map = coefMap,
       custom.model.names = c( 'Logit', 'Poisson', "OLS"))

names(df)

df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
head(df_fed)
