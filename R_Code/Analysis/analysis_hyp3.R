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
sub_data_agg = sub_data %>% group_by(date, target_province, type_sub_cat) %>%
                          summarise(policy_count = sum(policy_count),
                                    policy_dum = sum(policy_dum),
                                    type = unique(type)) %>%
                              ungroup()%>%
                    group_by(date, target_province, type) %>%
                        summarise(policy_count = mean(policy_count),
                         policy_dum = mean(policy_dum)) %>%
                          ungroup()%>%
                  mutate(policy_dum = if_else(policy_dum==0, 0, 1),
                         province = target_province) %>%
                dplyr:::select(-target_province) %>%
              mutate(type = ifelse(type == 'Social Distancing', 'Mask Wearing', type))


# get cases data
df_cases <- read_csv("WEP_analysis/data/Cases/final_cases.csv", guess_max = 10000)

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




# get RAI data
df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
names(df_fed)[which(names(df_fed) == 'Jurisdiction Name')] = 'country'
df_fed = df_fed %>% filter(country %in% c('France', 'Germany', 'Switzerland', 'Italy'))


# merge data
df = merge(sub_data_agg %>% filter(date > "2019-12-31"), df_cases, by = c("province","date"), all.x = TRUE)
df = merge(df , df_fed[, c('country', 'HueglinFennaFederalPolity', 'n_selfrule' , 'n_sharedrule', 'n_RAI', 'Self', 'Shared', 'RAI')], by = c("country"), all.x = TRUE)


# make time variable

df$differentiating = ifelse(df$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
df  = df %>% group_by(province) %>% mutate(time = 1:n(),
                                          time2 = time^2,
                                          time3 = time^3) %>%
  ungroup()

# make France the reference country
df$country = factor(df$country, levels(factor(df$country))[c(1, 3, 2, 4)])
df = within(df, country <- relevel(country, ref = 'France'))
 

# make differentiating dummy
df$differentiating = ifelse(type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
df = within(df, differentiating <- relevel(differentiating, ref =  'Differentiated'))



# make  Restrictions of Mass Gathering the reference
df$type = factor(df$type, levels(factor(df$type))[c(4, 3, 2, 1)])
df = within(df, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))


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
 
names(df)
# --------------
# analyze data
# --------------

# logit of policy dummy
logitA = glm(policy_dum ~ country*type*measure_H3_deaths_a    , data = df, family = 'binomial')
logitB = glm(policy_dum ~ RAI*type*measure_H3_deaths_a    , data = df, family = 'binomial')
logitC = glm(policy_dum ~ Self*type*measure_H3_deaths_a  , data = df, family = 'binomial')

# poisson of policy count
#model3b = glm(policy_count ~ country*type*measure_H3 + time + time2 +time3+ province, data = df, family = 'poisson')

# ols of policy count
lmA = lm(lpolicy_count ~ country*type*measure_H3_deaths_a  + measure_H1_H2_cases_JHU + time + time2 +time3, data = df)
lmB = lm(lpolicy_count ~ RAI*type*measure_H3_deaths_a  + measure_H1_H2_cases_JHU + time + time2 +time3, data = df)
lmC = lm(lpolicy_count ~ Self*type*measure_H3_deaths_a  + measure_H1_H2_cases_JHU + time + time2 +time3, data = df)


# ols of policy count, standardized
lmAs = lm(lpolicy_count ~ country*type*measure_H3_deaths_b +measure_H1_H2_cases_JHU + time + time2 +time3, data = df)
lmBs = lm(lpolicy_count ~ RAI*type*measure_H3_deaths_b  + measure_H1_H2_cases_JHU + time + time2 +time3, data = df)
lmCs = lm(lpolicy_count ~ Self*type*measure_H3_deaths_b  + measure_H1_H2_cases_JHU + time + time2 +time3, data = df)
 
# ---------------------
# substantive effect plots
# --------------------
 
# plogitA<- plot_model(logitA, type = 'pred', terms = c( 'measure_H3 [.262, 1.175]', 'country', 'type'), transform = 'exp')+
#   labs(y = 'Policy Dummy',
#      x = 'Cases Measure',
#      title = 'Predicted Values of Policy Incidence (dummy variable), \n Logit Model',
#      colors = c('red', 'blue', 'green', 'purple'))+
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_logit_country.pdf", plogitA)
# 
# plogitB<- plot_model(logitB, type = 'pred', terms = c( 'measure_H3 [.262, 1.175]', 'RAI', 'type'), transform = 'exp')+
#   labs(y = 'Policy Dummy',
#        x = 'Cases Measure',
#        title = 'Predicted Values of Policy Incidence (dummy variable), \n Logit Model',
#        colors = c('red', 'blue', 'green', 'purple'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_logit_rai.pdf", plogitB)
# 
# 
# plogitC<- plot_model(logitC, type = 'pred', terms = c( 'measure_H3[.262, 1.175]', 'Self', 'type'), transform = 'exp')+
#   labs(y = 'Policy Dummy',
#        x = 'Cases Measure',
#        title = 'Predicted Values of Policy Incidence (dummy variable), \n Logit Model',
#        colors = c('red', 'blue', 'green', 'purple'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  


# ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_logit_self.pdf", plogitC)
 
plmA<- plot_model(lmA, type = 'pred', terms = c( 'measure_H3_deaths_a [0, 2.15]', 'country', 'type'), transform = 'exp')+
  labs(y ='Policy Count, Logged',
       x = 'Relative Proportional Death Rate',
       title = 'Predicted Values of Policy Incidence (logged count variable), \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
 
plmA
ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_country_proportion.pdf", plmA)

plmB<- plot_model(lmB, type = 'pred', terms = c( 'measure_H3_deaths_a [0, 2.15]]', 'RAI', 'type'), transform = 'exp')+
  labs(y = 'Policy Count, Logged',
       x = 'Relative Proportional Death Rate',
       title = 'Predicted Values of Policy Incidence (logged count variable), \n OLS Model, Relative Proportional Death Rate',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plmB
 
ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_rai_proportion.pdf", plmB)

plmC<- plot_model(lmC, type = 'pred', terms = c( 'measure_H3_deaths_a [0, 2.15]', 'Self', 'type'), transform = 'exp')+
  labs(y = 'Policy Count, Logged',
       x = 'Relative Proportional Death Rate',
       title = 'Predicted Values of Policy Incidence (count variable), \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plmC
ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_self_proportion.pdf", plmC)


# Standardzed H3 measure
 quantile(df$measure_H3_deaths_b, c(.1, .2, .25, .75,.8, .9), na.rm = TRUE)
plmAs<- plot_model(lmAs, type = 'pred', terms = c( 'measure_H3_deaths_b [-1, 1.15]', 'country', 'type'), transform = 'exp')+
  labs(y ='Policy Count, Logged',
       x = 'Relative Standardized Death Rate',
       title = 'Predicted Values of Policy Incidence (logged count variable), \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plmAs
ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_country_standardized.pdf", plmAs)

plmBs<- plot_model(lmBs, type = 'pred', terms = c(  'measure_H3_deaths_b [-1, 1.15]', 'RAI', 'type'), transform = 'exp')+
  labs(y = 'Policy Count, Logged',
       x = 'Relative Standardized Death Rate',
       title = 'Predicted Values of Policy Incidence (logged count variable), \n OLS Model, Relative Proportional Death Rate',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plmBs 

ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_rai_standardized.pdf", plmBs)

plmCs<- plot_model(lmCs, type = 'pred', terms = c(  'measure_H3_deaths_b [-1, 1.15]', 'Self', 'type'), transform = 'exp')+
  labs(y = 'Policy Count, Logged',
       x = 'Relative Standardized Death Rate',
       title = 'Predicted Values of Policy Incidence (count variable), \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plmCs
ggsave("WEP_analysis/Results/predictedEffectsH3/predicted_effects_h3_ols_self_standardized.pdf", plmCs)


summary(lmBs)
# ---------------------
# format nice tables
# --------------------

coefMap = list(
  'RAI' = 'RAI',
  'Self' = 'Self',
  
  "typeRestrictions of Mass Gatherings" = 'Restrictions of Mass Gatherings Dum',
  'typeLockdown' = "Lockdown Dum",
  'typeMask Wearing' = 'Mask Wearing Dum',
  'typeClosure and Regulation of Schools' = 'Schools Dum',
  
  'measure_H3_deaths_a' = 'Prop. Death Rate',
  'measure_H3_deaths_b' = 'Std. Death Rate',
 
  
  "countrySwitzerland" = "Switzerland",
  "countryGermany" = "Germany",
  "countryItaly" = "Italy",
  
  'typeRestrictions of Mass Gatherings:countrySwitzerland' = 'Restrictions of Mass Gatherings Dum * Switzerland',
  'typeRestrictions of Mass Gatherings:countryGermany' = 'Restrictions of Mass Gatherings Dum * Germany',
  'typeRestrictions of Mass Gatherings:countryItaly' = 'Restrictions of Mass Gatherings Dum * Italy',
  
  'typeLockdown:countrySwitzerland' = 'Lockdown Dum * Switzerland',
  'typeLockdown:countryGermany' = 'Lockdown Dum * Germany',
  'typeLockdown:countryItaly' = 'Lockdown Dum * Italy',
  
  'typeMask Wearing:countrySwitzerland' = 'Mask Wearing Dum * Switzerland',
  'typeMask Wearing:countryGermany' = 'Mask Wearing Dum * Germany',
  'typeMask Wearing:countryItaly' = 'Mask Wearing Dum * Italy',
  
  'RAI:typeMask Wearing' = 'Mask Wearing Dum * RAI',
  'RAI:typeLockdown' = 'Lockdown Dum * RAI',
  'RAI:typeClosure and Regulation of Schools' = 'SchoolsDum * RAI',
  
  'RAI:measure_H3_deaths_b' = 'Std. Death Rate * RAI',
  'RAI:measure_H3_deaths_a' = 'Prop Death Rate * RAI',
    
  'Self:typeMask Wearing' = 'Mask Wearing Dum * Self',
  'Self:typeLockdown' = 'Lockdown Dum * Self',
  'Self:typeClosure and Regulation of Schools' = 'SchoolsDum * Self',
  'Self:measure_H3_deaths_b' =   'Std. Death Rate * Self',
    
  'countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Switzerland',
  
  'countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate* Switzerland',
  
  # 'typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum',
  # 'countryGermany:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Germany',
  # 'countryItaly:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Italy',
  # 'countrySwitzerland:typeRestrictions of Mass Gatherings:measure_H3' = 'Cases Measure * Restrictions of Mass Gatherings Dum * Switzerland',

  'typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum',
  'typeMask Wearing:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum',
  'typeClosure and Regulation of Schools:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum',
  
  'typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum',
  'typeMask Wearing:measure_H3_deaths_a' = 'Std. Death Rate * Mask Wearing Dum',
  'typeClosure and Regulation of Schools:measure_H3_deaths_a' = 'Std. Death Rate * Schools Dum',
  
  'countryGermany:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Germany',
  'countryItaly:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Italy',
  'countrySwitzerland:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Switzerland',
  
  'countryGermany:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Germany',
  'countryItaly:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Italy',
  'countrySwitzerland:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Switzerland',
  
  'RAI:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * RAI',
  'RAI:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * RAI',
  'RAI:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * RAI',
  
  'RAI:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * RAI',
  'RAI:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * RAI',
  'RAI:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * RAI',
  
  'Self:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Self',
  'Self:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Self',
  'Self:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Self',
  
  'Self:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Self',
  'Self:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Self',
  'Self:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Self',
  
  'measure_H1_H2_cases_JHU' = 'National Cases Count',
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
)
summary(lmC)  
texreg(list(model3a, model3b, model3c),
       custom.coef.map = coefMap,
       custom.model.names = c( 'Logit', 'Poisson', "OLS"))

names(df)

df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
head(df_fed)
