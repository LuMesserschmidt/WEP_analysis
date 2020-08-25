## analysis of H1 and H2

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(texreg)
library(ggplot2)
library(glmnet)
# ----------------------------
# load and format data
# ----------------------------
policyCentralization = readRDS( "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 

# get cases data and collapse provincial data to country data
df_cases <- read_csv("WEP_analysis/data/Cases/final_cases.csv", guess_max = 10000)
df_cases_nat = df_cases  %>% group_by(country, date) %>% summarize( measure_H1_H2_deaths = mean(measure_H1_H2_deaths),
                                                                   measure_H1_H2_cases_ECDC = mean(measure_H1_H2_cases_ECDC))  

# get RAI data
df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
names(df_fed)[which(names(df_fed) == 'Jurisdiction Name')] = 'country'
df_fed = df_fed %>% filter(country %in% c('France', 'Germany', 'Switzerland', 'Italy'))

# get covariates
# df_main <- read_csv("WEP_analysis/data/merged_final_backup.csv", guess_max = 10000) --- this is yearly data; can't use
df_hhi <- read_csv("WEP_analysis/Measures/hhi_deaths.csv", guess_max = 10000)

# ----------------------------
# merge data
# ----------------------------
df = merge(policyCentralization , df_cases_nat, by = c("country","date"), all.x = TRUE)
df = merge(df , df_hhi[, c('country', 'date', 'hhi_cumulative_deaths', 'hhi_new_deaths')], by = c("country","date"), all.x = TRUE)
df = merge(df , df_fed[, c('country', 'HueglinFennaFederalPolity', 'n_selfrule' , 'n_sharedrule', 'n_RAI', 'Self', 'Shared', 'RAI')], by = c("country"), all.x = TRUE)

# ----------------------------
# create variables
# ----------------------------

# make 'differentiating' variable
df$differentiating = ifelse(df$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
df = within(df, differentiating <- relevel(differentiating, ref =  'Differentiated'))
 
# make time count variable
df  = df %>% group_by(country) %>% mutate(time = 1:n(),
                                    time2 = time^2,
                                    time3 = time^3) %>%
              ungroup()
 
# make a version of the DV that rounds to 0 or 1
df = df %>% mutate(centDegR = factor(round(centDegStd)))
df = df %>% mutate(lcentDegStd = log(centDegStd +1))

# make France the reference country
df$country = factor(df$country, levels(factor(df$country))[c( 3, 4, 2 , 1)])
df = within(df, country <- relevel(country, ref = 'France'))

# make  Restrictions of Mass Gathering the reference
df$type = factor(df$type, levels(factor(df$type))[c(4, 3, 2, 1)])
df = within(df, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))


# -----------------
# EDA
# ----------------
hist(df$centDegStd)
hist(df$lcentDegStd)

# -----------------
# Run models
# ----------------

lmA1 = lm(centDegStd ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
 
lmB1 = lm(centDegStd ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

lmC1 = lm(centDegStd ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

 
lmA1_diff = lm(centDegStd ~ differentiating*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

lmB1_diff = lm(centDegStd ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

lmC1_diff = lm(centDegStd ~ differentiating*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
  # note that when you include time3 -- 0 or 1 is perfectly predicted
# logitA = glm(centDegR ~ type*country +measure_H1_H2_cases_ECDC+hhi_new_deaths + time  , data = df, family = 'binomial' )

# logitB = glm(centDegR ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time , data = df, family = 'binomial' )

# logitC = glm(centDegR ~ type*Self+measure_H1_H2_cases_ECDC +hhi_new_deaths+ time , data = df, family = 'binomial' )


 
#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r

# # -----------------
# Substantive effect plots
# ----------------
 
plmA1 = plot_model(lmA1, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))

plmA1 
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_countrydum.pdf", plmA1)

plmB1 = plot_model(lmB1, type = 'pred', terms = c('RAI', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
plmB1
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_rai.pdf", plmB1)


plmC1 = plot_model(lmC1, type = 'pred', terms = c('Self', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
plmC1

ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_self.pdf", plmC1)



# differentating

plmA1_diff = plot_model(lmA1_diff, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))

plmA1_diff
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_countrydum_diff.pdf", plmA1)

plmB1_diff = plot_model(lmB1_diff, type = 'pred', terms = c('RAI', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
plmB1_diff
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_rai_diff.pdf", plmB1_diff)


plmC1_diff = plot_model(lmC1_diff, type = 'pred', terms = c('Self', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
plmC1_diff

ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_self_diff.pdf", plmC1_diff)




 
plogisA = plot_model(logitA , type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n Logit Model',
       colors = c('red', 'blue', 'green', 'purple'))
plogisA 
ggsave("WEP_analysis/Results/predicted_effects_h1_logit_country.pdf", plogisA)

plogisB = plot_model(logitB , type = 'pred', terms = c('RAI', 'type'), transform = "exp",
           axis.labels = c('Policy Centralization', 'Policy Type'))+
  labs(y = 'Policy Centralization',
       x = 'RAI',
       title = 'Predicted Values of Policy Centralization, \n Logit Model')
plogisB
ggsave("WEP_analysis/Results/predicted_effects_h1_logit_rai.pdf", plogisB)

plogisC = plot_model(logitC, , type = 'pred', terms = c('Self', 'type'), transform = "exp",
                  axis.labels = c('Policy Centralization', 'Policy Type'))+
   labs(y = 'Policy Centralization',
        x = 'Self Index',
        title = 'Predicted Values of Policy Centralization, \n Logit Model')
plogisC
ggsave("WEP_analysis/Results/predicted_effects_h1_logit_self.pdf", plogisC)

# ---------------
# format tables
# ----------------
coefMap = list(
  
  "typeRestrictions of Mass Gatherings" = 'Restrictions of Mass Gatherings Dum',
  'typeLockdown' = "Lockdown Dum",
  'typeMask Wearing' = 'Mask Wearing Dum',
  'typeClosure and Regulation of Schools' = 'Schools Dum',
  
  'RAI' = 'RAI',
  'Self' = 'Self',

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
  
  'Self:typeMask Wearing' = 'Mask Wearing Dum * Self',
  'Self:typeLockdown' = 'Lockdown Dum * Self',
  'Self:typeClosure and Regulation of Schools' = 'SchoolsDum * Self',
  'Self:measure_H3_deaths_b' =   'Std. Death Rate * Self',
  
  'RAI:typeMask Wearing' = 'Mask Wearing Dum * RAI',
  'RAI:typeLockdown' = 'Lockdown Dum * RAI',
  'RAI:typeClosure and Regulation of Schools' = 'SchoolsDum * RAI',
  
  "hhi_new_deaths" = "HHI (new deaths)",
  'measure_H1_H2_cases_JHU' = 'National Cases Count',
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
  )

 texreg(list(model1, model2),
        custom.coef.map = coefMap,
        custom.model.names = c('OLS', 'Logit'))
unique(qualtrics$type)
foo = qualtrics %>% filter(type =="Restriction and Regulation of Businesses" &
                       country %in% countries &
                       grepl('mask|Mask', description))


foo %>% select(description) %>% data.frame()
 unique(sub_data$type)
 
 unique(test$type)
test2 = test %>% filter(country == 'Switzerland' & type ==  "Closure and Regulation of Schools"    & init_country_level == 'Provincial' & type_sub_cat == 'Primary Schools (generally for children ages 10 and below)' )  %>%  #  select(record_id) %>% pull %>% unique %>% dput
    arrange(date_start) %>%
   #dplyr:::select( date_start, date_end) %>%
   #distinct %>%
   dplyr:::select(update_type, record_id, policy_id, country, province, description, type, type_mass_gathering, type_sub_cat, type_who_gen, compliance, date_start, date_end) %>%
   #slice (c(1, 2, 3, 4, 5, 7, 10)) %>%
   #slice (c(6, 8, 9)) %>%
   data.frame()


split(test2, test2$province)[[1]]
  

   
qualtrics%>% filter(policy_id == 7500010  )%>% 
  arrange(policy_id, date_start) %>%
  #dplyr:::select( date_start, date_end) %>%
  #distinct %>%
  dplyr:::select(update_type, record_id, policy_id, country, province, description, type_mass_gathering, type_sub_cat, type_who_gen, compliance, date_start, date_end, init_country_level, target_country) %>%
  #slice (c(1, 2, 3, 4, 5, 7, 10)) %>%
  #slice (c(6, 8, 9)) %>%
  data.frame()

 sub_data %>% filter(country == 'Germany' & type == 'Lockdown') %>%
   arrange(init_country_level, province, date_start, entry_type)%>%
   data.frame()

 
 sub_data %>% filter(country == 'Germany' & type == 'Restrictions of Mass Gatherings' & province == 'Bavaria' ) %>% 
   arrange(date_start) %>%
   #dplyr:::select( date_start, date_end) %>%
   #distinct %>%
   dplyr:::select(update_type, record_id, policy_id, country, province, description, type_mass_gathering, type_sub_cat, type_who_gen, compliance, date_start, date_end, init_country_level, target_country) %>%
   #slice (c(1, 2, 3, 4, 5, 7, 10)) %>%
   #slice (c(6, 8, 9)) %>%
   data.frame()
 
   select(policy_id) %>% pull %>% unique %>% dput
 
 
 sub_data %>% filter(country == 'Germany' & type == 'Lockdown') %>%
   arrange(init_country_level, province, date_start, entry_type)%>%
   data.frame()
 
 policyCentralization %>% filter(country == 'Germany' & type == 'Lockdown') %>% 
   dplyr:::select(date, centDegStd)
  
 policyCentralization %>% filter(country == 'Switzerland' & type == 'Lockdown') %>% 
   dplyr:::select(date, centDegStd)
 
 policyCentralization %>% filter(country == 'Italy' & type == 'Lockdown') %>% 
   dplyr:::select(date, centDegStd)
 
 policyCentralization %>% filter(country == 'Switzerland' & type == 'Restrictions of Mass Gatherings') %>%
   dplyr:::select(centDegRaw, centDegTheory, centDegStd, date) %>%
   data.frame()
 
 test %>% filter(country == 'Switzerland' & type == 'Restrictions of Mass Gatherings' & init_country_level == 'National') %>% 
   arrange(init_country_level,  date_start, entry_type ) %>%
   select(description, date_start, date_end, type_mass_gathering, type_sub_cat, type_who_gen, policy_id, record_id)%>%
   data.frame

 