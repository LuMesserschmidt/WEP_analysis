## analysis of H1

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(texreg)
library(ggplot2)

# ----------------------------
# load and format data
# ----------------------------
policyCentralization = readRDS( "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 


# get cases data
df_cases <- read_csv("WEP_analysis/data/Cases/cases.csv", guess_max = 10000)
df_cases_nat = df_cases  %>% group_by(country, date) %>% summarize( measure_H1_H2 = mean(measure_H1_H2))  

# get RAI data
df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
names(df_fed)[which(names(df_fed) == 'Jurisdiction Name')] = 'country'
df_fed = df_fed %>% filter(country %in% c('France', 'Germany', 'Switzerland', 'Italy'))


# get covariates
# df_main <- read_csv("WEP_analysis/data/merged_final_backup.csv", guess_max = 10000) --- this is yearly data; can't use
df_hhi <- read_csv("WEP_analysis/Measures/hhi.csv", guess_max = 10000)


# ----------------------------
# merge data
# ----------------------------
df = merge(policyCentralization , df_cases_nat, by = c("country","date"), all.x = TRUE)
df = merge(df , df_hhi[, c('country', 'date', 'hhi_cumulative', 'hhi_new')], by = c("country","date"), all.x = TRUE)

# ----------------------------
# create variables
# ----------------------------

# make 'differentiating' variable
df$differentiating = ifelse(type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
df = within(df, differentiating <- relevel(differentiating, ref =  'Differentiated'))
 
# make time count variable
df  = df %>% group_by(country) %>% mutate(time = 1:n(),
                                    time2 = time^2,
                                    time3 = time^3) %>%
              ungroup()
 
# make a version of the DV that rounds to 0 or 1
df = df %>% mutate(centDegR = round(centDegStd))
df = df %>% mutate(centDegD = ifelse(centDegStd>0, 1, 0))

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

# -----------------
# Run models
# ----------------

model1T = lm(centDegStd ~ type*country + hhi_new + time + time2+time3, data = df )
summary(model1)


model1D = lm(centDegStd ~ differentiating*country + hhi_new + time + time2+time3, data = df )
summary(model1D)

# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
  # note that when you include time3 -- 0 or 1 is perfectly predicted
model2T = glm(centDegR ~ type*country + hhi_new + time , data = df, family = 'binomial' )

model2D = glm(centDegR ~ differentiating*country + hhi_new + time , data = df, family = 'binomial' )
summary(model2D) 
# -----------------
# Substantive effect plots
# ----------------
 
p1T = plot_model(model1T, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
ggsave("WEP_analysis/Results/predicted_effects_h1_ols.pdf", p1T)



p1D = plot_model(model1D, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Differentiating Policy Dummy',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))

ggsave("WEP_analysis/Results/predicted_effects_h1_ols_diffDum.pdf", p1D)


p2T = plot_model(model2T, type = 'int', transform = "exp",
           axis.labels = c('Policy Centralization', 'Policy Type'))+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n Logit Model')

ggsave("WEP_analysis/Results/predicted_effects_h1_logit.pdf", p2T)

p2D = plot_model(model2D, type = 'int', transform = "exp",
                  axis.labels = c('Policy Centralization', 'Policy Type'))+
   labs(y = 'Policy Centralization',
        x = 'Policy Type',
        title = 'Predicted Values of Policy Centralization, \n Logit Model')

ggsave("WEP_analysis/Results/predicted_effects_h1_logit_diffDum.pdf", p2D)

# ---------------
# format tables
# ----------------
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
  
  "hhi_new" = "HHI (new)",
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
  )

 texreg(list(model1, model2),
        custom.coef.map = coefMap,
        custom.model.names = c('OLS', 'Logit'))


 
 
 sub_data %>% filter(country == 'Germany' & type == 'Closure and Regulation of Schools'   & compliance == "Voluntary/Recommended but No Penalties")  %>%  #  select(record_id) %>% pull %>% unique %>% dput
  
   sub_data %>% filter(is.na(type_sub_cat) )%>%
    arrange(date_start) %>%
   #dplyr:::select( date_start, date_end) %>%
   #distinct %>%
   dplyr:::select(update_type, record_id, policy_id, country, province, description, type, type_mass_gathering, type_sub_cat, type_who_gen, compliance, date_start, date_end) %>%
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

 