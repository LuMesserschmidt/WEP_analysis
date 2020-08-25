## analysis of H1 and H2

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(texreg)
library(ggplot2)
library(glmnet)
library(stargazer)
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
# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
# note that when you include time3 -- 0 or 1 is perfectly predicted
# logitA = glm(centDegR ~ type*country +measure_H1_H2_cases_ECDC+hhi_new_deaths + time  , data = df, family = 'binomial' )
# logitB = glm(centDegR ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time , data = df, family = 'binomial' )
# logitC = glm(centDegR ~ type*Self+measure_H1_H2_cases_ECDC +hhi_new_deaths+ time , data = df, family = 'binomial' )
#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r


# type as interaction
hyp1_country_type = lm(centDegStd ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
 
hyp1_rai_type = lm(centDegStd ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp1_self_type = lm(centDegStd ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )


# diff dum as interaction
hyp1_country_diff = lm(centDegStd ~ differentiating*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp1_rai_diff = lm(centDegStd ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp1_self_diff = lm(centDegStd ~ differentiating*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )


# # -----------------
# Substantive effect plots
# ----------------
 
p_hyp1_country_type  = plot_model(hyp1_country_type , type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
p_hyp1_country_type 
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_countrydum.pdf", p_hyp1_country_type )

p_hyp1_rai_type  = plot_model(hyp1_rai_type , type = 'pred', terms = c('RAI', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
p_hyp1_rai_type
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_rai.pdf", p_hyp1_rai_type)


p_hyp1_self_type = plot_model(hyp1_self_type, type = 'pred', terms = c('Self', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))


ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_self.pdf", p_hyp1_self_type)

# differentating

p_hyp1_country_diff = plot_model(hyp1_country_diff, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))

p_hyp1_country_diff
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_countrydum_diff.pdf", p_hyp1_country_diff)

p_hyp1_rai_diff = plot_model(hyp1_rai_diff, type = 'pred', terms = c('RAI', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
p_hyp1_rai_diff
ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_rai_diff.pdf", p_hyp1_rai_diff)


p_hyp1_self_diff = plot_model(hyp1_self_diff, type = 'pred', terms = c('Self', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
p_hyp1_self_diff

ggsave("WEP_analysis/Results/predictEffectsH1/predicted_effects_h1_ols_self_diff.pdf", p_hyp1_self_diff)


# plogisA = plot_model(logitA , type = 'int')+
#   labs(y = 'Policy Centralization',
#        x = 'Policy Type',
#        title = 'Predicted Values of Policy Centralization, \n Logit Model',
#        colors = c('red', 'blue', 'green', 'purple'))
# plogisA 
# ggsave("WEP_analysis/Results/predicted_effects_h1_logit_country.pdf", plogisA)
# 
# plogisB = plot_model(logitB , type = 'pred', terms = c('RAI', 'type'), transform = "exp",
#            axis.labels = c('Policy Centralization', 'Policy Type'))+
#   labs(y = 'Policy Centralization',
#        x = 'RAI',
#        title = 'Predicted Values of Policy Centralization, \n Logit Model')
# plogisB
# ggsave("WEP_analysis/Results/predicted_effects_h1_logit_rai.pdf", plogisB)
# 
# plogisC = plot_model(logitC, , type = 'pred', terms = c('Self', 'type'), transform = "exp",
#                   axis.labels = c('Policy Centralization', 'Policy Type'))+
#    labs(y = 'Policy Centralization',
#         x = 'Self Rule Index',
#         title = 'Predicted Values of Policy Centralization, \n Logit Model')
# plogisC
# ggsave("WEP_analysis/Results/predicted_effects_h1_logit_self.pdf", plogisC)

# ---------------
# format tables
# ----------------
coefMap = list(
  
  "typeRestrictions of Mass Gatherings" = 'Restrictions of Mass Gatherings Dum',
  'typeLockdown' = "Lockdown Dum",
  'typeMask Wearing' = 'Mask Wearing Dum',
  'typeClosure and Regulation of Schools' = 'Schools Dum',
  
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
  
  'Self:typeMask Wearing' = 'Mask Wearing Dum * Self Rule Index',
  'Self:typeLockdown' = 'Lockdown Dum * Self Rule Index',
  'Self:typeClosure and Regulation of Schools' = 'SchoolsDum * Self Rule Index',
  'Self:measure_H3_deaths_b' =   'Std. Death Rate * Self Rule Index',
  
  'typeMask Wearing:Self' = 'Mask Wearing Dum * Self Rule Index',
  'typeLockdown:Self' = 'Lockdown Dum * Self Rule Index',
  'typeClosure and Regulation of Schools:Self' = 'SchoolsDum * Self Rule Index',
  'measure_H3_deaths_b:Self' =   'Std. Death Rate * Self Rule Index',
  
  
  'RAI' = 'RAI',
  'Self' = 'Self',
  
  'RAI:typeMask Wearing' = 'Mask Wearing Dum * RAI',
  'RAI:typeLockdown' = 'Lockdown Dum * RAI',
  'RAI:typeClosure and Regulation of Schools' = 'SchoolsDum * RAI',
  
  
  'typeMask Wearing:RAI' = 'Mask Wearing Dum * RAI',
  'typeLockdown:RAI' = 'Lockdown Dum * RAI',
  'typeClosure and Regulation of Schools:RAI' = 'SchoolsDum * RAI',
  
  "hhi_new_deaths" = "HHI (new deaths)",
  'measure_H1_H2_cases_JHU' = 'National Cases Count',
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
  )


screenreg(list(lmA1, lmB1),
        custom.coef.map = coefMap,
        # custom.model.names = c('OLS', 'Logit'),
  
)
htmlreg(list(lmA1, lmB1),
       custom.coef.map = coefMap,
      # custom.model.names = c('OLS', 'Logit'),
      file = '~/Downloads/testhtml.doc', 
      inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
      head.tag = TRUE, body.tag = TRUE
      )
       
 
