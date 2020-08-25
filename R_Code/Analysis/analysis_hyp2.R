#analysis_h2




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
policyHeterog = #readRDS( "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 

# get cases data and collapse provincial data to country data
df_cases <- read_csv("WEP_analysis/data/Cases/final_cases.csv", guess_max = 10000)
df_cases_nat = df_cases  %>% group_by(country, date) %>% summarize( measure_H2_H2_deaths = mean(measure_H2_H2_deaths),
                                                                    measure_H2_H2_cases_ECDC = mean(measure_H2_H2_cases_ECDC))  

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
df = merge(policyHeterog , df_cases_nat, by = c("country","date"), all.x = TRUE)
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

# make France the reference country
df$country = factor(df$country, levels(factor(df$country))[c( 3, 4, 2 , 1)])
df = within(df, country <- relevel(country, ref = 'France'))

# make  Restrictions of Mass Gathering the reference
df$type = factor(df$type, levels(factor(df$type))[c(4, 3, 2, 1)])
df = within(df, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))


# -----------------
# EDA
# ----------------



# -----------------
# Run models
# ----------------
# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
# note that when you include time3 -- 0 or 1 is perfectly predicted
# logitA = glm(centDegR ~ type*country +measure_H2_H2_cases_ECDC+hhi_new_deaths + time  , data = df, family = 'binomial' )
# logitB = glm(centDegR ~ type*RAI +measure_H2_H2_cases_ECDC +hhi_new_deaths + time , data = df, family = 'binomial' )
# logitC = glm(centDegR ~ type*Self+measure_H2_H2_cases_ECDC +hhi_new_deaths+ time , data = df, family = 'binomial' )
#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r


# type as interaction
hyp2_country_type = lm(centDegStd ~ type*country +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp2_rai_type = lm(centDegStd ~ type*RAI +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp2_self_type = lm(centDegStd ~ type*Self +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )


# diff dum as interaction
hyp2_country_diff = lm(centDegStd ~ differentiating*country +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp2_rai_diff = lm(centDegStd ~ differentiating*RAI +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

hyp2_self_diff = lm(centDegStd ~ differentiating*Self +measure_H2_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )


# # -----------------
# Substantive effect plots
# ----------------

p_hyp2_country_type  = plot_model(hyp2_country_type , type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
p_hyp2_country_type 
ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_countrydum.pdf", p_hyp2_country_type )

p_hyp2_rai_type  = plot_model(hyp2_rai_type , type = 'pred', terms = c('RAI', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
p_hyp2_rai_type
ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_rai.pdf", p_hyp2_rai_type)


p_hyp2_self_type = plot_model(hyp2_self_type, type = 'pred', terms = c('Self', 'type'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))


ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_self.pdf", p_hyp2_self_type)

# differentating

p_hyp2_country_diff = plot_model(hyp2_country_diff, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))

p_hyp2_country_diff
ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_countrydum_diff.pdf", p_hyp2_country_diff)

p_hyp2_rai_diff = plot_model(hyp2_rai_diff, type = 'pred', terms = c('RAI', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Regional Authority Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')
p_hyp2_rai_diff
ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_rai_diff.pdf", p_hyp2_rai_diff)


p_hyp2_self_diff = plot_model(hyp2_self_diff, type = 'pred', terms = c('Self', 'differentiating'))+
  labs(y = 'Policy Centralization',
       x = 'Self Index',
       title = 'Predicted Values of Policy Centralization, \n OLS Model',
       colors = c('red', 'blue', 'green', 'purple'))
p_hyp2_self_diff

ggsave("WEP_analysis/Results/predictEffectsH2/predicted_effects_H2_ols_self_diff.pdf", p_hyp2_self_diff)
