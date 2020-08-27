#analysis_h2
# by Cindy Cheng

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(texreg)
library(ggplot2)
library(glmnet)
library(stargazer)
library(readr)
library(betareg)
 
# ----------------------------
# load and format data
# ----------------------------
policy_heterog = read_csv("WEP_analysis/data/heterogeneity_data.csv")
policy_heterog = policy_heterog %>% mutate( type = ifelse(type ==  "Wearing Masks inside public or commercial building", 'Mask Wearing', type),
                                            type = ifelse(type ==  "Primary Schools (generally for children ages 10 and below)", 'Closure and Regulation of Schools', type))

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
df_2 = merge(policy_heterog , df_cases_nat, by = c("country","date"), all.x = TRUE)
df_2 = merge(df_2 , df_hhi[, c('country', 'date', 'hhi_cumulative_deaths', 'hhi_new_deaths')], by = c("country","date"), all.x = TRUE)
df_2 = merge(df_2 , df_fed[, c('country',  'Self', 'RAI')], by = c("country"), all.x = TRUE)
df_2 = df_2 %>% filter(date <= "2020-07-21")
# ----------------------------
# create variables
# ----------------------------
 
# make 'differentiating' variable
df_2$differentiating = ifelse(df_2$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Homogeneous') %>% factor()
df_2 = within(df_2, differentiating <- relevel(differentiating, ref =  'Differentiated'))

# make time count variable
df_2  = df_2 %>% group_by(country) %>% mutate(time = 1:n(),
                                          time2 = time^2,
                                          time3 = time^3) %>%
  ungroup()

# make France the reference country
df_2$country = factor(df_2$country, levels(factor(df_2$country))[c( 1, 3, 2, 4)])
df_2 = within(df_2, country <- relevel(country, ref = 'France'))

# make  Restrictions of Mass Gathering the reference
df_2$type = factor(df_2$type, levels(factor(df_2$type))[c(4, 3, 2, 1)])
df_2 = within(df_2, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))

 
# -----------------
# Run models
# ----------------

# type as interaction
hyp2_country_type = lm(hetero_mean ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )
hyp2_rai_type = lm(hetero_mean ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )
hyp2_self_type = lm(hetero_mean ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )
 

# diff dum as interaction
hyp2_country_diff = lm(hetero_mean ~ differentiating*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )
hyp2_rai_diff = lm(hetero_mean ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )
hyp2_self_diff = lm(hetero_mean ~ differentiating*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )


# # -----------------
# Substantive effect plots
# ----------------
makeH2plots = function(model, pubPlot = FALSE){
  
  varNames = names(coef(model))
  
  unit = ifelse(any(grepl('RAI', varNames)), 'RAI', ifelse(any(grepl('Self', varNames)), 'Self', 'country'))
  policy = ifelse(any(grepl('differentiating', varNames)), 'differentiating', 'type')
  
  xMin = ifelse(any(grepl('deaths_a', varNames)),0 , -1)
  xMax = ifelse(any(grepl('deaths_a', varNames)),8.5 , 7.6)
  
  dvLabel = "Policy Heterogeneity"
  modelLabel = ifelse(any(grepl('^lm', model$call)), ' OLS Model', ' Logit Model')
  unitLabel =  ifelse(any(grepl('RAI', varNames)), 'Regional Authourity Index',
                      ifelse(any(grepl('Self', varNames)), 'Self Rule Index', 
                             'Policy Type'))
  
  if(any(grepl('RAI|Self', varNames))){
    
    plot = plot_model(model, 
                      type = 'pred', 
                      terms = c(unit, policy  ),
                      legend.title = 'Policy Type') 
    
  } else{
    plot = plot_model(model, 
                      type = 'pred', 
                      terms = c(policy, unit),
                      legend.title = 'Country'
                                
    )
  }
  
  
  if (pubPlot == FALSE){
    plot = plot+ 
      labs(y = dvLabel,
           x = unitLabel,
           title= paste0('Predicted Values of ', dvLabel, ',', '\n', modelLabel),
           colors = c('red', 'blue', 'green', 'purple'))}
  
  if (pubPlot == TRUE){
    plot = plot+ 
      labs(y = dvLabel,
           x = unitLabel,
           title= NULL,
           colors = c('red', 'blue', 'green', 'purple'))}
  
  plot = plot +
    coord_cartesian(clip = "off") +
    theme_bw()+
    theme(legend.position="bottom")

  
  print(plot)
  
  modelfileName = ifelse(any(grepl('^lm', model$call)), 'ols', ' logit')
  unitFileName = ifelse(any(grepl('RAI', varNames)), 'rai', ifelse(any(grepl('Self', varNames)), 'self', 'countrydum'))
  policyFileName = ifelse(any(grepl('differentiating', varNames)), '_diff', '')

  if(pubPlot == FALSE){
    fileName = paste0('WEP_analysis/Results/predictedEffectsH2/OLS/','predicted_effects_h2_', modelfileName, '_', unitFileName, policyFileName, '.pdf')
    
  }
  
  if(pubPlot == TRUE){
    fileName = paste0('WEP_analysis/Results/predictedEffectsH2/OLS_pub/','predicted_effects_h2_', modelfileName, '_', unitFileName, policyFileName, '.pdf')
    
  }
   ggsave(fileName, plot)

}

makeH2plots(hyp2_country_type)
makeH2plots(hyp2_rai_type)
makeH2plots(hyp2_self_type)


makeH2plots(hyp2_country_diff)
makeH2plots(hyp2_rai_diff)
makeH2plots(hyp2_self_diff)


makeH2plots(hyp2_country_type, pubPlot = TRUE)
makeH2plots(hyp2_rai_type, pubPlot = TRUE)
makeH2plots(hyp2_self_type, pubPlot = TRUE)


makeH2plots(hyp2_country_diff, pubPlot = TRUE)
makeH2plots(hyp2_rai_diff, pubPlot = TRUE)
makeH2plots(hyp2_self_diff, pubPlot = TRUE)
