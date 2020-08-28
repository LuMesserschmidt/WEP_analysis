## analysis of H1 and H2
# by Cindy Cheng

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
df = merge(df , df_fed[, c('country',  'Self',  'RAI')], by = c("country"), all.x = TRUE)

df = df %>% filter(date <= "2020-07-21")

# ----------------------------
# create variables
# ----------------------------

# make 'differentiating' variable
df$differentiating = ifelse(df$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Homogeneous') %>% factor()
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


## logit models

# type as interaction
# note that - 0 or 1 is perfectly predicted
hyp1_country_type_logit = glm(centDegR ~ type*country +measure_H1_H2_cases_ECDC+hhi_new_deaths + time  , data = df, family = 'binomial' )
hyp1_rai_type_logit = glm(centDegR ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time , data = df, family = 'binomial' )
hyp1_self_type_logit = glm(centDegR ~ type*Self+measure_H1_H2_cases_ECDC +hhi_new_deaths+ time , data = df, family = 'binomial' )
#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r

# diff dum as interaction
hyp1_country_diff_logit = glm(centDegR ~ differentiating*country +measure_H1_H2_cases_ECDC+hhi_new_deaths + time +time2 , data = df, family = 'binomial' )
hyp1_rai_diff_logit = glm(centDegR ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time  +time2+time3, data = df, family = 'binomial' )
hyp1_self_diff_logit = glm(centDegR ~ differentiating*Self+measure_H1_H2_cases_ECDC +hhi_new_deaths+ time +time2+time3 , data = df, family = 'binomial' )


## ols models

# type as interaction
hyp1_country_type = lm(centDegStd ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
hyp1_rai_type = lm(centDegStd ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
hyp1_self_type = lm(centDegStd ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )

# diff dum as interaction
hyp1_country_diff = lm(centDegStd ~ differentiating*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
hyp1_rai_diff = lm(centDegStd ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )
hyp1_self_diff = lm(centDegStd ~ differentiating*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df )


summary(hyp1_country_type)
 summary(hyp1_country_diff)
# # -----------------
# Substantive effect plots
# ----------------
# user created function to generate plots quickly
makeH1plots = function(model, pubPlot = FALSE){
  
  varNames = names(coef(model))
  
  unit = ifelse(any(grepl('RAI', varNames)), 'RAI', ifelse(any(grepl('Self', varNames)), 'Self', 'country'))
  policy = ifelse(any(grepl('differentiating', varNames)), 'differentiating', 'type')
 
  xMin = ifelse(any(grepl('deaths_a', varNames)),0 , -1)
  xMax = ifelse(any(grepl('deaths_a', varNames)),8.5 , 7.6)
  
  dvLabel = "Policy Centralization"
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
                      terms = c(policy, unit ),
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
    theme_bw() +
    theme(legend.position="bottom") 
  
  print(plot)

  modelfileName = ifelse(any(grepl('^lm', model$call)), 'ols', ' logit')
  unitFileName = ifelse(any(grepl('RAI', varNames)), 'rai', ifelse(any(grepl('Self', varNames)), 'self', 'countrydum'))
  policyFileName = ifelse(any(grepl('differentiating', varNames)), '_diff', '')

  folderName = ifelse(modelfileName == 'ols', '/OLS', '/Logit')
  
  
  if(pubPlot == FALSE){
    fileName = paste0('WEP_analysis/Results/predictEffectsH1', folderName, '/predicted_effects_h1_', modelfileName, '_', unitFileName, policyFileName, '.pdf')
    
  } else if  (pubPlot == TRUE){
    fileName = paste0('WEP_analysis/Results/predictEffectsH1', paste0(folderName, '_pub'), '/predicted_effects_h1_', modelfileName, '_', unitFileName, policyFileName, '.pdf')
    
  }
  
  ggsave(fileName, plot)

}
 
makeH1plots(hyp1_country_type)
makeH1plots(hyp1_rai_type)
makeH1plots(hyp1_self_type)

 
makeH1plots(hyp1_country_diff)
makeH1plots(hyp1_rai_diff)
makeH1plots(hyp1_self_diff)

makeH1plots(hyp1_country_diff_logit)
makeH1plots(hyp1_rai_diff_logit)
makeH1plots(hyp1_self_diff_logit)


makeH1plots(hyp1_country_type, pubPlot = TRUE)
makeH1plots(hyp1_rai_type, pubPlot = TRUE)
makeH1plots(hyp1_self_type, pubPlot = TRUE)


makeH1plots(hyp1_country_diff, pubPlot = TRUE)
makeH1plots(hyp1_rai_diff, pubPlot = TRUE)
makeH1plots(hyp1_self_diff, pubPlot = TRUE)

makeH1plots(hyp1_country_diff_logit, pubPlot = TRUE)
makeH1plots(hyp1_rai_diff_logit, pubPlot = TRUE)
makeH1plots(hyp1_self_diff_logit, pubPlot = TRUE)

0.44-.25
