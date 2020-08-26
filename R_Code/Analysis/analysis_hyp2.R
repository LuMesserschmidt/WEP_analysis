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

# ----------------------------
# create variables
# ----------------------------
 
# make 'differentiating' variable
df_2$differentiating = ifelse(df_2$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
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
# EDA
# ----------------



# -----------------
# Run models
# ----------------
# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
# note that when you include time3 -- 0 or 1 is perfectly predicted
# logitA = glm(centDegR ~ type*country +measure_H2_H2_cases_ECDC+hhi_new_deaths + time  , data = df_2, family = 'binomial' )
# logitB = glm(centDegR ~ type*RAI +measure_H2_H2_cases_ECDC +hhi_new_deaths + time , data = df_2, family = 'binomial' )
# logitC = glm(centDegR ~ type*Self+measure_H2_H2_cases_ECDC +hhi_new_deaths+ time , data = df_2, family = 'binomial' )
#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r

 
# type as interaction
hyp2_country_type = lm(hetero_mean ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_rai_type = lm(hetero_mean ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_self_type = lm(hetero_mean ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )


# diff dum as interaction
hyp2_country_diff = lm(hetero_mean ~ differentiating*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_rai_diff = lm(hetero_mean ~ differentiating*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_self_diff = lm(hetero_mean ~ differentiating*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )


hyp2_country_type = lm(hetero_mean ~ type*country +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_rai_type = lm(hetero_mean ~ type*RAI +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

hyp2_self_type = lm(hetero_mean ~ type*Self +measure_H1_H2_cases_ECDC +hhi_new_deaths + time + time2+time3, data = df_2 )

# # -----------------
# Substantive effect plots
# ----------------
makeH2plots = function(model){
  
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
  
  legendTitle = ifelse(any(grepl('differentiating', varNames)), 'Policy Type', 'Country')
  
  if (any(grepl('differentiating', varNames))){
    legendLabels = c('Differentiated', 'Unitary')
    legendColor = c("#E41A1C", "#377EB8" )
  } else{
    legendLabels =  c('France', 'Italy', 'Switzerland', 'Germany')
    legendColor = c("#E41A1C", "#377EB8", "#984EA3"  ,"#4DAF4A" )
  }
  
  if(any(grepl('RAI|Self', varNames))){
    
    
    plot = plot_model(model, 
                      type = 'pred', 
                      terms = c(unit, policy  )) 
    
  } else{
    legendLabels =  c('France', 'Italy', 'Switzerland', 'Germany')
    legendColor = c("#E41A1C", "#377EB8", "#984EA3"  ,"#4DAF4A" )
    plot = plot_model(model, 
                      type = 'pred', 
                      terms = c(policy, unit )
    )
  }
  
  
  plot = plot + 
    labs(y = dvLabel,
         x = unitLabel,
         title = paste0('Predicted Values of ', dvLabel, ',', '\n', modelLabel))+
    scale_color_manual(values  = legendColor,
                       name = legendTitle, 
                       labels = legendLabels)+
    scale_fill_manual(values  = legendColor,
                      name = legendTitle, 
                      labels = legendLabels)+
    coord_cartesian(clip = "off") +
    theme(legend.position="bottom")
  
  
  print(plot)
  
  modelfileName = ifelse(any(grepl('^lm', model$call)), 'ols', ' logit')
  unitFileName = ifelse(any(grepl('RAI', varNames)), 'rai', ifelse(any(grepl('Self', varNames)), 'self', 'countrydum'))
  policyFileName = ifelse(any(grepl('differentiating', varNames)), '_diff', '')


  fileName = paste0('WEP_analysis/Results/predictedEffectsH2/','predicted_effects_h2_', modelfileName, '_', unitFileName, policyFileName, '.pdf')
  ggsave(fileName, plot)

}

makeH2plots(hyp2_country_type)
makeH2plots(hyp2_rai_type)
makeH2plots(hyp2_self_type)


makeH2plots(hyp2_country_diff)
makeH2plots(hyp2_rai_diff)
makeH2plots(hyp2_self_diff)


#-------------
# format nice tables
#------------
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


