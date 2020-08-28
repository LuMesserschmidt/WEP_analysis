# analysis hyp3
# by Cindy Cheng

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(ggplot2)
library(readr)
# --------------
# load and format data
# --------------

## get coronanet data
sub_data = readRDS("WEP_analysis/data/CoronaNet/coronanet_internal_sub_clean_long_h3.RDS")

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


df_cases= df_cases %>%
                mutate(measure_H3_deaths_a = if_else(measure_H3_deaths_a<0, 0, measure_H3_deaths_a)) %>%
                mutate(measure_H3_deaths_a = if_else(date <= as.Date("2020-02-23", '%Y-%m-%d') & is.na(measure_H3_deaths_a), 0, measure_H3_deaths_a)) %>%
                 mutate(measure_H3_deaths_b = if_else(date <= as.Date("2020-02-23", '%Y-%m-%d') & is.na(measure_H3_deaths_b), 0, measure_H3_deaths_b))


     
# get RAI data
df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
names(df_fed)[which(names(df_fed) == 'Jurisdiction Name')]= 'country'
df_fed = df_fed %>% dplyr:::filter(country %in% c('France', 'Germany', 'Switzerland', 'Italy'))
df_fed
# merge data
df_3 = merge(sub_data_agg %>% filter(date > "2019-12-31"), df_cases, by = c("province","date"), all.x = TRUE)
df_3 = merge(df_3 , df_fed[, c('country',   'Self',   'RAI')], by = c("country"), all.x = TRUE)

# subset data to before or on 7/21
df_3 = df_3 %>% filter(date <= "2020-07-21")
df_3 = df_3 %>% filter(date >= "2020-02-24")

# make time variable
df_3 = df_3 %>% group_by(province) %>% mutate(time = 1:n(),
                                          time2 = time^2,
                                          time3 = time^3) %>%
  ungroup()

# make France the reference country
df_3$country = factor(df_3$country, levels(factor(df_3$country))[c(1, 3, 2, 4)])
df_3 = within(df_3, country <- relevel(country, ref = 'France'))
 

# make differentiating dummy
df_3$differentiating = ifelse(df_3$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Homogeneous') %>% factor()
df_3 = within(df_3, differentiating <- relevel(differentiating, ref =  'Differentiated'))
 

 
# make  Restrictions of Mass Gathering the reference
df_3$type = factor(df_3$type, levels(factor(df_3$type))[c(4, 3, 2, 1)])
df_3 = within(df_3, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))
df_3 = df_3 %>% mutate(lpolicy_count = ifelse(policy_count == 0, 0, log(policy_count)))
summary(df_3)
df_cases %>% filter(measure_H3_deaths_a >0) %>% select(date) %>% pull %>% table 
# --------------
# eda
# --------------
hist(df_3$lpolicy_count)
hist(df_3$policy_count)
hist(log(df_3$policy_count+1))
hist(df_3$policy_dum)

hist(df_3$measure_H3_deaths_a)
hist(df_3$measure_H3_deaths_b)


# --------------
# analyze data
# --------------

# logit of policy dummy
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
hyp3_country_type_deaths_a_logit = glm(policy_dum ~ type*country*measure_H3_deaths_a +time+time2+time3   , data = df_3, family = 'binomial')
hyp3_rai_type_deaths_a_logit = glm(policy_dum ~type*RAI*measure_H3_deaths_a +time+time2+time3    , data = df_3, family = 'binomial')
hyp3_self_type_deaths_a_logit = glm(policy_dum ~ type*Self*measure_H3_deaths_a +time+time2+time3  , data = df_3, family = 'binomial')

hyp3_country_type_deaths_b_logit = glm(policy_dum ~ type*country*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_type_deaths_b_logit = glm(policy_dum ~ type*RAI*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_type_deaths_b_logit = glm(policy_dum ~ type*Self*measure_H3_deaths_b +time + time2 + time3   , data = df_3, family = 'binomial')


hyp3_country_diff_deaths_a_logit = glm(policy_dum ~ differentiating*country*measure_H3_deaths_a +time+time2+time3  , data = df_3, family = 'binomial')
hyp3_rai_diff_deaths_a_logit = glm(policy_dum ~ differentiating*RAI*measure_H3_deaths_a  +time+time2+time3 , data = df_3, family = 'binomial')
hyp3_self_diff_deaths_a_logit = glm(policy_dum ~ differentiating*Self*measure_H3_deaths_a +time +time2 +time3, data = df_3, family = 'binomial')

hyp3_country_diff_deaths_b_logit = glm(policy_dum ~ differentiating*country*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_diff_deaths_b_logit = glm(policy_dum ~ differentiating*RAI*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_diff_deaths_b_logit = glm(policy_dum ~ differentiating*Self*measure_H3_deaths_b +time  +time2 +time3 , data = df_3, family = 'binomial')


# ols of policy count
hyp3_country_type_deaths_a = lm(lpolicy_count ~ type*country*measure_H3_deaths_a  + time + time2 +time3, data = df_3)
hyp3_rai_type_deaths_a = lm(lpolicy_count ~ type*RAI*measure_H3_deaths_a   + time + time2 +time3, data = df_3)
hyp3_self_type_deaths_a = lm(lpolicy_count ~ type*Self*measure_H3_deaths_a   + time + time2 +time3, data = df_3)

hyp3_country_type_deaths_b  = lm(lpolicy_count ~ type*country*measure_H3_deaths_b   + time + time2 +time3, data = df_3)
hyp3_rai_type_deaths_b  = lm(lpolicy_count ~ type*RAI*measure_H3_deaths_b    + time + time2 +time3, data = df_3)
hyp3_self_type_deaths_b  = lm(lpolicy_count ~ type*Self*measure_H3_deaths_b + time + time2 +time3, data = df_3)
 
hyp3_country_diff_deaths_a = lm(lpolicy_count ~ differentiating*country*measure_H3_deaths_a  + time + time2 +time3, data = df_3)
hyp3_rai_diff_deaths_a= lm(lpolicy_count ~ differentiating*RAI*measure_H3_deaths_a   + time + time2 +time3, data = df_3)
hyp3_self_diff_deaths_a = lm(lpolicy_count ~ differentiating*Self*measure_H3_deaths_a   + time + time2 +time3, data = df_3)

hyp3_country_diff_deaths_b  = lm(lpolicy_count ~ differentiating*country*measure_H3_deaths_b   + time + time2 +time3, data = df_3)
hyp3_rai_diff_deaths_b = lm(lpolicy_count ~ differentiating*RAI*measure_H3_deaths_b    + time + time2 +time3, data = df_3)
hyp3_self_diff_deaths_b  = lm(lpolicy_count ~ differentiating*Self*measure_H3_deaths_b + time + time2 +time3, data = df_3)
 

# ---------------------
# substantive effect plots
# --------------------
# user created function to generate plots quickly

quantile(df_3$measure_H3_deaths_b, c(.1, .99), na.rm = TRUE)
makeH3plots = function(model, pubPlot = FALSE){
  
  varNames = names(coef(model))
  
  unit = ifelse(any(grepl('RAI', varNames)), 'RAI', ifelse(any(grepl('Self', varNames)), 'Self', 'country'))
  policy = ifelse(any(grepl('differentiating', varNames)), 'differentiating', 'type')
  deaths = ifelse(any(grepl('deaths_a', varNames)), 'measure_H3_deaths_a [0, 8.5]', 'measure_H3_deaths_b [-1, 7.5]')
  
  xMin = ifelse(any(grepl('deaths_a', varNames)),0 , -1)
  xMax = ifelse(any(grepl('deaths_a', varNames)),8.5 , 7.5)
  
  dvLabel = ifelse(any(grepl('lpolicy_count', model$call)), 'Policy Count, Logged', 'Policy Dum')
  deathsLabel = ifelse(any(grepl('deaths_a', varNames)), 'Relative Proportional Death Rate',"Relative Standardized Death Rate")
  modelLabel = ifelse(any(grepl('^lm', model$call)), ' OLS Model', ' Logit Model')
  
  legendTitle = ifelse(any(grepl('RAI', varNames)), 'RAI', ifelse(any(grepl('Self', varNames)), 'Self Rule Index', 'Country'))
  

  if(any(grepl('RAI', varNames))){
  legendLabels = c('France (0.54)', 'Switzerland (0.71)', 'Italy (0.73)', 'Germany (1.0)')
  legendColor = c("#E41A1C", "#984EA3" ,  "#377EB8",  "#4DAF4A" )
  } else if (any(grepl('Self', varNames))){
    legendLabels =  c('Switzerland (0.72)', 'France (0.80)', 'Italy (0.96)', 'Germany (1.0)')   
    legendColor = c("#984EA3"  , "#E41A1C" , "#377EB8" , "#4DAF4A" )
  } else{
    legendLabels =  c('France', 'Italy', 'Switzerland', 'Germany')
    legendColor = c("#E41A1C", "#377EB8", "#984EA3"  ,"#4DAF4A" )
    }
             
  plot = plot_model(model, 
                    type = 'pred', 
                    terms = c(deaths, unit, policy)
                    )
  
    if (pubPlot == FALSE){
      plot = plot+ 
        labs(y = dvLabel,
                          x = deathsLabel,
                          title = paste0('Predicted Values of ', dvLabel, ',', '\n', modelLabel),
                          colors = c('red', 'blue', 'green', 'purple'))}
  
  if (pubPlot == TRUE){
    plot = plot+ 
      labs(y = dvLabel,
           x = deathsLabel,
           title= NULL,
           colors = c('red', 'blue', 'green', 'purple'))}
  
  plot = plot +
    xlim(xMin, xMax)+
    scale_color_manual(values  = legendColor,
                       name = legendTitle, 
                       labels = legendLabels)+
    scale_fill_manual(values  = legendColor,
                      name = legendTitle, 
                      labels = legendLabels)+
    theme_bw()+
    theme(legend.position="bottom")
  
 
   
  modelfileName = ifelse(any(grepl('^lm', model$call)), 'ols', ' logit')
  unitFileName = ifelse(any(grepl('RAI', varNames)), 'rai', ifelse(any(grepl('Self', varNames)), 'self', 'country'))
  deathsFileName = ifelse(any(grepl('deaths_a', varNames)), 'proportion', 'standardized')
  policyFileName = ifelse(any(grepl('differentiating', varNames)), '_diff', '')

  folderName = ifelse(modelfileName == 'ols', '/OLS', '/Logit')

  if(pubPlot == FALSE){
    fileName = paste0('WEP_analysis/Results/predictedEffectsH3', folderName, '/predicted_effects_h3_', modelfileName, '_', unitFileName, '_', deathsFileName, policyFileName, '.pdf')

  } else if  (pubPlot == TRUE){
    fileName = paste0('WEP_analysis/Results/predictedEffectsH3/', paste0(folderName, '_pub'), '/predicted_effects_h3_', modelfileName, '_', unitFileName, '_', deathsFileName, policyFileName, '.pdf')

  }
  ggsave(fileName, plot)

}
 
# make ols plots
makeH3plots(hyp3_country_type_deaths_a)
makeH3plots(hyp3_rai_type_deaths_a) 
makeH3plots(hyp3_self_type_deaths_a) 

makeH3plots(hyp3_country_type_deaths_b ) 
makeH3plots(hyp3_rai_type_deaths_b ) 
makeH3plots(hyp3_self_type_deaths_b) 

makeH3plots(hyp3_country_diff_deaths_a ) 
makeH3plots(hyp3_rai_diff_deaths_a ) 
makeH3plots(hyp3_self_diff_deaths_a ) 

makeH3plots(hyp3_country_diff_deaths_b ) 
makeH3plots(hyp3_rai_diff_deaths_b ) 
makeH3plots(hyp3_self_diff_deaths_b ) 

# make logit plots
makeH3plots(hyp3_country_type_deaths_a_logit)
makeH3plots(hyp3_rai_type_deaths_a_logit )
makeH3plots(hyp3_self_type_deaths_a_logit )

makeH3plots(hyp3_country_type_deaths_b_logit ) 
makeH3plots(hyp3_rai_type_deaths_b_logit ) 
makeH3plots(hyp3_self_type_deaths_b_logit ) 

makeH3plots(hyp3_country_diff_deaths_a_logit ) 
makeH3plots(hyp3_rai_diff_deaths_a_logit ) 
makeH3plots(hyp3_self_diff_deaths_a_logit ) 

makeH3plots(hyp3_country_diff_deaths_b_logit ) 
makeH3plots(hyp3_rai_diff_deaths_b_logit ) 
makeH3plots(hyp3_self_diff_deaths_b_logit ) 
 

## plots for publication

makeH3plots(hyp3_country_type_deaths_a,  pubPlot = TRUE)
makeH3plots(hyp3_rai_type_deaths_a,  pubPlot = TRUE) 
makeH3plots(hyp3_self_type_deaths_a,  pubPlot = TRUE) 

makeH3plots(hyp3_country_type_deaths_b,  pubPlot = TRUE ) 
makeH3plots(hyp3_rai_type_deaths_b,  pubPlot = TRUE ) 
makeH3plots(hyp3_self_type_deaths_b,  pubPlot = TRUE) 

makeH3plots(hyp3_country_diff_deaths_a,  pubPlot = TRUE ) 
makeH3plots(hyp3_rai_diff_deaths_a,  pubPlot = TRUE ) 
makeH3plots(hyp3_self_diff_deaths_a,  pubPlot = TRUE ) 

makeH3plots(hyp3_country_diff_deaths_b,  pubPlot = TRUE ) 
makeH3plots(hyp3_rai_diff_deaths_b ,  pubPlot = TRUE) 
makeH3plots(hyp3_self_diff_deaths_b ,  pubPlot = TRUE) 

# make logit plots
makeH3plots(hyp3_country_type_deaths_a_logit,  pubPlot = TRUE)
makeH3plots(hyp3_rai_type_deaths_a_logit,  pubPlot = TRUE )
makeH3plots(hyp3_self_type_deaths_a_logit ,  pubPlot = TRUE)

makeH3plots(hyp3_country_type_deaths_b_logit,  pubPlot = TRUE ) 
makeH3plots(hyp3_rai_type_deaths_b_logit,  pubPlot = TRUE ) 
makeH3plots(hyp3_self_type_deaths_b_logit,  pubPlot = TRUE ) 

makeH3plots(hyp3_country_diff_deaths_a_logit,  pubPlot = TRUE ) 
makeH3plots(hyp3_rai_diff_deaths_a_logit ,  pubPlot = TRUE) 
makeH3plots(hyp3_self_diff_deaths_a_logit ,  pubPlot = TRUE) 

makeH3plots(hyp3_country_diff_deaths_b_logit ,  pubPlot = TRUE) 
makeH3plots(hyp3_rai_diff_deaths_b_logit ,  pubPlot = TRUE) 
makeH3plots(hyp3_self_diff_deaths_b_logit ,  pubPlot = TRUE) 
