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


df_cases = df_cases %>%mutate(measure_H3_deaths_a = ifelse(measure_H3_deaths_a<0, 0, measure_H3_deaths_a))


# get RAI data
df_fed <- read_csv("WEP_analysis/Measures/fed.csv", guess_max = 10000)
names(df_fed)[which(names(df_fed) == 'Jurisdiction Name')] = 'country'
df_fed = df_fed %>% filter(country %in% c('France', 'Germany', 'Switzerland', 'Italy'))

# merge data
df_3 = merge(sub_data_agg %>% filter(date > "2019-12-31"), df_cases, by = c("province","date"), all.x = TRUE)
df_3 = merge(df_3 , df_fed[, c('country',   'Self',   'RAI')], by = c("country"), all.x = TRUE)


# make time variable
df_3 = df_3 %>% group_by(province) %>% mutate(time = 1:n(),
                                          time2 = time^2,
                                          time3 = time^3) %>%
  ungroup()

# make France the reference country
df_3$country = factor(df_3$country, levels(factor(df_3$country))[c(1, 3, 2, 4)])
df_3 = within(df_3, country <- relevel(country, ref = 'France'))
 

# make differentiating dummy
df_3$differentiating = ifelse(df_3$type %in% c("Lockdown", "Closure and Regulation of Schools"), 'Differentiated', 'Unitary') %>% factor()
df_3 = within(df_3, differentiating <- relevel(differentiating, ref =  'Differentiated'))
 

 
# make  Restrictions of Mass Gathering the reference
df_3$type = factor(df_3$type, levels(factor(df_3$type))[c(4, 3, 2, 1)])
df_3 = within(df_3, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))
df_3 = df_3 %>% mutate(lpolicy_count = ifelse(policy_count == 0, 0, log(policy_count)))


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
hyp3_country_type_deaths_a_logit = glm(policy_dum ~ country*type*measure_H3_deaths_a  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_type_deaths_a_logit = glm(policy_dum ~ RAI*type*measure_H3_deaths_a  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_type_deaths_a_logit = glm(policy_dum ~ Self*type*measure_H3_deaths_a +time + time2 + time3   , data = df_3, family = 'binomial')

hyp3_country_type_deaths_b_logit = glm(policy_dum ~ country*type*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_type_deaths_b_logit = glm(policy_dum ~ RAI*type*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_type_deaths_b_logit = glm(policy_dum ~ Self*type*measure_H3_deaths_b +time    , data = df_3, family = 'binomial')


hyp3_country_diff_deaths_a_logit = glm(policy_dum ~ country*differentiating*measure_H3_deaths_a  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_diff_deaths_a_logit = glm(policy_dum ~ RAI*differentiating*measure_H3_deaths_a  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_diff_deaths_a_logit = glm(policy_dum ~ Self*differentiating*measure_H3_deaths_a +time + time2 + time3   , data = df_3, family = 'binomial')

hyp3_country_diff_deaths_b_logit = glm(policy_dum ~ country*differentiating*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_rai_diff_deaths_b_logit = glm(policy_dum ~ RAI*differentiating*measure_H3_deaths_b  +time + time2 + time3  , data = df_3, family = 'binomial')
hyp3_self_diff_deaths_b_logit = glm(policy_dum ~ Self*differentiating*measure_H3_deaths_b +time    , data = df_3, family = 'binomial')


# ols of policy count
hyp3_country_type_deaths_a = lm(lpolicy_count ~ country*type*measure_H3_deaths_a  + time + time2 +time3, data = df_3)
hyp3_rai_type_deaths_a = lm(lpolicy_count ~ RAI*type*measure_H3_deaths_a   + time + time2 +time3, data = df_3)
hyp3_self_type_deaths_a = lm(lpolicy_count ~ Self*type*measure_H3_deaths_a   + time + time2 +time3, data = df_3)

hyp3_country_type_deaths_b  = lm(lpolicy_count ~ country*type*measure_H3_deaths_b   + time + time2 +time3, data = df_3)
hyp3_rai_type_deaths_b  = lm(lpolicy_count ~ RAI*type*measure_H3_deaths_b    + time + time2 +time3, data = df_3)
hyp3_self_type_deaths_b  = lm(lpolicy_count ~ Self*type*measure_H3_deaths_b + time + time2 +time3, data = df_3)
 
hyp3_country_diff_deaths_a = lm(lpolicy_count ~ country*differentiating*measure_H3_deaths_a  + time + time2 +time3, data = df_3)
hyp3_rai_diff_deaths_a= lm(lpolicy_count ~ RAI*differentiating*measure_H3_deaths_a   + time + time2 +time3, data = df_3)
hyp3_self_diff_deaths_a = lm(lpolicy_count ~ Self*differentiating*measure_H3_deaths_a   + time + time2 +time3, data = df_3)

hyp3_country_diff_deaths_b  = lm(lpolicy_count ~ country*differentiating*measure_H3_deaths_b   + time + time2 +time3, data = df_3)
hyp3_rai_diff_deaths_b = lm(lpolicy_count ~ RAI*differentiating*measure_H3_deaths_b    + time + time2 +time3, data = df_3)
hyp3_self_diff_deaths_b  = lm(lpolicy_count ~ Self*differentiating*measure_H3_deaths_b + time + time2 +time3, data = df_3)
 

# ---------------------
# substantive effect plots
# --------------------
# user created function to generate plots quickly
makeH3plots = function(model){
  
  varNames = names(coef(model))
  
  unit = ifelse(any(grepl('RAI', varNames)), 'RAI', ifelse(any(grepl('Self', varNames)), 'Self', 'country'))
  policy = ifelse(any(grepl('differentiating', varNames)), 'differentiating', 'type')
  deaths = ifelse(any(grepl('deaths_a', varNames)), 'measure_H3_deaths_a [0, 8.5]', 'measure_H3_deaths_b [-1, 7.6]')
  
  xMin = ifelse(any(grepl('deaths_a', varNames)),0 , -1)
  xMax = ifelse(any(grepl('deaths_a', varNames)),8.5 , 7.6)
  
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
                    )+
    #geom_rug( position = "jitter", sides = 'b')+
    #geom_rug( position = "jitter", sides = 'b')+
    xlim(xMin, xMax)+
    labs(y = dvLabel,
         x = deathsLabel,
         title = paste0('Predicted Values of ', dvLabel, ',', '\n', modelLabel),
         colors = c('red', 'blue', 'green', 'purple'))+
    scale_color_manual(values  = legendColor,
                       name = legendTitle, 
                       labels = legendLabels)+
    scale_fill_manual(values  = legendColor,
                       name = legendTitle, 
                       labels = legendLabels)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
    theme(legend.position="bottom")
  
   
  modelfileName = ifelse(any(grepl('^lm', model$call)), 'ols', ' logit')
  unitFileName = ifelse(any(grepl('RAI', varNames)), 'rai', ifelse(any(grepl('Self', varNames)), 'self', 'country'))
  deathsFileName = ifelse(any(grepl('deaths_a', varNames)), 'proportion', 'standardized')
  policyFileName = ifelse(any(grepl('differentiating', varNames)), '_diff', '')

  folderName = ifelse(modelfileName == 'ols', 'OLS/', 'Logit/')

  fileName = paste0('WEP_analysis/Results/predictedEffectsH3/', folderName, 'predicted_effects_h3_', modelfileName, '_', unitFileName, '_', deathsFileName, policyFileName, '.pdf')
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
  
  'countryItaly:differentiatingUnitary' = 'Unitary Policy Dum * Italy',
  'countryGermany:differentiatingUnitary' = 'Unitary Policy Dum * Germany',
  'countrySwitzerland:differentiatingUnitary' = 'Unitary Policy Dum * Switzerland',
  
  
  'RAI:typeMask Wearing' = 'Mask Wearing Dum * RAI',
  'RAI:typeLockdown' = 'Lockdown Dum * RAI',
  'RAI:typeClosure and Regulation of Schools' = 'SchoolsDum * RAI',
  
  'Self:typeMask Wearing' = 'Mask Wearing Dum * Self',
  'Self:typeLockdown' = 'Lockdown Dum * Self',
  'Self:typeClosure and Regulation of Schools' = 'SchoolsDum * Self',
  'Self:measure_H3_deaths_b' =   'Std. Death Rate * Self',
  
  
  'RAI:differentiatingUnitary' = 'Unitary Policy Dum * RAI',
  'Self:differentiatingUnitary' = 'Unitary Policy Dum * Self Rule Index',
  
  'RAI:measure_H3_deaths_b' = 'Std. Death Rate * RAI',
  'RAI:measure_H3_deaths_a' = 'Prop Death Rate * RAI',
    

  'countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Switzerland',
  
  'countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate* Switzerland',
  
  'differentiatingUnitary:measure_H3_deaths_a' = 'Prop. Death Rate * Unitary Policy Dum',
  'differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum',
  
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
  'RAI:typeMask Wearing:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * RAI',
  'RAI:Closure and Regulation of Schools:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * RAI',
  
  'RAI:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * RAI',
  'RAI:typeMask Wearing:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum * RAI',
  'RAI:typeClosure and Regulation of Schools:measure_H3_deaths_b' = 'Std. Death Rate * Schools * RAI',
  
  'Self:typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Self',
  'Self:typeMask Wearing:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * Self',
  'Self:typeClosure and Regulation of Schools:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * Self',
  
  'Self:typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Self',
  'Self:typeMask Wearing:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing * Self',
  'Self:typeClosure and Regulation of Schools:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum * Self',
  
  
  'countryItaly:differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum * Italy',
  'countryGermany:differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum * Germany',
  'countrySwitzerland:differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum * Switzerland',
  
  'countryItaly:differentiatingUnitary:measure_H3_deaths_b' = 'Prop. Death Rate * Unitary Policy Dum * Italy',
  'countryGermany:differentiatingUnitary:measure_H3_deaths_b' = 'Prop. Death Rate * Unitary Policy Dum * Germany',
  'countrySwitzerland:differentiatingUnitary:measure_H3_deaths_b' = 'Prop. Death Rate * Unitary Policy Dum * Switzerland',
  
  'RAI:differentiatingUnitary:measure_H3_deaths_a' = 'Prop. Death Rate * Unitary Policy Dum * RAI',
  'RAI:differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum * RAI',
  
  'Self:differentiatingUnitary:measure_H3_deaths_a' = 'Prop. Death Rate * Unitary Policy Dum * Self Rule Index',
  'Self:differentiatingUnitary:measure_H3_deaths_b' = 'Std. Death Rate * Unitary Policy Dum * Self Rule Index',
  
  'measure_H1_H2_cases_ECDC' = 'National Cases Count',
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
)


summary(hyp3_rai_diff_deaths_b)
htmlreg(list(hyp3_country_diff_deaths_b, hyp3_country_diff_deaths_a),
       custom.coef.map = coefMap )
 
texreg(list(model3a, model3b, model3c),
       custom.coef.map = coefMap,
       custom.model.names = c( 'Logit', 'Poisson', "OLS"))

 
