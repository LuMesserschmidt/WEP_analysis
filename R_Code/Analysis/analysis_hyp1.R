## analysis of H1

# load packages
library(dplyr)
library(magrittr)
library(sjPlot)
library(texreg)

# ----------------------------
# load and format data
# ----------------------------
policyCentralization = readRDS( "~/Dropbox/West European Politics Corona Article/WEP_analysis/data/CoronaNet/coronanet_network_measures.rds") 

df_main <- read_csv("WEP_analysis/data/merged_final.csv", guess_max = 10000)
df_hhi <- read_csv("WEP_analysis/Measures/hhi.csv", guess_max = 10000)

# get cases data
df_cases <- read_csv("WEP_analysis/data/Cases/cases.csv", guess_max = 10000)
df_cases_nat = df_cases  %>% group_by(country, date) %>% summarize( measure_H1_H2 = mean(measure_H1_H2))  

# merge data
df = merge(policyCentralization , df_cases_nat, by = c("country","date"), all.x = TRUE)
df = merge(df , df_hhi[, c('country', 'date', 'hhi_cumulative', 'hhi_new')], by = c("country","date"), all.x = TRUE)
df$differentiating = ifelse(type %in% c("Lockdown", "Closure and Regulation of Schools"), 1, 0)

# make time count variable
df  = df %>% group_by(country) %>% mutate(time = 1:n(),
                                    time2 = time^2,
                                    time3 = time^3) %>%
              ungroup()
 
# make a version of the DV that rounds to 0 or 1
df = df %>% mutate(centDegR = round(centDegStd))

# make France the reference country
df = within(df, country <- relevel(country, ref = 'France'))

# make  Restrictions of Mass Gathering the reference
df = within(df, type <- relevel(type, ref = 'Restrictions of Mass Gatherings'))


# -----------------
# EDA
# ----------------
hist(df$centDegStd)

# -----------------
# Run models
# ----------------

model1 = lm(centDegStd ~ type*country + hhi_new + time + time2+time3, data = df )
summary(model1)

# note that the distribution of the DV is highly bimodal; see what happens when you run a logit
  # note that when you include time3 -- 0 or 1 is perfectly predicted
model2 = glm(centDegR ~ type*country + hhi_new + time + time2 , data = df, family = 'binomial' )
summary(model2)

# -----------------
# Substantive effect plots
# ----------------
 
p1 = plot_model(model1, type = 'int')+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n OLS Model')

ggsave("WEP_analysis/Results/predicted_effects_h1_ols.pdf", p1)

p2 = plot_model(model2, type = 'int', transform = "exp",
           axis.labels = c('Policy Centralization', 'Policy Type'))+
  labs(y = 'Policy Centralization',
       x = 'Policy Type',
       title = 'Predicted Values of Policy Centralization, \n Logit Model')

ggsave("WEP_analysis/Results/predicted_effects_h1_logit.pdf", p2)
summary(model1)
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



 