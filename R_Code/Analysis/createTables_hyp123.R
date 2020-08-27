# create word tables
# note must run analysis_hyp1, analysis_hyp2, and analysis_hyp3 before running this code

library(texreg)

# ---------------------
# format nice tables
# --------------------

# create coefficient map
coefMap = list(
  'RAI' = 'RAI',
  'Self' = 'Self Rule Index',
  
  
  "countrySwitzerland" = "Switzerland",
  "countryGermany" = "Germany",
  "countryItaly" = "Italy",
  
  
  "typeRestrictions of Mass Gatherings" = 'Restrictions of Mass Gatherings Dum',
  'typeLockdown' = "Lockdown Dum",
  'typeMask Wearing' = 'Mask Wearing Dum',
  'typeClosure and Regulation of Schools' = 'Schools Dum',
  
  'differentiatingHomogeneous' = 'Homogeneous Policy Dum',
  
  'typeRestrictions of Mass Gatherings:countrySwitzerland' = 'Restrictions of Mass Gatherings Dum * Switzerland',
  'typeRestrictions of Mass Gatherings:countryGermany' = 'Restrictions of Mass Gatherings Dum * Germany',
  'typeRestrictions of Mass Gatherings:countryItaly' = 'Restrictions of Mass Gatherings Dum * Italy',
  
  'typeLockdown:countrySwitzerland' = 'Lockdown Dum * Switzerland',
  'typeLockdown:countryGermany' = 'Lockdown Dum * Germany',
  'typeLockdown:countryItaly' = 'Lockdown Dum * Italy',
  
  'typeClosure and Regulation of Schools:countrySwitzerland' = 'Schools Dum * Switzerland',
  'typeClosure and Regulation of Schools:countryGermany' = 'Schools Dum * Germany',
  'typeClosure and Regulation of Schools:countryItaly' = 'Schools Dum * Italy',
  
  'typeMask Wearing:countrySwitzerland' = 'Mask Wearing Dum * Switzerland',
  'typeMask Wearing:countryGermany' = 'Mask Wearing Dum * Germany',
  'typeMask Wearing:countryItaly' = 'Mask Wearing Dum * Italy',
  
  'countryItaly:differentiatingHomogeneous' = 'Homogeneous Policy Dum * Italy',
  'countryGermany:differentiatingHomogeneous' = 'Homogeneous Policy Dum * Germany',
  'countrySwitzerland:differentiatingHomogeneous' = 'Homogeneous Policy Dum * Switzerland',
  
  'differentiatingHomogeneous:countryItaly' = 'Homogeneous Policy Dum * Italy',
  'differentiatingHomogeneous:countryGermany' = 'Homogeneous Policy Dum * Germany',
  'differentiatingHomogeneous:countrySwitzerland' = 'Homogeneous Policy Dum * Switzerland',
  
  
  'typeMask Wearing:RAI' = 'Mask Wearing Dum * RAI',
  'typeLockdown:RAI' = 'Lockdown Dum * RAI',
  'typeClosure and Regulation of Schools:RAI' = 'SchoolsDum * RAI',
  
  'typeMask Wearing:Self' = 'Mask Wearing Dum * Self Rule Index',
  'typeLockdown:Self' = 'Lockdown Dum * Self Rule Index',
  'typeClosure and Regulation of Schools:Self' = 'SchoolsDum * Self Rule Index',
  
  
  'differentiatingHomogeneous:RAI' = 'Homogeneous Policy Dum * RAI',
  'differentiatingHomogeneous:Self' = 'Homogeneous Policy Dum * Self Rule Index',

  'measure_H3_deaths_a' = 'Prop. Death Rate',
  'measure_H3_deaths_b' = 'Std. Death Rate',
  
  'RAI:measure_H3_deaths_b' = 'Std. Death Rate * RAI',
  'RAI:measure_H3_deaths_a' = 'Prop. Death Rate * RAI',
  
  'Self:measure_H3_deaths_a' =   'Prop. Death Rate * Self Rule Index',
  'Self:measure_H3_deaths_b' =   'Std. Death Rate * Self Rule Index',
  
  'countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Switzerland',
  
  'countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Germany',
  'countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Italy',
  'countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate * Switzerland',
  
  'differentiatingHomogeneous:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum',
  'differentiatingHomogeneous:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum',
  
  'typeLockdown:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum',
  'typeMask Wearing:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum',
  'typeClosure and Regulation of Schools:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum',
  
  'typeLockdown:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum',
  'typeMask Wearing:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum',
  'typeClosure and Regulation of Schools:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum',
  
  'typeLockdown:countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Germany',
  'typeLockdown:countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Italy',
  'typeLockdown:countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Switzerland',
  
  'typeLockdown:countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Germany',
  'typeLockdown:countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Italy',
  'typeLockdown:countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Switzerland',
  
  'typeClosure and Regulation of Schools:countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * Germany',
  'typeClosure and Regulation of Schools:countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * Italy',
  'typeClosure and Regulation of Schools:countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * Switzerland',
  
  'typeClosure and Regulation of Schools:countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum * Germany',
  'typeClosure and Regulation of Schools:countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum * Italy',
  'typeClosure and Regulation of Schools:countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum * Switzerland',
  
  'typeMask Wearing:countryGermany:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * Germany',
  'typeMask Wearing:countryItaly:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * Italy',
  'typeMask Wearing:countrySwitzerland:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * Switzerland',
  
  'typeMask Wearing:countryGermany:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum * Germany',
  'typeMask Wearing:countryItaly:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum * Italy',
  'typeMask Wearing:countrySwitzerland:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum * Switzerland',
  
  
  'typeLockdown:RAI:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * RAI',
  'typeMask Wearing:RAI:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * RAI',
  'Closure and Regulation of Schools:RAI:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * RAI',
  
  'typeLockdown:RAI:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * RAI',
  'typeMask Wearing:RAI:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing Dum * RAI',
  'typeClosure and Regulation of Schools:RAI:measure_H3_deaths_b' = 'Std. Death Rate * Schools * RAI',
  
  'typeLockdown:Self:measure_H3_deaths_a' = 'Prop. Death Rate * Lockdown Dum * Self Rule Index',
  'typeMask Wearing:Self:measure_H3_deaths_a' = 'Prop. Death Rate * Mask Wearing Dum * Self Rule Index',
  'typeClosure and Regulation of Schools:Self:measure_H3_deaths_a' = 'Prop. Death Rate * Schools Dum * Self Rule Index',
  
  'typeLockdown:Self:measure_H3_deaths_b' = 'Std. Death Rate * Lockdown Dum * Self Rule Index',
  'typeMask Wearing:Self:measure_H3_deaths_b' = 'Std. Death Rate * Mask Wearing * Self Rule Index',
  'typeClosure and Regulation of Schools:Self:measure_H3_deaths_b' = 'Std. Death Rate * Schools Dum * Self Rule Index',
  
  'countryItaly:differentiatingHomogeneous:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum * Italy',
  'countryGermany:differentiatingHomogeneous:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum * Germany',
  'countrySwitzerland:differentiatingHomogeneous:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum * Switzerland',
  
  'countryItaly:differentiatingHomogeneous:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum * Italy',
  'countryGermany:differentiatingHomogeneous:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum * Germany',
  'countrySwitzerland:differentiatingHomogeneous:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum * Switzerland',
  
  'differentiatingHomogeneous:RAI:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum * RAI',
  'differentiatingHomogeneous:RAI:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum * RAI',
  
  'differentiatingHomogeneous:Self:measure_H3_deaths_a' = 'Prop. Death Rate * Homogeneous Policy Dum * Self Rule Index',
  'differentiatingHomogeneous:Self:measure_H3_deaths_b' = 'Std. Death Rate * Homogeneous Policy Dum * Self Rule Index',
  
  "hhi_new_deaths" = "HHI (new deaths)",
  'measure_H1_H2_cases_JHU' = 'National Cases Count',
  'measure_H1_H2_cases_ECDC' = 'National Cases Count',
  
  'time' = 'time',
  'time2' = 'time2',
  'time3' = 'time3',
  '(Intercept)' = "Intercept"
)


 
htmlreg(list(hyp1_self_diff, hyp2_self_diff, hyp3_self_diff_deaths_a_logit),
          custom.coef.map = coefMap,
          custom.model.names = c('H1', 'H2', 'H3'),
          caption.above = TRUE,
          file = 'WEP_analysis/Results/tables/table_self_diff_deaths_prop.doc', 
          inline.css = FALSE, 
          doctype = TRUE, 
          html.tag = TRUE, 
          head.tag = TRUE, 
          body.tag = TRUE)
 
htmlreg(list(hyp1_self_diff, hyp2_self_diff, hyp3_self_diff_deaths_b_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
       file = 'WEP_analysis/Results/tables/table_self_diff_deaths_std.doc',
       inline.css = FALSE,
       doctype = TRUE,
       html.tag = TRUE,
       head.tag = TRUE,
       body.tag = TRUE
       )


htmlreg(list(hyp1_rai_diff, hyp2_rai_diff, hyp3_rai_diff_deaths_a_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_rai_diff_deaths_prop.doc', 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE)

htmlreg(list(hyp1_rai_diff, hyp2_rai_diff, hyp3_rai_diff_deaths_b_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_rai_diff_deaths_std.doc', 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE)


htmlreg(list(hyp1_country_diff, hyp2_country_diff, hyp3_country_diff_deaths_a_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_country_diff_deaths_prop.doc', 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE)

htmlreg(list(hyp1_country_diff, hyp2_country_diff, hyp3_country_diff_deaths_b_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_country_diff_deaths_std.doc', 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE)

 

htmlreg(list(hyp1_country_type, hyp2_country_type, hyp3_country_type_deaths_a_logit),
          custom.coef.map = coefMap,
          custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_country_type_deaths_prop.doc', 
        inline.css = FALSE, 
        doctype = TRUE, 
        html.tag = TRUE, 
        head.tag = TRUE, 
        body.tag = TRUE
          
)


htmlreg(list(hyp1_country_type, hyp2_country_type, hyp3_country_type_deaths_b_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
       file = 'WEP_analysis/Results/tables/table_country_type_deaths_std.doc',
     inline.css = FALSE,
     doctype = TRUE,
     html.tag = TRUE,
     head.tag = TRUE,
     body.tag = TRUE

)


htmlreg(list(hyp1_self_type, hyp2_self_type, hyp3_self_type_deaths_b_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_self_type_deaths_std.doc',
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE
        
)



htmlreg(list(hyp1_self_type, hyp2_self_type, hyp3_self_type_deaths_a_logit),
        custom.coef.map = coefMap,
        custom.model.names = c('H1', 'H2', 'H3'),
        caption.above = TRUE,
        file = 'WEP_analysis/Results/tables/table_self_type_deaths_prop.doc',
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE
        
)



