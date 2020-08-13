
# Bavaria: quarantine/curfew miscoded, should be lockdown
sub_data = sub_data %>% mutate_cond(policy_id == 7776503,
                                    description = gsub("Curfew|Quarantine","Lockdown", description),
                                    type = 'Lockdown',
                                    type_sub_cat = NA,
                                    target_who_what = "All Residents (Citizen Residents +Foreign Residents)",
                                    domestic_policy = 1)

# Germany, Schlwesig-Holstien 'other' miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(policy_id == 11030 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on  private vehicles in public circulation")

# 7472784, 248287 Germany Schlwesig-Holstien 'other' miscoded, should be 2 policies: restriction of government services, social distancing, and restrictions on businesses

# Germany, Rheinland-Pflaz 'other' miscoded, should be restrictions of mass gatherings
sub_data = sub_data %>% mutate_cond(policy_id == 5567772 ,
                                    type = 'Restrictions of Mass Gatherings')


# Germany 'other' miscoded, should be health resources
sub_data = sub_data %>% mutate_cond(policy_id == 8816665 ,
                                    type = 'Health Resources',
                                    type_sub_cat = 'Vaccines')


# Germany Lower Saxony 'other' miscoded, should be 'internal border restrictions'
sub_data = sub_data %>% mutate_cond(policy_id == 9020379 ,
                                    type = 'Internal Border Restrictions')

# France 'other' miscoded, should be 'social distancing'
sub_data = sub_data %>% mutate_cond(policy_id == 9228473 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on  private vehicles in public circulation")

# France 'other' miscoded, should be external border restrictions
sub_data = sub_data %>% mutate_cond(policy_id == 9239915 ,
                                    type = 'External Border Restrictions')


# Switzerland, 'other' miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(policy_id == 8797046 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on ridership of subways and trams")

# Switzerland, Thurgau 'other' miscoded should be declaration of emergecny
sub_data = sub_data %>% mutate_cond(policy_id == 5284665 ,
                                    type = 'Declaration of Emergency')

# Switzerland, Schaffhausen 'other' miscoded should be restriction of mass gatherings
sub_data = sub_data %>% mutate_cond(policy_id == 5707766 ,
                                    type = 'Restrictions of Mass Gatherings')

# Switzerland, Glaurus 'other' miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(policy_id == 5801330 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on ridership of subways and trams")

# italy other miscoded, should be business restrictions  
sub_data = sub_data %>% mutate_cond(policy_id == 2173638,
                                    type = "Restriction and Regulation of Businesses")

# italy other miscoded, should be mask policy sicily mask policy  
sub_data = sub_data %>% mutate_cond(policy_id == 9861723,
                                    type = "Social Distancing",
                                    type_sub_cat = "Other Mask Wearing Policy" )


# italy other miscoded, should be government services
sub_data = sub_data %>% mutate_cond(policy_id %in% c(5034034, 2769956),
                                    type = "Restriction and Regulation of Government Services" )

# italy other miscoded, should be quarantine
sub_data = sub_data %>% mutate_cond(policy_id %in% c(6084808),
                                    type = "Quarantine" )


# italy other miscoded, should be lockdown
sub_data = sub_data %>% mutate_cond(policy_id == 8916151,
                                    type = "Lockdown")



# clean 'missing' types

# germany missing type, should be schools
sub_data = sub_data %>% mutate_cond(policy_id == 1640670 & type == 'MISSING',
                                    type = "Closure and Regulation of Schools",
                                    type_sub_cat = "Preschool or childcare facilities (generally for children ages 5 and below),Primary Schools (generally for children ages 10 and below),Secondary Schools (generally for children ages 10 to 18)",
                                    date_start = as.Date("2020-03-16", "%Y-%m-%d"),
                                    date_announced = as.Date("2020-03-13", "%Y-%m-%d"))
sub_data = sub_data %>% separate_rows(type_sub_cat, sep = ',')


# italy missing type, should be health resources
sub_data = sub_data %>% mutate_cond(policy_id == 6091782 & type == 'MISSING',
                                    type = "Other Policy Not Listed Above",
                                    type_sub_cat = "Preschool or childcare facilities (generally for children ages 5 and below),Primary Schools (generally for children ages 10 and below),Secondary Schools (generally for children ages 10 to 18)",
                                    date_start = as.Date("2020-03-16", "%Y-%m-%d"),
                                    date_announced = as.Date("2020-03-13", "%Y-%m-%d"))

# germany missing type, should be social distancing/masks
sub_data = sub_data %>% mutate_cond(policy_id == 8491763 & type == 'MISSING',
                                    type = "Social Distancing",
                                    type_sub_cat = "Wearing Masks inside public or commercial building,Other Mask Wearing Policy",
                                    date_start = as.Date("2020-04-27", "%Y-%m-%d"),
                                    date_announced = as.Date("2020-04-27", "%Y-%m-%d"))
 


