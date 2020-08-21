
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
# 
# # italy other miscoded, should be business restrictions  
# sub_data = sub_data %>% mutate_cond(policy_id == 2173638,
#                                     type = "Restriction and Regulation of Businesses")

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




# clean entry for Italy, should be a lockdown initiated at national, not municipal level
sub_data = sub_data %>% mutate_cond(policy_id %in% c( '5139191', '2759844'),
                                    init_country_level = 'National',
                                    city = NA,
                                    date_end = as.Date('2020-03-10', '%Y-%m-%d'))


# italy type miscoded, should be quarantine, not lockdown
sub_data = sub_data %>% mutate_cond(policy_id == '3591526',
                                    type = 'Quarantine')


# Italy type_who_gen miscoded, should be everyone in the municipality not 'other population not specified above'

sub_data = sub_data %>% mutate_cond(policy_id %in% c('1186194',
                                                     '1445515'),
                                    type_who_gen = "No special population targeted")




# Switzerland miscoded as social distancing, should be lockdown
sub_data = sub_data %>% mutate_cond(policy_id == '2154117',
                                    type = 'Lockdown',
                                    type_sub_cat = 'People in nursing homes/long term care facilities')


sub_data = sub_data %>% mutate_cond(policy_id %in% c('4304926'),
                                    type_sub_cat = "Postponement of a recreational or commercial event") 
 

## clean wrong dates
sub_data = sub_data %>% 
  mutate_cond(record_id == 'R_1rwDJbQOLkzV69MAj',
              date_start = as.Date("2020-03-13", "%Y-%m-%d")) %>%
  mutate_cond(record_id == 'R_BSvrVDwm7M5IF8JAj',
              date_start = as.Date("2020-02-29", "%Y-%m-%d"),
              date_announced = as.Date("2020-02-28", "%Y-%m-%d"))%>%
  mutate_cond(record_id == 'R_eEYKavm4cHzaEKJNA',
              date_start = as.Date("2020-03-14", "%Y-%m-%d"),
              date_end = as.Date("2020-04-30", "%Y-%m-%d"))%>%
  mutate_cond(record_id %in% c( 'R_2aIXQCrRqvYXDtpCu',
                                'R_2aIXQCrRqvYXDtpDp',
                                'R_2aIXQCrRqvYXDtpBf'),
              date_start = as.Date("2020-03-17", "%Y-%m-%d"),
              date_end = NA)  %>%
  mutate_cond(record_id %in% c( 'R_1r9CqgxJiBgq3GBCu',
                                'R_1r9CqgxJiBgq3GBDp',
                                'R_1r9CqgxJiBgq3GBBf'),
              date_start = as.Date("2020-03-20", "%Y-%m-%d")) %>%
  mutate_cond(record_id %in% c('R_10GFpwOB1ehsCd3Du'),
              date_end = as.Date("2020-06-03", "%Y-%m-%d"))


sub_data = sub_data %>% mutate_cond(policy_id %in% c('474049',
                                                     '1774357',
                                                     '4001371',
                                                     '3467369',
                                                     '6083880',
                                                     '3335313',
                                                     '167432',
                                                     '3333413',
                                                     '4524997',
                                                     '4126406',
                                                     '6249300',
                                                     '9846529',
                                                     '4603012')|
                                            record_id %in% c('R_ah2w0ZOyeSvMEj7NA',
                                                             'R_qOUYa35iAW11h5LNA',
                                                             'R_1HepS3Sb89rj1UPNA',
                                                             'R_3rHuIhkSsPzEOrYNA',
                                                             'R_28YRziizw7ZZOfjNA',
                                                             'R_2P8c6lkNycftS7tNA',
                                                             'R_1jCEucwkge7z3oTCj',
                                                             'R_1qe6CqFYOHk6RBANA',
                                                             'R_1HjmbY7oxNCMKBKNA',
                                                             'R_3rPojgRhfH9fjfMNA',
                                                             'R_1gegqGyoUUZdrcNNA',
                                                             'R_2pX2smox9mkUJ8ANA',
                                                             'R_3R2cCsYmeuhlnNnNA',
                                                             'R_1JK05QmhgbvBXgwNA'),
                                    type_sub_cat = "All/Unspecified mass gatherings") 


sub_data = sub_data %>% mutate_cond(policy_id %in% c('8908778', '9664505', '2000573', '8140826'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '1000') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('9239737'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '2') 


sub_data = sub_data %>% mutate_cond(policy_id %in% c('474049'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '500') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('3728913', '4415069'),
                                    type_sub_cat = "Attendance at religious services restricted (e.g. mosque/church closings)") 

sub_data = sub_data %>% mutate_cond(record_id %in% c('R_1mJaJpu5i50Y5myNA'),
                                    type = 'Other Policy Not Listed Above')

sub_data = sub_data %>% mutate_cond(record_id %in% c('R_reT4y2kUxvffllLNA',
                                                     'R_1q9AXImZ4raUmOCNA',
                                                     'R_1MX0RRHcaCgiTXaNA'),
                                    type_sub_cat = 'Other mass gatherings not specified above restricted')

sub_data = sub_data %>% mutate_cond(record_id %in% c('R_xlQQ2Wj92uVqOjfNAA',
                                                     'R_3lDo64IEsUEi7ihNA',
                                                     'R_xlQQ2Wj92uVqOjfNA'),
                                    type_sub_cat= 'Cancellation of a recreational or commercial event')





sub_data = sub_data %>% mutate(type_mass_gathering  = recode( type_mass_gathering,
                                                          "Max of 10 people at sports training/competition" = "10",
                                                          "Max of 25 people at sports training/competition"= "25",
                                                          "max. 5"= "5",
                                                          "more then 5" = '5',
                                                          "Maximum five people or two households, in private/family areas, up to 50 people, if this is for imperative reasons." = "5",
                                                          "Maximum of 10" = "10",
                                                          "No more than two households may now gather in public." = "",
                                                          "inside: 50, outside: 100" = "100",
                                                          "gatherings in public: max 30 people" = "30")) %>%
  mutate(
    type_mass_gathering = recode(type_mass_gathering,
                                 "1,000 max" = "1000")
  ) %>% mutate_cond(
    record_id %in% c("R_BRNlMFEnGMb6v1TNA"),
    type_mass_gathering = '100') %>% 
  mutate_cond(
    record_id %in% c("R_2DV3bzyAiv9dWo1NA"),
    type_mass_gathering = '150'
  ) %>% 
  mutate_cond(
    record_id %in% c("R_238I6RvAk7kxLiXNA"),
    type_mass_gathering = '200') %>%
  mutate_cond(
    record_id %in% c("R_3rPojgRhfH9fjfMNA"), # all events are forbidden unless specifically permitted by the Health Ministry with the limit of 150 people outside and 75 people in enclosed spaces
    type_mass_gathering = "150"
  ) %>% 
  mutate_cond(
    record_id %in% c("R_OfFlR1wLXeMsH0BNA"),
    type_mass_gathering = "350"
  )%>% 
  mutate_cond(
    record_id %in% c("R_2bPs7nEEJON4s3cNA"),
    type_mass_gathering = "75") %>%
  mutate_cond(
    record_id %in% c('R_1mQw6OyanKLEu6DNA'),
    type_mass_gathering = '150',
  ) %>%
  mutate_cond(
    record_id %in% c('R_1QKEi57lkPGHNY7Cj', # the Canton of Bern, Switzerland decides on February 29th that events of over 1000 participants are only allowed if organizers prove that no participants travel from affected regions and if organizers know identity of all participants
                     'R_2aaTB59xbpmsOS5Cj'),#  UPDATE: The original policy was set by the canton. On March 13, the federal government overruled the policy.
    type_mass_gathering = "1000"
  ) %>%
  mutate_cond(
    record_id %in% c('R_3jIbQpzDi4OEovLNA'),
    type_mass_gathering = "100"
  )%>%
  mutate_cond(
    record_id %in% c('R_2zkyXK53wslGNXxNA'),
    type_mass_gathering = "350"
  )%>%
  mutate_cond(
    record_id %in% c('R_1jCEucwkge7z3oTCj'),
    type_mass_gathering = "300"
  ) %>%
  mutate_cond(
    record_id %in% c('R_sb9IyocQDRID7b3NA'), # check after RAs make corrections
    type_mass_gathering = "1000"
  ) %>% mutate_cond(
    record_id %in% c("R_1NCiVEhGeKyczqOCj"),
    type_mass_gathering = '100'
  ) %>%
  mutate_cond(
    policy_id %in% c(3639091),
    type_mass_gathering = '50'
  )



## clean 'missing' types

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
 


