

## Remove irrelevant entries

# remove irrelevant mass gathering policies policies
sub_data = sub_data %>% filter(record_id %!in% c("R_3kzcET6o5pQyvP0NA",  # February 28,2020 Switzerland Council of Basel is cancelling the carnival of Basel ( target date 02-03-20 till 04-03-20)
                                                 "R_1lxIGQvx9vZZYM8NA", # In Switzerland based on Art. 6 und 7c CO-VID-19-Verordnung 2 (mass gathering at events and gatherings in public) from the 2nd of April on the new COVID law allows the police of Aargau to monitor public spaces in Aargau through cameras and use imaging devices of third parties without the permission of the federal commissioner for data protection and public issues.
                                                 'R_1eCZx9LHWfawhbdNA',# currently miscoded As of May 19, the préfet of Haute-Corse closed all beaches, coasts, and coastal pathways in the department until March 31, prohibiting access to pedestrians, cyclists, and non-motorized vehicle traffic.
                                                 'R_qLC2DxYYk0CEQ25NA', # currently miscoded As of May 19, the préfet of Haute-Corse closed all beaches, coasts, and coastal pathways in the department until March 31, prohibiting access to pedestrians, cyclists, and non-motorized vehicle traffic.
                                                 'R_3ku0aYbVr9CHtEVNA',# currently miscoded  The Canton Zurich (Switzerland) recommends not to hold any event with participants from Italy, China, South Korea or Iran, with close bodily contact in closed facilities (eg clubs) or events with external visitors in care facilities and nursing homes.
                                                 'R_2tM6ncrVSTXJyJUNA',# curently micoded, should be a city level policy 
                                                 'R_1CHDPKjV4cVUREBNA',# miscoded, shouldn't be restrictions of mass gatherings
                                                 'R_sHdwM3uydrZkLvzNA',# The Canton Zurich (Switzerland) recommends not to hold any event with participants from Italy, China, South Korea or Iran, with close bodily contact in closed facilities (eg clubs) or events with external visitors in care facilities and nursing homes.
                                                 'R_reT4y2kUxvffllLNA',# In Italy Ligury region says Sampdoria-Verona match to be played "behind closed doors" on March 2
                                                 # 'R_pLy55nCKN6rp7gdNA',# On February 25, the Swiss canton of St. Gallen announced that there are no restrictions on mass gatherings and no events will be canceled. Organizers should provide the canton with lists of participants and contact details; especially international participants
                                                 'R_1q9AXImZ4raUmOCNA',# On 3rd of March the canton of Zurich in Switzerland recommends to cancel events that include people from Italy, China, South Korea and Iran.
                                                 'R_3mlDLgfh7yZRBlaNA', # On the 28th of May, the Berlin Senate announced that from the 30th of May onwards, public demonstrations in the open would be allowed again, without any limitation to the number of participants, provided that the minimum distance of 1.5 meters and other hygiene rules are observed.
                                                 'R_1MX0RRHcaCgiTXaNA',# From March 9, 2020, the prefects of Corsica and Corse-du-Sud in France announced an obligation for sports events to take place behind closed doors.
                                                 'R_blUCYuaQoqSHAl3NA',  # Switzerland, Geneva--"The cantonal police, with the support of the Civil Protection (PCi), proceeded this morning, Saturday March 28, to a cordon of the lakeside surroundings in order to prohibit the parking of private vehicles.
                                                 'R_1JUu6XJWWJSvPJ3NA', # From March 9, 2020, the prefects of Corsica and Corse-du-Sud in France announced the closing of public baths except for closed-door competitions.
                                                 'R_1kUebz9zyXeMQhUNA', # As of March 19, the préfet of Haute-Corse closed all beaches, coasts, and coastal pathways in the department until March 31, prohibiting access to pedestrians, cyclists, and non-motorized vehicle traffic. On March 28, this policy was extended until April 15. UPDATE: The original beach and coastal pathway ban was set to end on March 31. On March 28, this was extended until April 15.
                                                 'R_3G8uMNTTO2IBD7SNA', # In the German state of Rheinland-Pfalz, different households will now be allowed to take residence with each other in public spaces as of May 13
                                                 'R_vBNxXQwwxlUCukNNA', # The Canton of Zug allows ice hockey games in the Bossard Arena to continue without spectators on February 28th 2020
                                                 'R_xlQQ2Wj92uVqOjfNA', # The Italian government is banning all sporting matches and public events until next month in several regions of Northern Italy (restrictions of mass gatherings).\n\nThe new measures, approved last night, extends the urgent steps the government is taking for the containment of the coronavirus outbreak outside the exclusion zone. \n\nThe decree bans all events and sport matches in public and private locations from Febuary 26 until March 1.
                                                 'R_1mJaJpu5i50Y5myNA', # In Italy Ligury region suspends check-in and guest entrance to university dormitories from March 1 till the midnight of March 8
                                                 'R_reT4y2kUxvffllLNA',# In Italy Ligury region says Sampdoria-Verona match to be played "behind closed doors" on March 2
                                                 'R_1LSBNoVwpZakIfZNA')) # Sonja already recoded, can be deleted
                                                  
# remove 'lockdown' policies for FranceCorsica --- they are not lockdown policies, should be removed from the dataset
sub_data = sub_data %>% filter(policy_id %!in% c(5328165,
                                                 4995594,
                                                 5380034,
                                                 2123620))

# italy, remove duplicate entry
sub_data = sub_data %>% filter(policy_id %!in% c(9846529))

# italy, remove nuisance entry for lockdown, this will need to be corrected at some point, but easier to just remove
sub_data = sub_data %>% filter(policy_id %!in% c(4850883))
sub_data = sub_data %>% filter(record_id %!in% c('R_10GFpwOB1ehsCd3Du'))

# switzerland, remove duplicate national restrictions of mas gathering entry
sub_data = sub_data %>% filter(policy_id %!in% c(3333413))

# germany, sarrland remove duplicate lockdown entry
sub_data = sub_data %>% filter(policy_id %!in% c(9495684))

# germany, remove duplicate national restriction of mass gatherings entries
sub_data = sub_data %>% filter(policy_id %!in% c(3143804))



## Recode entries

#  'lockdown' policy for France this shoudld be recoded as social distancing
sub_data = sub_data %>% 
  mutate_cond(policy_id %in% c(1974229),
              type = 'Social Distancing')

# Bavaria: quarantine/curfew miscoded, should be lockdown
sub_data = sub_data %>% mutate_cond(policy_id == 7776503,
                                    description = gsub("Curfew|Quarantine","Lockdown", description),
                                    type = 'Lockdown',
                                    type_sub_cat = NA,
                                    target_who_what = "All Residents (Citizen Residents +Foreign Residents)",
                                    domestic_policy = 1)


# Germany, Rheinland-Pflaz 'other' miscoded, should be restrictions of mass gatherings
sub_data = sub_data %>% mutate_cond(policy_id == 5567772 ,
                                    type = 'Restrictions of Mass Gatherings')


# Switzerland, Schaffhausen 'other' miscoded should be restriction of mass gatherings
sub_data = sub_data %>% mutate_cond(policy_id == 5707766 ,
                                    type = 'Restrictions of Mass Gatherings')

# italy other miscoded, should be mask policy sicily mask policy  
sub_data = sub_data %>% mutate_cond(policy_id == 9861723,
                                    type = "Social Distancing",
                                    type_sub_cat = "Other Mask Wearing Policy" )



# clean entry for Italy, should be a lockdown initiated at national, not municipal level
sub_data = sub_data %>% mutate_cond(policy_id %in% c( '5139191', '2759844'),
                                    init_country_level = 'National',
                                    city = NA,
                                    date_end = as.Date('2020-03-10', '%Y-%m-%d'))


# Switzerland miscoded as social distancing, should be lockdown
sub_data = sub_data %>% mutate_cond(policy_id == 2154117,
                                    type = 'Lockdown',
                                    type_sub_cat = 'People in nursing homes/long term care facilities')

# swiss ticino miscoded as restrictions of mass gatherings, should be restrictions of businesses/gov
sub_data = sub_data %>% mutate_cond(policy_id == 2295515,
                                    type = "Restriction and Regulation of Businesses" )



# france init_country_level miscoded, should be provincial
sub_data = sub_data %>% mutate_cond(policy_id == 5260883,
                                    init_country_level = 'Provincial') 


# italy lockdown miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(record_id == 'R_3kbq7bFJPPTtS2gNA',
                                    type = 'Social Distancing') 

# france lockdown miscoded, should be restrictions of businesses
sub_data = sub_data %>% mutate_cond(policy_id == 2123620,
                                    type = "Restriction and Regulation of Businesses" )

# italy --- should be two policies, but keeping just the one on mandatory use of masks in public transport
sub_data = sub_data %>% mutate_cond(record_id == 'R_1Nlw2c9SIZiQGHIEr' ,
                                    type_sub_cat = "Other Mask Wearing Policy",
                                    compliance =  "Mandatory (Unspecified/Implied)")

# germany, mass gathering miscoded should be restrictions of businesses
sub_data = sub_data %>% mutate_cond(policy_id == 8100308,
                                    type = "Restriction and Regulation of Businesses" )



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


sub_data = sub_data %>% mutate_cond(policy_id == 1843419 & type == 'MISSING',
                                    province = 'Bremen',
                                    type =  'Closure and Regulation of Schools',
                                    type_sub_cat = "Preschool or childcare facilities (generally for children ages 5 and below)")

sub_data = sub_data %>% mutate_cond(policy_id == 9165956  & type == 'MISSING',
                                    province = 'Geneva',
                                    type =  'Closure and Regulation of Schools',
                                    type_sub_cat = "Primary Schools (generally for children ages 10 and below)",
                                    date_start = as.Date("2020-04-30", "%Y-%m-%d"))




# clean type_who_gen
sub_data = sub_data %>% mutate_cond(
  policy_id %in% c('2154117',
                   '7818804',
                   '9903168',
                   '1352319'),
                    type_who_gen = 'People in nursing homes/long term care facilities'
) %>% mutate_cond(
  policy_id %in% c('9292517'),
                  type_who_gen = 'Other population not specifed above',
)%>% mutate_cond(
  record_id %in% c('R_1HjmbY7oxNCMKBKNA'),
  type_sub_cat = 'Other mass gatherings not specified above restricted'
)%>% mutate_cond(
  policy_id %in% c(1186194, # Italy type_who_gen miscoded, should be everyone in the municipality not 'other population not specified above'
                    1445515,
                   741217,
                   4730905,
                   4578339, 
                   6107102, 
                   7394364, 
                   6848445, 
                   2631265, 
                   5195126, 
                   5793020, 
                   3145490, 
                   7641508, 
                   7816864, 
                   9667684,
                   3268249,
                   6936559,
                   8806284, 
                   8052058, 
                   5979520, 
                   647125, 
                   4188041,
                   1859975, 
                   5226788, 
                   1179383, 
                   2167133, 
                   6855278, 
                   7560600, 
                   731714, 
                   1794942, 
                   2386845, 
                   2886370, 
                   4377294, 
                   7367924, 
                   8209036, 
                   8496475,
                   4231455,
                   5762826),
                    type_who_gen = "No special population targeted")



# clean target country
sub_data = sub_data %>% mutate_cond(
                      policy_id %in% c(8434835),
                      target_country = 'France') %>%
                    mutate_cond(
                      policy_id %in% c(4324965, 9239737, 411172, 9952814, 1640670, 5122018, 8491763),
                      target_country = 'Germany'
                    )

## clean wrong dates
sub_data = sub_data %>% 
  # mutate_cond(record_id == 'R_1rwDJbQOLkzV69MAj',
  #             date_start = as.Date("2020-03-13", "%Y-%m-%d")) %>%
  # mutate_cond(record_id == 'R_BSvrVDwm7M5IF8JAj',
  #             date_start = as.Date("2020-02-29", "%Y-%m-%d"),
  #             date_announced = as.Date("2020-02-28", "%Y-%m-%d"))%>%
  mutate_cond(record_id == 'R_eEYKavm4cHzaEKJNA',
              date_start = as.Date("2020-03-14", "%Y-%m-%d"),
              date_end = as.Date("2020-04-30", "%Y-%m-%d"))%>%
  # mutate_cond(record_id %in% c('R_10GFpwOB1ehsCd3Du'),
  #             date_end = as.Date("2020-06-03", "%Y-%m-%d")) %>%
  #mutate_cond(policy_id %in% c(9590203),
  #           date_end = as.Date("2020-06-06", "%Y-%m-%d")) %>% # https://www.admin.ch/gov/en/start/documentation/media-releases/media-releases-federal-council.msg-id-79268.html
  mutate_cond(record_id %in% c('R_3hcPvfIHbqLvynNNA'),
              date_start =as.Date("2020-02-24", "%Y-%m-%d") ) %>%
  mutate_cond(record_id %in% c('R_PA0Psuk3mF6TvLbAn', 'R_PA0Psuk3mF6TvLbCs'), # coder accidently put 5/17 instead of 3/17
              date_start =as.Date("2020-03-17", "%Y-%m-%d") )%>%
  mutate_cond(record_id %in% c('R_1qe6CqFYOHk6RBANA','R_3nr5vkzUFgjXhjkNA'), # end date should be 3/13 not 6/20
              date_end =as.Date("2020-03-13", "%Y-%m-%d") )%>%
  mutate_cond(record_id %in% c('R_3328OauaKxuc7SvNA','R_9pHSE7nIOuRAKjLNA'), # end date should be 3/16 not 6/20
              date_end =as.Date("2020-03-16", "%Y-%m-%d"),
              type_mass_gathering = '100')%>%
  mutate_cond(record_id %in% c('R_1o72xRdGBzv9Z51Ah'), # start date should be 3/16 not 3/17; end date should be 6/6 not 4/19
              date_start =as.Date("2020-03-16", "%Y-%m-%d"),
              date_end =as.Date("2020-06-06", "%Y-%m-%d"))%>%
  mutate_cond(record_id %in% c('R_3e4vvQd0GyoWTXxCj'), # end date should be june 22, not blank
              date_end =as.Date("2020-06-22", "%Y-%m-%d"))
         




# change compliance measures
sub_data = sub_data %>%
  mutate_cond(record_id %in% c("R_3KPknxzcxN1vSyxNA"),
            compliance = "Voluntary/Recommended but No Penalties") %>%
  mutate_cond(record_id %in% 
     c("R_elAEbJyf0gujRbHCi", 
       "R_2CHdP2cD0joISWAEs", 
       "R_2CHdP2cD0joISWACi", 
       "R_1cV3JVqRdmGDsujEr",
       "R_1cV3JVqRdmGDsujEs", 
       "R_1Pe1wlpB8X8KR9KEs", 
       "R_1ml1q75EQ1fAXAJEs", 
       "R_1ml1q75EQ1fAXAJCi",
       'R_24ei5TTHd0Md9jWCi', 
       'R_e4mnXh4FEiywibvCi', 
       'R_27iXuNAigP77HXvCi',
       'R_OBc0ZZH8ZddVsGJNA',
       "R_1gjXKjAgizlMciCCv", 
       "R_3EuSJwRBYTg2cgUCv",
       "R_1f1VSvPEJnvmqmlCv", 
       "R_1jU4fge97Jy4FdnCv", 
       "R_1MWXRrl0x3hqkNHCv",
       'R_7WLyowJS9OwbKxjCv'),
     compliance = 'Mandatory (Unspecified/Implied)' )

 unique(sub_data$compliance)

# adjust mass restrictions category

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
                                                     '4603012',
                                                     '411172',
                                                     '1981688',
                                                     '2267407',
                                                     '2679041',
                                                     '3779518',
                                                     '3135093',
                                                     '4223201',
                                                     '5126100',
                                                     '5828989',
                                                     '6155066',
                                                     '6249300',
                                                     '6310955',
                                                     '6620376',
                                                     '6766468',
                                                     '7505960',
                                                     '7463826',
                                                     '7505960',
                                                     '7749192',
                                                     '7862501',
                                                     '8280241',
                                                     '8379520',
                                                     '8379520',
                                                     '8768058',
                                                     '9008440',
                                                     '9952814',
                                                     '667813',
                                                     '1229568',
                                                     '1746787',
                                                     '1938903',
                                                     '1990503',
                                                     '6221664',
                                                     '7066318',
                                                     '7752252',
                                                     '8523470',
                                                     '256638',
                                                     '2378155',
                                                     '3156419',
                                                     '2769110',
                                                     '5169904',
                                                     '1824507',
                                                     '3120762',
                                                     '6650185',
                                                     '7042359',
                                                     '4472397',
                                                     '3285136',
                                                     '4456314',
                                                     '8010781',
                                                     '4730905',
                                                     '4797956',
                                                     '970254',
                                                     '9010893',
                                                     "5376164", 
                                                     "6279485",
                                                     "4655419",
                                                     "7222692",
                                                     "933706", 
                                                     "6192024", 
                                                     "6279485",
                                                     "3143804",
                                                     '6547663',
                                                     '84950',
                                                     '97903',
                                                     '249869',
                                                     '643690',
                                                     '741217',
                                                     '1245934',
                                                     '1549982',
                                                     '4425840',
                                                     '5260883',
                                                     '8218737',
                                                     '9086049',
                                                     '6547663',
                                                     '8972618',
                                                     '8908778',
                                                     '6681622',
                                                     '9590203',
                                                     '4363753',
                                                     '9921723'
                                                     )|
                                            record_id %in% c('R_ah2w0ZOyeSvMEj7NA',
                                                             'R_qOUYa35iAW11h5LNA',
                                                             'R_1HepS3Sb89rj1UPNA',
                                                             'R_3rHuIhkSsPzEOrYNA',
                                                             'R_28YRziizw7ZZOfjNA',
                                                             'R_2P8c6lkNycftS7tNA',
                                                             'R_1jCEucwkge7z3oTCj',
                                                             'R_1qe6CqFYOHk6RBANA',
                                                             #'R_1HjmbY7oxNCMKBKNA',
                                                             'R_3rPojgRhfH9fjfMNA',
                                                             'R_1gegqGyoUUZdrcNNA',
                                                             'R_2pX2smox9mkUJ8ANA',
                                                             'R_3R2cCsYmeuhlnNnNA',
                                                             'R_1JK05QmhgbvBXgwNA'),
                                    type_sub_cat = "All/Unspecified mass gatherings") 


# change date for hamburg restriction of mass gathering
sub_data = sub_data %>% mutate_cond(record_id == "R_ah2w0ZOyeSvMEj7NA",
                         date_start = as.Date("2020-03-14", "%Y-%m-%d"),
                         date_end = as.Date("2020-03-14", "%Y-%m-%d"),
                         )
 
sub_data = sub_data %>% mutate_cond(policy_id %in% c('4304926'),
                                    type_sub_cat = "Postponement of a recreational or commercial event") 

sub_data = sub_data %>% mutate_cond(policy_id %in% c( '9664505', '2000573', '8140826', '7343179'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '1000') 




sub_data = sub_data %>% mutate_cond(policy_id %in% c('8066334'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '75') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('757552'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '100') 


sub_data = sub_data %>% mutate_cond(record_id %in% c('R_3POjmt0Ax7ShYryAg'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '1000',
                                    type_who_gen = 'No special population targeted') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('1382039'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_who_gen =  'No special population targeted' ) 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('9239737'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '2') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('474049'),
                                    type_sub_cat = "All/Unspecified mass gatherings",
                                    type_mass_gathering = '500') 

sub_data = sub_data %>% mutate_cond(policy_id %in% c('3728913', '4415069')|
                                      record_id %in% c('R_3HwvvANQMC4hR9ENA'),
                                    type_sub_cat = "Attendance at religious services restricted (e.g. mosque/church closings)") 

sub_data = sub_data %>% mutate_cond(record_id %in% c('R_1mJaJpu5i50Y5myNA'),
                                    type = 'Other Policy Not Listed Above')

sub_data = sub_data %>% mutate_cond(policy_id %in% c(8248035)|
                                      record_id %in% c('R_2COe9Uwo1Q6CHDgNA', 'R_2eOObK67RpRGaorNA'),
                                    type_sub_cat = 'Other mass gatherings not specified above restricted')


sub_data = sub_data %>% mutate_cond(record_id %in% c('R_reT4y2kUxvffllLNA',
                                                     'R_1q9AXImZ4raUmOCNA',
                                                     'R_1MX0RRHcaCgiTXaNA'),
                                    type_sub_cat = 'Other mass gatherings not specified above restricted')

sub_data = sub_data %>% mutate_cond(record_id %in% c( 'R_3lDo64IEsUEi7ihNA',
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
                                                          "gatherings in public: max 30 people" = "30",
                                                          'Outdoor event max of 350, indoor event max of 150' = "350",
                                                          "From 150 to 999" = "1000",
                                                          '0; 5; 50' = "",
                                                          "Max 100 people per room, not more than 1000 in total (both excluding active participants)" = "1000",
                                                          'more than 1000' = "1000",
                                                          'less than 250' = "250",
                                                          '150-1000' = "1000",
                                                          "nN" = "2",
                                                          'inside: 100, outside: 200' = '100',
                                                          '30 for gatherings and 300 for manifestations' = '300')) %>%
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
  ) %>% mutate_cond(
    policy_id %in% 6155066,
    type_mass_gathering = ""
  ) %>% mutate_cond(
    record_id == 'R_3F2jJtrxhgZ4LciAg',
    type_mass_gathering = "200",
  )%>% mutate_cond(
    record_id == 'R_2ceFzxz80PWbXfZAh',
    type_mass_gathering = ""
  )%>% mutate_cond(
    record_id == 'R_TcMRS1GeErWQPgBCj',
    type_mass_gathering = "2"
  ) %>% mutate_cond(
    policy_id == 9554383,
    type_mass_gathering = "2"
  )

# ---------------

# Germany, Schlwesig-Holstien 'other' miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(policy_id == 11030 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on  private vehicles in public circulation")


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

# Switzerland, Glaurus 'other' miscoded, should be social distancing
sub_data = sub_data %>% mutate_cond(policy_id == 5801330 ,
                                    type = 'Social Distancing',
                                    type_sub_cat = "Restrictions on ridership of subways and trams")
# 
# # italy other miscoded, should be business restrictions  
# sub_data = sub_data %>% mutate_cond(policy_id == 2173638,
#                                     type = "Restriction and Regulation of Businesses")



# italy other miscoded, should be government services
sub_data = sub_data %>% mutate_cond(policy_id %in% c(5034034, 2769956),
                                    type = "Restriction and Regulation of Government Services" )

# italy other miscoded, should be quarantine
sub_data = sub_data %>% mutate_cond(policy_id %in% c(6084808),
                                    type = "Quarantine" )


# italy type miscoded, should be quarantine, not lockdown
sub_data = sub_data %>% mutate_cond(policy_id == '3591526',
                                    type = 'Quarantine')

