# filter and clean 'missing' policies
 
# select wep countries based on country/region name strings
regions = read_csv("WEP_analysis/data/CoronaNet/country_region_clean.csv")
regions = regions %>% dplyr::: filter(Country %in% countries) %>% dplyr:::select(-ISO2)
regions$`0` = regions$Country
regions = regions %>% gather(reg_num, region, -Country)
regions = regions %>% dplyr:::filter(!is.na(region))

region_names = unlist(str_split(regions$region,  '\\-'))
region_names = region_names[-which(region_names %in% c('de'))]
missing = qualtrics %>% filter(country == 'MISSING' & grepl(paste0(paste(region_names, collapse = '|'), 
                                                                   "|German|Swiss|Switz|French|Ital|german|swiss|switz|french|ital"), description))

# remove 'false positives'
missing = missing %>% filter(record_id %!in% c(
  "R_qFurEHJAxJ0E6ZPNA",
  "R_XnzEJHSVCs2YlupNA",
  "R_1JV1PLgT3QJN1tNNA",
  "R_b1qvi3pGaB4ft5vAp",
  "R_28YmLND8nufcoqRNA",
  "R_20PtpDcvInjhSkyNA",
  "R_1jNcC0tP3ek82DANA",
  "R_6M4SwZrMbf05GtbNA",
  "R_3DEyPlYsi5vRbWhNA",
  "R_1LUNJUVU9OoTS6zNA",
  "R_1CDOXh5Kf5hk6OoNA",
  "R_3njik2byz0Ipf39NA",
  "R_2S7SbGM0zjCzz5UNA",
  "R_RK2ty360YhBTv69NA")) %>% 
  filter(policy_id %!in% c(
    1320995,
    1360884,
    1630056,
    1661071,
    1701936,
    1702802,
    1713490,
    1739250,
    191015,
    2111960,
    221292,
    2261917,
    2342524,
    2425732,
    2475507,
    2486195,
    2579083,
    2791699,
    2794795,
    3000156,
    3023458,
    3190765,
    3200312,
    3214265,
    3226219,
    3433877,
    2991429,
    3462554,
    3590514,
    3737451,
    3763985,
    3868650,
    4101330,
    4107894,
    4187108,
    4324313,
    4339100,
    4514001,
    460209,
    4072432,
    4650112,
    4727941,
    485181,
    4870291,
    4968407,
    515052,
    5438608,
    5471869,
    5530770,
    5554731,
    5588387,
    5869360,
    5890240 ,
    6000447,
    6015471,
    6129027,
    6133077,
    6136578,
    617940,
    6236227,
    629441,
    6511900,
    6853421,
    6874487,
    7202698,
    7248678 ,
    7322805,
    7389075,
    7449784,
    7510484,
    7528268,
    7768948 ,
    7920088,
    8042143,
    8147627,
    8385677,
    8405988,
    8472080 ,
    8773786,
    8880419,
    9009955,
    923147,
    9585149,
    9681962,
    9777258,
    9950494,
    2211137,
    4613041,
    1650862,
    1316267,
    264912,
    4967325,
    770362,
    3370190,
    773628,
    9097213))



# recode types
type_strings = c('school', 'School', 'day care', 'Day care', 'university', 'University',
                 'lockdown', 'Lockdown', 'quarantine', 'Quarantine',
                 'estriction of mass gather', 'estriction of Mass Gather',
                 'estriction of business', 'estriction of Business',
                 'overnment service', 'overnmen Service',
                 'awareness',
                 'internal border restriction','Internal Border Restriction',
                 'external border restriction','External Border Restriction',
                 'ocial distancing', 'Social distancing',
                 'Curfew', 'curfew',
                 'ealth monitor', 'ealth Monitor',
                 'ealth test', 'ealth Test',
                 'hygiene', 'Hygiene',
                 'ew task force', 'ew Task Force',
                 'ealth Resource', 'ealth resource',
                 'usiness')


missing = missing %>% mutate(type = ifelse(type == 'MISSING', str_match(description, paste(type_strings, collapse = '|')), type))
missing = missing %>% mutate(
  type_sub_cat = recode(type_sub_cat,
                        'day care' = 'Preschool or childcare facilities (generally for children ages 5 and below)',
                        'university' = 'Higher education institutions (i.e. degree granting institutions)'),
  type = recode(type,
                'day care' = 'Closure and Regulation of Schools',
                'School' = 'Closure and Regulation of Schools',
                'school' = 'Closure and Regulation of Schools',
                'university' = 'Closure and Regulation of Schools',
                'estriction of Mass Gather' = 'Restrictions of Mass Gatherings',
                'usiness' = 'Restriction and Regulation of Businesses',
                'curfew' = 'Curfew',
                'quarantine' = 'Quarantine',
                'ealth Resource' = 'Health Resources',
                'lockdown' = 'Lockdown'),
  type = case_when(policy_id %in% c(2000573, 4603012, 3728913, 4415069, 8140826, 9239737, 3426871) ~ 'Restrictions of Mass Gatherings',
                   policy_id %in% c(107570, 1004176, 1181772, 3697000) ~ 'Other Policy Not Listed Above',
                   policy_id %in% c(4042835, 9894310)~ 'Health Testing',
                   policy_id %in% c(3711078, 5340498) ~'Restriction and Regulation of Businesses',
                   policy_id %in% c(124646, 4160415, 9387553) ~ 'Health Resources',
                   policy_id %in% c(178354, 2387161, 3976628, 1178354) ~ 'Public Awareness Campaigns',
                   policy_id %in% c(1968290, 2070522, 5387340, 570913, 7785775, 9599172) ~ 'External Border Restrictions',
                   policy_id %in% c(2703818, 4617207) ~ 'Declaration of Emergency',
                   policy_id %in% c(5825151) ~ 'Quarantine',
                   policy_id %in% c(495684, 9495684) ~ 'Lockdown',
                   TRUE ~ type))


 
# recode countries
german_regions = paste0(paste(gsub("\\-", "\\|", regions %>% filter(Country == 'Germany') %>% select(region) %>% pull), collapse = '|'), '|German|germ')
swiss_regions = paste(c(unique(gsub("Landschaft|City|\\-", "", regions %>% filter(Country == 'Switzerland') %>% select(region) %>% pull)), 'Swiss|Switz|swiss|switz'), collapse = '|')
italy_regions = paste(c(gsub("^The ", "", regions %>% filter(Country == 'Italy') %>% select(region) %>% pull), 'Ital|ital|Vatican'), collapse = '|')
french_regions = unlist(str_split(regions %>% filter(Country == 'France') %>% select(region) %>% pull, '\\-'))
french_regions = french_regions[-which(french_regions %in% 'de')]
french_regions = paste(c(french_regions, 'French', 'french', 'france'), collapse = "|")

missing = missing %>% mutate(country = ifelse(country == 'MISSING' & grepl(german_regions, description), "Germany", country))
missing = missing %>% mutate(country = ifelse(country == 'MISSING' &  grepl(swiss_regions, description), "Switzerland", country))
missing = missing  %>% mutate(country = ifelse(country == 'MISSING'  &grepl( italy_regions, description), "Italy", country))
missing = missing %>% mutate(country = ifelse(country == 'MISSING' & grepl(french_regions, description), "France", country))

missing = missing %>% mutate(country = case_when(policy_id %in%c( 1441139, 1308500) ~ 'Switzerland',
                                                 TRUE~country),
                             target_country = ifelse(is.na(target_country), country, target_country))

# recode provinces
prov_names = region_names[-which(region_names %in% c('France', 'Germany', 'Italy', 'Switzerland'))]

missing = missing %>% mutate(province = ifelse(is.na(province), str_match(description, paste(prov_names, collapse = '|')), province),
                             target_province = ifelse(is.na(target_province), str_match(description, paste(prov_names, collapse = '|')) , target_province),
                             province = case_when(policy_id %in% c(3976628) ~ 'Neuachtel',
                                                  policy_id %in% c(1308500) ~ 'Saint Gallen',
                                                  TRUE~province),
                             target_province = case_when(policy_id %in% c(3976628) ~ 'Neuachtel',
                                                         policy_id %in% c(1308500) ~ 'Saint Gallen',
                                                         TRUE~target_province),
)

missing = missing %>% mutate(init_country_level = case_when(
  policy_id %in% c(107570, 
                   5051034,
                   5077477,
                   1181772, 
                   124646,
                   1856763,
                   1968290,
                   2070522,
                   2703818,
                   3697000,
                   4160415,
                   4617207,
                   5825151,
                   6968514,
                   7785775,
                   8140826,
                   9239737,
                   1178354,
                   570913,
                   9387553)~ "National",
  TRUE~'Provincial'
))


# fix dates

missing %>% mutate(date_start = 
                   case_when(record_id == "R_1Kvv86YjNYcJlJYNA" ~ as.Date("2020-04-02", "%Y-%m-%d"),
                             record_id == "R_2w1rhYRI144xEq5NA" ~ as.Date("2020-02-27", "%Y-%m-%d"),
                             record_id == "R_3M3Qlql4Cpt6awpNA" ~ as.Date("2020-03-31", "%Y-%m-%d"),
                             record_id == "R_24qK2v7MbfHZ6cUNA" ~ as.Date("2020-03-04", "%Y-%m-%d"),
                             record_id == "R_1oHJ7fZ6XaaR7fiCl" ~ as.Date("2020-03-14", "%Y-%m-%d"),
                             record_id == "R_3EtyKkLbkyvTiS8NA" ~ as.Date("2020-02-28", "%Y-%m-%d"),
                             record_id == "R_uq6Du6oLcE0LWTLNA" ~ as.Date("2020-03-13", "%Y-%m-%d"),
                             record_id == "R_3hbxW6LjGx6laEONA" ~ as.Date("2020-04-03", "%Y-%m-%d"),
                             record_id == "R_3hbxW6LjGx6laEONA" ~ as.Date("2020-03-27", "%Y-%m-%d"),
                             record_id == "R_BKY8BIvpVWWfI0pNA" ~ as.Date("2020-03-23", "%Y-%m-%d"),
                             record_id == "R_rqkGzcEfVVDpGuZNA" ~ as.Date("2020-03-15", "%Y-%m-%d"),
                             record_id == "R_2pY9tsQ6M9zcbo3NA" ~ as.Date("2020-01-28", "%Y-%m-%d"),
                             record_id == "R_6QNEPgKb6EMjSOlNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_9RFRToH4G257vj3NA" ~ as.Date("2020-02-23", "%Y-%m-%d"),
                             record_id == "R_2tM6ncrVSTXJyJUNA" ~ as.Date("2020-03-12", "%Y-%m-%d"),
                             record_id == "R_2WUVmwiE1MCFzA2NA" ~ as.Date("2020-03-27", "%Y-%m-%d"),
                             record_id == "R_3PzsEBUGZZ9wxwONA" ~ as.Date("2020-03-21", "%Y-%m-%d"),
                             record_id == "R_1oFpnTi8hjgOXquNA" ~ as.Date("2020-01-27", "%Y-%m-%d"),
                             record_id == "R_3paMSCSzi3S9dtwNA" ~ as.Date("2020-02-11", "%Y-%m-%d"),
                             record_id == "R_3HY9mgVlloJlNAWNA" ~ as.Date("2020-03-24", "%Y-%m-%d"),
                             record_id == "R_3J2eepbZKhPsSNbNA" ~ as.Date("2020-03-04", "%Y-%m-%d"),
                             record_id == "R_3n2vqYHr61L8PITNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_2656zf74PwvFoBiNA" ~ as.Date("2020-03-27", "%Y-%m-%d"),
                             record_id == "R_1cSQFWb3Abd80lmNA" ~ as.Date("2020-03-12", "%Y-%m-%d"),
                             record_id == "R_2wHzdmNyz6ECIsVNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_2BaDK9YI4YDKntQNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_33vFHYT8Axuz2zyNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_2v79nXep664J52QNA" ~ as.Date("2020-03-30", "%Y-%m-%d"),
                             record_id == "R_3CVQ6HA8O2Vp9WsNA" ~ as.Date("2020-03-14", "%Y-%m-%d"),
                             record_id == "R_2SdarZJ9DbFc4JBNA" ~ as.Date("2020-03-21", "%Y-%m-%d"),
                             record_id == "R_eJt6gg1bjVOvOutNA" ~ as.Date("2020-03-17", "%Y-%m-%d"),
                             record_id == "R_3KPknxzcxN1vSyxNA" ~ as.Date("2020-03-10", "%Y-%m-%d"),
                             record_id == "R_2Bh5IknsciL6hj2Bf" ~ as.Date("2020-03-30", "%Y-%m-%d"),
                             record_id == "R_RUjbvTbfNm37kWtNA" ~ as.Date("2020-03-30", "%Y-%m-%d"),
                             record_id == "R_2c7Q1oEYzBtk0qYNA" ~ as.Date("2020-04-30", "%Y-%m-%d"),
                             record_id == "R_XKQ8o4MsgRFPI1HNA" ~ as.Date("2020-03-22", "%Y-%m-%d"),
                             record_id == "R_248ZzY1Ri6mfNrINA" ~ as.Date("2020-03-30", "%Y-%m-%d"),
                             record_id == "R_2XdsxTFDK27jeJXNA" ~ as.Date("2020-03-18", "%Y-%m-%d"),
                             record_id == "R_2E3S60pHl0RmtUtNA" ~ as.Date("2020-03-20", "%Y-%m-%d")
                             ),
                   date_end = case_when(
                     record_id == "R_1oHJ7fZ6XaaR7fiCl" ~ as.Date("2020-04-30", "%Y-%m-%d"),
                     record_id == "R_3PzsEBUGZZ9wxwONA" ~ as.Date("2020-05-04", "%Y-%m-%d"),
                     record_id == "R_3n2vqYHr61L8PITNA" ~ as.Date("2020-05-04", "%Y-%m-%d"),
                     record_id == "R_2wHzdmNyz6ECIsVNA" ~ as.Date("2020-06-15", "%Y-%m-%d"),
                     record_id == "R_2BbqDbgGBH85RhCNA" ~ as.Date("2020-03-27", "%Y-%m-%d"),
                     record_id == "R_2Bh5IknsciL6hj2Bf" ~ as.Date("2020-05-04", "%Y-%m-%d"),
                     record_id == "R_248ZzY1Ri6mfNrINA" ~ as.Date("2020-04-20", "%Y-%m-%d"),
                     record_id == "R_2XdsxTFDK27jeJXNA" ~ as.Date("2020-04-19", "%Y-%m-%d"),
                   ),
                   date_announced = case_when(
                     record_id == "R_1cSQFWb3Abd80lmNA" ~ as.Date("2020-03-11", "%Y-%m-%d"),
                     record_id == "R_248ZzY1Ri6mfNrINA" ~ as.Date("2020-03-30", "%Y-%m-%d")
                   )
                            )

# dummy variable for if this entry was originally 'missing' or not
missing = missing %>% mutate(missingDum = 1)
