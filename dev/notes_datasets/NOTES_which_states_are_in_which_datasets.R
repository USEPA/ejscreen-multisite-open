# # WHICH STATES OR TERRITORIES OR PR ARE IN WHICH DATASETS 
# # what is missing in each dataset as of 10/22?

# EJAM package uses blockgroupstats that has race/ethnic subgroups but not in PR (and NEEDS TO BE FIXED / UPDATED  to include 4 island areas)
# EJAMejscreendata package  # ****** lookup table NEEDS TO BE FIXED / UPDATED  to include 4 island areas**********
# ejscreen package   # ****** lookup tables and bg lists TO BE FIXED / UPDATED to include 4 island areas **********

# Define the universe of what states/other places might be included

# stinfo   <- ejanalysis package file get.state.info()
stinfo <-structure(
  list(
    statename = c(
      "Alabama", "Alaska", "Arizona", 
      "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
      "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
      "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
      "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
      "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
      "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
      "Wyoming", "American Samoa", "Guam", "Northern Mariana Islands", 
      "Puerto Rico", "U.S. Minor Outlying Islands", "U.S. Virgin Islands", 
      "United States"), 
    FIPS.ST = c(
      "01", "02", "04", "05", "06", "08", 
      "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", 
      "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", 
      "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
      "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
      "56", "60", "66", "69", "72", "74", "78", NA), 
    ST = c("AL", "AK", 
           "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", 
           "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
           "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
           "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
           "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", "UM", "VI", 
           "US"), 
    ftpname = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                "California", "Colorado", "Connecticut", "Delaware", "DistrictOfColumbia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                "Montana", "Nebraska", "Nevada", "NewHampshire", "NewJersey", 
                "NewMexico", "NewYork", "NorthCarolina", "NorthDakota", "Ohio", 
                "Oklahoma", "Oregon", "Pennsylvania", "RhodeIsland", "SouthCarolina", 
                "SouthDakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                "Washington", "WestVirginia", "Wisconsin", "Wyoming", NA, NA, 
                NA, "PuertoRico", NA, NA, "UnitedStates"), 
    REGION = c(4, 10, 
               9, 6, 9, 8, 1, 3, 3, 4, 4, 9, 10, 5, 5, 7, 7, 4, 6, 1, 3, 1, 
               5, 5, 4, 7, 8, 7, 9, 1, 2, 6, 2, 4, 8, 5, 6, 10, 3, 1, 4, 8, 
               4, 6, 8, 1, 3, 10, 3, 5, 8, 9, 9, 9, 2, 9, 2, NA), 
    is.usa.plus.pr = c(TRUE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, 
                       FALSE, FALSE, FALSE), 
    is.usa = c(TRUE, TRUE, TRUE, TRUE, TRUE, 
               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
               TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
    is.state = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                 FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                 FALSE), 
    is.contiguous.us = c(TRUE, FALSE, TRUE, TRUE, TRUE, 
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, 
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                         TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, 
                         FALSE, FALSE, FALSE, FALSE), 
    is.island.areas = c(FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                        FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, 
                        TRUE, FALSE), 
    area.sqmi = c(52420L, 665384L, 113990L, 53179L, 
                  163695L, 104094L, 5543L, 2489L, 68L, 65758L, 59425L, 10932L, 
                  83569L, 57914L, 36420L, 56273L, 82278L, 40408L, 52378L, 35380L, 
                  12406L, 10554L, 96714L, 86936L, 48432L, 69707L, 147040L, 
                  77348L, 110572L, 9349L, 8723L, 121590L, 54555L, 53819L, 70698L, 
                  44826L, 69899L, 98379L, 46054L, 1545L, 32020L, 77116L, 42144L, 
                  268596L, 84897L, 9616L, 42775L, 71298L, 24230L, 65496L, 97813L, 
                  581L, 571L, 1976L, 5325L, NA, 733L, 3796744L), 
    area.sqkm = c(135767L, 
                  1723337L, 295234L, 137732L, 423967L, 269601L, 14357L, 6446L, 
                  177L, 170312L, 153910L, 28313L, 216443L, 149995L, 94326L, 
                  145746L, 213100L, 104656L, 135659L, 91633L, 32131L, 27336L, 
                  250487L, 225163L, 125438L, 180540L, 380831L, 200330L, 286380L, 
                  24214L, 22591L, 314917L, 141297L, 139391L, 183108L, 116098L, 
                  181037L, 254799L, 119280L, 4001L, 82933L, 199729L, 109153L, 
                  695662L, 219882L, 24906L, 110787L, 184661L, 62756L, 169635L, 
                  253335L, 1505L, 1478L, 5117L, 13791L, NA, 1898L, 9833519L
    ), landarea.sqmi = c(50645L, 570641L, 113594L, 52035L, 155779L, 
                         103642L, 4842L, 1949L, 61L, 53625L, 57513L, 6423L, 82643L, 
                         55519L, 35826L, 55857L, 81759L, 39486L, 43204L, 30843L, 9707L, 
                         7800L, 56539L, 79627L, 46923L, 68742L, 145546L, 76824L, 109781L, 
                         8953L, 7354L, 121298L, 47126L, 48618L, 69001L, 40861L, 68595L, 
                         95988L, 44743L, 1034L, 30061L, 75811L, 41235L, 261232L, 82170L, 
                         9217L, 39490L, 66456L, 24038L, 54158L, 97093L, 76L, 210L, 
                         182L, 3424L, NA, 134L, 3531907L), 
    landarea.sqkm = c(131171L, 
                      1477953L, 294207L, 134771L, 403466L, 268431L, 12542L, 5047L, 
                      158L, 138887L, 148959L, 16635L, 214045L, 143793L, 92789L, 
                      144669L, 211754L, 102269L, 111898L, 79883L, 25142L, 20202L, 
                      146435L, 206232L, 121531L, 178040L, 376962L, 198974L, 284332L, 
                      23187L, 19047L, 314161L, 122057L, 125920L, 178711L, 105829L, 
                      177660L, 248608L, 115883L, 2678L, 77857L, 196350L, 106798L, 
                      676587L, 212818L, 23871L, 102279L, 172119L, 62259L, 140268L, 
                      251470L, 198L, 543L, 472L, 8868L, NA, 348L, 9147594L), 
    waterarea.sqmi = c(1775L, 
                       94743L, 396L, 1143L, 7916L, 452L, 701L, 540L, 7L, 12133L, 
                       1912L, 4509L, 926L, 2395L, 593L, 416L, 520L, 921L, 9174L, 
                       4537L, 2699L, 2754L, 40175L, 7309L, 1509L, 965L, 1494L, 524L, 
                       791L, 397L, 1368L, 292L, 7429L, 5201L, 1698L, 3965L, 1304L, 
                       2391L, 1312L, 511L, 1960L, 1305L, 909L, 7365L, 2727L, 400L, 
                       3285L, 4842L, 192L, 11339L, 720L, 505L, 361L, 1793L, 1901L, 
                       NA, 599L, 264841L), 
    waterarea.sqkm = c(4597L, 245383L, 1026L, 
                       2961L, 20501L, 1170L, 1816L, 1399L, 19L, 31424L, 4951L, 11678L, 
                       2398L, 6202L, 1537L, 1077L, 1346L, 2387L, 23761L, 11750L, 
                       6990L, 7134L, 104052L, 18930L, 3907L, 2501L, 3869L, 1356L, 
                       2048L, 1027L, 3544L, 757L, 19240L, 13471L, 4397L, 10269L, 
                       3377L, 6191L, 3397L, 1324L, 5076L, 3379L, 2355L, 19075L, 
                       7064L, 1035L, 8508L, 12542L, 497L, 29367L, 1864L, 1307L, 
                       935L, 4644L, 4924L, NA, 1550L, 685926L),
    inland.sqmi = c(1058L, 
                    19304L, 396L, 1143L, 2833L, 452L, 171L, 91L, 7L, 5027L, 1412L, 
                    42L, 926L, 820L, 361L, 416L, 520L, 921L, 4562L, 2314L, 768L, 
                    486L, 2001L, 4763L, 769L, 965L, 1494L, 524L, 791L, 328L, 
                    436L, 292L, 1989L, 4052L, 1698L, 474L, 1304L, 1068L, 563L, 
                    182L, 1064L, 1305L, 909L, 5616L, 2727L, 400L, 1282L, 1715L, 
                    192L, 1997L, 720L, 8L, 8L, 6L, 76L, NA, 18L, 85650L), 
    inland.sqkm = c(2740L, 
                    49997L, 1026L, 2961L, 7339L, 1170L, 443L, 237L, 19L, 13019L, 
                    3657L, 109L, 2398L, 2124L, 935L, 1077L, 1346L, 2387L, 11815L, 
                    5992L, 1990L, 1258L, 5182L, 12336L, 1992L, 2501L, 3869L, 
                    1356L, 2048L, 849L, 1128L, 757L, 5151L, 10495L, 4397L, 1227L, 
                    3377L, 2766L, 1459L, 470L, 2755L, 3379L, 2355L, 14546L, 7064L, 
                    1035L, 3320L, 4441L, 497L, 5172L, 1864L, 21L, 21L, 16L, 198L, 
                    NA, 46L, 221827L), 
    coastal.sqmi = c(517, 26119, 0, 0, 245, 
                     0, 530, 355, 0, 1349, 46, 9, 0, 0, 0, 0, 0, 0, 2880, 591, 
                     1820, 1177, 0, 0, 616, 0, 0, 0, 0, 0, 427, 0, 975, 0, 0, 
                     0, 0, 72, 0, 64, 110, 0, 0, 401, 0, 0, 1565, 2468, 0, 0, 
                     0, 0, 1, 5, 13, NA, 16, 42336), 
    coastal.sqkm = c(1340, 67647, 
                     0, 0, 634, 0, 1372, 920, 0, 3495, 119, 23, 0, 0, 0, 0, 0, 
                     0, 7460, 1530, 4714, 3048, 0, 0, 1596, 0, 0, 0, 0, 0, 1105, 
                     0, 2526, 0, 0, 0, 0, 186, 0, 165, 286, 0, 0, 1040, 0, 0, 
                     4053, 6392, 0, 0, 0, 0, 2, 13, 34, NA, 40, 109651), 
    greatlakes.sqmi = c(0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1575, 233, 0, 0, 0, 0, 
                        0, 0, 0, 38174, 2546, 0, 0, 0, 0, 0, 0, 0, 0, 3986, 0, 0, 
                        3491, 0, 0, 748, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9342, 0, 0, 
                        0, 0, 0, NA, 0, 60095), 
    greatlakes.sqkm = c(0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 4078, 602, 0, 0, 0, 0, 0, 0, 0, 98870, 
                        6594, 0, 0, 0, 0, 0, 0, 0, 0, 10324, 0, 0, 9042, 0, 0, 1938, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24195, 0, 0, 0, 0, 0, NA, 0, 
                        155643), 
    territorial.sqmi = c(199, 49320, 0, 0, 4837, 0, 
                         0, 94, 0, 5757, 453, 4458, 0, 0, 0, 0, 0, 0, 1732, 1633, 
                         111, 1092, 0, 0, 123, 0, 0, 0, 0, 69, 506, 0, 479, 1149, 
                         0, 0, 0, 1251, 0, 266, 786, 0, 0, 1347, 0, 0, 438, 660, 0, 
                         0, 0, 497, 352, 1782, 1812, NA, 565, 76760), 
    territorial.sqkm = c(516, 
                         127739, 0, 0, 12528, 0, 0, 242, 0, 14911, 1175, 11546, 0, 
                         0, 0, 0, 0, 0, 4486, 4228, 286, 2829, 0, 0, 318, 0, 0, 0, 
                         0, 178, 1311, 0, 1240, 2976, 0, 0, 0, 3240, 0, 688, 2036, 
                         0, 0, 3489, 0, 0, 1135, 1709, 0, 0, 0, 1286, 911, 4615, 4692, 
                         NA, 1464, 198806), 
    lat = c(32.7396323, 63.346191, 34.2099643, 
            34.8955256, 37.148573, 38.9935752, 41.5797842, 38.9935501, 
            38.9041485, 28.4574302, 32.629384, 19.809767, 44.3020948, 
            40.1028754, 39.9030256, 42.0700243, 38.4985464, 37.5336807, 
            30.8577705, 45.3906022, 38.9466584, 42.1565196, 44.8410835, 
            46.3161343, 32.6864655, 38.35075, 47.0511771, 41.5438105, 
            39.3310928, 43.6708595, 40.1072744, 34.4391265, 42.9133974, 
            35.53971, 47.4569538, 40.4149297, 35.5894185, 43.9715225, 
            40.9042486, 41.5978358, 33.8741769, 44.4467957, 35.8585639, 
            31.4347032, 39.3349735, 44.0605475, 37.5222512, 47.4162296, 
            38.6472854, 44.628484, 42.9918024, -14.2638166, 13.4383, 
            14.9367835, 18.217648, NA, 18.3267485, 39.57668484), 
    lon = c(-86.8434593, 
            -152.8370679, -111.602401, -92.4446262, -119.5406515, -105.5077737, 
            -72.7466666, -75.4473739, -77.0170942, -82.4091478, -83.4232125, 
            -155.5061027, -114.5956254, -89.1526108, -86.2839503, -93.4933473, 
            -98.3834298, -85.2929841, -91.803273, -68.6574869, -76.6744939, 
            -71.4895915, -85.6593197, -94.1994801, -89.6561493, -92.4567826, 
            -109.6348174, -99.8123253, -116.6151469, -71.5811278, -74.6652012, 
            -106.1261511, -75.5962723, -79.1308636, -100.4619304, -82.7119975, 
            -97.4868683, -120.6226269, -77.8280624, -71.5252895, -80.8542699, 
            -100.2381762, -86.3493573, -99.2818238, -111.6563633, -72.673354, 
            -78.6681938, -120.5996231, -80.6183274, -89.7119299, -107.5419255, 
            -170.6620902, 144.7729285, 145.601021, -66.4107992, NA, -64.9712501, 
            -93.43364955)), 
  class = "data.frame", row.names = c(1L, 2L, 
                                      4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 14L, 15L, 16L, 17L, 18L, 
                                      19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 
                                      32L, 33L, 34L, 35L, 36L, 37L, 39L, 40L, 41L, 42L, 44L, 45L, 46L, 
                                      47L, 48L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 3L, 13L, 38L, 43L, 
                                      49L, 50L, 51L))

universe <- stinfo[stinfo$is.usa.plus.pr | stinfo$is.island.areas, 1:10]
# Note that state.abb from base datasets  lacks DC PR VI GU etc.
universe <- universe[universe$ST != 'UM', ]
# Note on US Minor Outlying Islands: 
# NONE of the EJ-related datasets include "U.S. Minor Outlying Islands" FIPS 74 "UM"  
# so ignore that even though listed as territories in ejanalysis package file get.state.info()
# Regarding Island Areas see http://www.census.gov/geo/reference/gtc/gtc_island.html which states the following: 
# Separate from the Island Areas is the term "U.S. Minor Outlying Islands." 
# The Island Areas of the United States are 
# American Samoa, Guam, the Commonwealth of the Northern Mariana Islands (Northern Mariana Islands),
# and the United States Virgin Islands. 
# The U.S. Minor Outlying Islands refers to certain small islands under U.S. jurisdiction
# in the Caribbean and Pacific: Baker Island, Howland Island, Jarvis Island, Johnston Atoll, 
# Kingman Reef, Midway Islands, Navassa Island, Palmyra Atoll, and Wake Island. 
# These areas usually are not part of standard data products.



# # what is missing in each dataset as of 10/22? ####

################ RACE/ETHNIC SUBGROUPS?

# EJAM::blockgroupstats has race/ethnic subgroups for US but not PR (and not Island Areas):
t(table(is.na(blockgroupstats$pcthisp),  get.state.info(substr(blockgroupstats$bgfips,1,2))[,"ST"] ))


################ GEOGRAPHIES?


# EJAMejscreendata package  # ****** lookup table NEEDS TO BE FIXED / UPDATED  to include 4 island areas**********

EJAM::datapack('EJAMejscreendata')
#                                            Item                          Title
# 1     EJSCREEN_Full_with_AS_CNMI_GU_VI EJScreen 2.1 data for each blo
# 2 EJSCREEN_StatePct_with_AS_CNMI_GU_VI EJScreen 2.1 data for each blo
# 3                          States_2022 EJScreen 2.1 lookup table of p
# 4                             USA_2022 EJScreen 2.1 lookup table of p
stop('need to load these from package EJAMejscreendata')
setdiff(universe$ST, unique( EJSCREEN_Full_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique( EJSCREEN_StatePct_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique( States_2022$REGION  ))  # ****** THIS NEEDS TO BE FIXED / UPDATED  to include 4 island areas **********
# [1] "AS" "GU" "MP" "VI"  


# ejscreen package   # ****** lookup tables and bg lists TO BE FIXED / UPDATED to include 4 island areas **********

EJAM::datapack(ejscreen)
# 9                               bg21plus 
# 12                              bg22plus  -- THIS HAS THE RACE/ETHNIC SUBGROUPS !
# 10                                  bg22 ACS blockgroup data for EJScre
# 11    bg22DemographicSubgroups2016to2020 Demographic subgroups of race/
# 16                          lookupStates The State-level latest version
# 17                             lookupUSA The nationwide most recent ver
# 4                     States_2021_LOOKUP 
# 5                     States_2022_LOOKUP 
# 6                        USA_2021_LOOKUP 
# 7                        USA_2022_LOOKUP 
# 8          acs_B03002_2016_2020_bg_tract 
# 40 tract22DemographicSubgroups2016to2020 Demographic subgroups of race/
stop('need lookupStates and several other files for below, from ejscreen pkg')
setdiff(universe$ST, lookupStates$REGION)
# [1] "AS" "GU" "MP" "VI"
'PR' %in%  lookupStates$REGION
# [1] TRUE
setdiff(universe$ST, States_2022_LOOKUP$REGION)
'PR' %in%  States_2022_LOOKUP$REGION
# [1] TRUE
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, States_2021_LOOKUP$REGION)
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, bg22$ST)
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, bg22plus$ST)
# [1] "AS" "GU" "MP"  "VI"
get.state.info( setdiff(universe$FIPS.ST, substr(bg22DemographicSubgroups2016to2020$FIPS,1,2)))[,'ST']
# [1] "AS" "GU" "MP" "PR"  "VI"      # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********
#   # Also see https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_prcs_handbook_2020_ch04.pdf  
#   # script to add PR demog subgroups was in /EJAM/dev/notes_datasets/4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R 
setdiff(universe$ST,  acs_B03002_2016_2020_bg_tract$bg$STUSAB)
# [1] "AS" "GU" "MP" "PR"  "VI"  # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********
get.state.info( setdiff(universe$FIPS.ST, substr(tract22DemographicSubgroups2016to2020$FIPS,1,2)))$ST
# [1] "AS" "GU" "MP" "PR"  "VI"  # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********




# EJAM package # ****** bg lists TO BE FIXED / UPDATED  to include 4 island areas**********

EJAM::datapack('EJAM')
# 2                    bgpts lat lon of popwtd center of bl
# 3          blockgroupstats EJSCREEN demographic and envir
# 22               stateinfo data.frame of state abbreviati
# 25              statestats data.table of 100 percentiles 
# ejanalysis package file get.state.info( setdiff(universe$FIPS.ST, substr(EJAM::blockgroupstats$bgfips, 1,2) ))[,'ST'] 
# [1] "AS" "GU" "MP" "VI"  
# ejanalysis package file get.state.info( setdiff(universe$FIPS.ST, substr(EJAM::bgpts$bgfips, 1,2) ))[,'ST']
# [1] "AS" "GU" "MP" "VI"
# setdiff(universe$ST, EJAM::stateinfo$ST)
# [1] "AS" "GU" "MP" "UM" "VI"
# setdiff(universe$ST, EJAM::statestats$REGION)
# [1] "AS" "GU" "MP" "UM" "VI"


#  # ****** block lists and bg list TO BE FIXED / UPDATED  to include 4 island areas**********

##### obsolete:   EJAM::datapack('EJAMblockdata') # now loaded from AWS S3 bucket during .onLoad
# 1     bgid2fips BLOCK GROUP id for each BLOCK 
# 2  blockid2fips block id for each block fips c
# 3   blockpoints Decennial Census block group l
# 5      blockwts Decennial Census block weights
# 6 lookup_states basic information about US Sta
# 7      quaddata quad tree data on locations of
length(unique(substr(unique( bgid2fips[ , bgfips], by = 'bgfips')  ,1,2)) )
# [1] 52 # has DC and PR.
# ejanalysis package file get.state.info(setdiff(universe$FIPS.ST, unique(substr(unique( bgid2fips[ , bgfips], by = 'bgfips')  ,1,2))))$ST
# [1] "AS" "GU" "MP" "VI"  missing
# ejanalysis package file get.state.info(setdiff(universe$FIPS.ST, unique(substr(unique( blockid2fips[ , blockfips], by = 'blockfips')  ,1,2))))$ST
# [1] "AS" "GU" "MP" "VI"
all.equal( blockid2fips$blockid ,  blockpoints$blockid)
# [1] TRUE
all.equal( blockid2fips$blockid ,  blockwts$blockid)
# [1] TRUE
all.equal( blockid2fips$blockid ,  quaddata$blockid)
# [1] TRUE
setdiff(universe$ST,  lookup_states$ST)
# character(0)
setdiff( lookup_states$ST, universe$ST)
# [1] "UM" "US"


# proxistat package

# library(
#   proxistat
#   )
unique(countiesall$ST)
substr(data(package='proxistat')$results[ , c('Item', 'Title')], 1, 55)
# Data sets in package ‘proxistat’:
#      Item              Title                                                    
# [1,] "bg.pts"          "Block group internal points and areas (square meters) f"
# [2,] "bg.pts_2010"     ""                                                       
# [3,] "countiesall"     "Counties information from U.S. Census Bureau from 2021" 
# [4,] "county.pts"      "approx lat lon of each US County by FIPS - needs updati"
# [5,] "county.pts_2010" ""                                                       
# [6,] "lookup.states"   "States and related areas dataset" 

### need proxistat pkg dataset called countiesall and bg.pts and lookup.states
# setdiff( universe$ST, unique(countiesall$ST))
# [1] "AS" "GU" "MP" "UM" "VI"

#  bg.pts from proxistat pkg
# ejanalysis package file get.state.info(unique(substr( bg.pts$FIPS, 1,2) ))$ST
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM"
# [33] "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY" "AS" "GU" "MP" "PR" "VI"
# length(unique(substr( bg.pts$FIPS,1,2)))  # 56 !
# setdiff(ejanalysis package file get.state.info(unique(substr( bg.pts$FIPS, 1,2) ))$ST, state.abb)
# [1] "DC" "AS" "GU" "MP" "PR" "VI"  # the 2010 version had PR **and also the 4 island areas.** 

#  lookup.states
# toupper(c("as", "gu", "mp", "um", "vi", "us")) %in% lookup.states$ST
# TRUE TRUE TRUE TRUE TRUE TRUE


#   


# ACSdownload package

# clean.mystates() relies on data(lookup.states, envir = environment(), package = 'proxistat')

# ???????????



############################################################# #
# 
# > universe[,1:3]
#                   statename FIPS.ST ST

# 1                   Alabama      01 AL
# 2                    Alaska      02 AK
# 4                   Arizona      04 AZ
# 5                  Arkansas      05 AR
# 6                California      06 CA
# 7                  Colorado      08 CO
# 8               Connecticut      09 CT
# 9                  Delaware      10 DE
# 10     District of Columbia      11 DC
# 11                  Florida      12 FL
# 12                  Georgia      13 GA
# 14                   Hawaii      15 HI
# 15                    Idaho      16 ID
# 16                 Illinois      17 IL
# 17                  Indiana      18 IN
# 18                     Iowa      19 IA
# 19                   Kansas      20 KS
# 20                 Kentucky      21 KY
# 21                Louisiana      22 LA
# 22                    Maine      23 ME
# 23                 Maryland      24 MD
# 24            Massachusetts      25 MA
# 25                 Michigan      26 MI
# 26                Minnesota      27 MN
# 27              Mississippi      28 MS
# 28                 Missouri      29 MO
# 29                  Montana      30 MT
# 30                 Nebraska      31 NE
# 31                   Nevada      32 NV
# 32            New Hampshire      33 NH
# 33               New Jersey      34 NJ
# 34               New Mexico      35 NM
# 35                 New York      36 NY
# 36           North Carolina      37 NC
# 37             North Dakota      38 ND
# 39                     Ohio      39 OH
# 40                 Oklahoma      40 OK
# 41                   Oregon      41 OR
# 42             Pennsylvania      42 PA
# 44             Rhode Island      44 RI
# 45           South Carolina      45 SC
# 46             South Dakota      46 SD
# 47                Tennessee      47 TN
# 48                    Texas      48 TX
# 52                     Utah      49 UT
# 53                  Vermont      50 VT
# 54                 Virginia      51 VA
# 55               Washington      53 WA
# 56            West Virginia      54 WV
# 57                Wisconsin      55 WI
# 58                  Wyoming      56 WY
# 3            American Samoa      60 AS
# 13                     Guam      66 GU
# 38 Northern Mariana Islands      69 MP
# 43              Puerto Rico      72 PR
# 50      U.S. Virgin Islands      78 VI

