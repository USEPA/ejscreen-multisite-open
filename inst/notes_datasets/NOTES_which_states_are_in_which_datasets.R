# # WHICH STATES OR TERRITORIES OR PR ARE IN WHICH DATASETS 
# # what is missing in each dataset as of 10/22?

# Define the universe of what states/other places might be included

stinfo   <- ejanalysis::get.state.info()
universe <- stinfo[stinfo$is.usa.plus.pr | stinfo$is.island.areas, 1:10]
# Note that state.abb from base datasets  lacks DC PR VI GU etc.
universe <- universe[universe$ST != 'UM', ]
# Note on US Minor Outlying Islands: 
# NONE of the EJ-related datasets include "U.S. Minor Outlying Islands" FIPS 74 "UM"  
# so ignore that even though listed as territories in ejanalysis::get.state.info()
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


# EJAMejscreendata package  # ****** lookup table NEEDS TO BE FIXED / UPDATED  to include 4 island areas**********

EJAM::datapack('EJAMejscreendata')
#                                            Item                          Title
# 1     EJSCREEN_Full_with_AS_CNMI_GU_VI EJScreen 2.1 data for each blo
# 2 EJSCREEN_StatePct_with_AS_CNMI_GU_VI EJScreen 2.1 data for each blo
# 3                          States_2022 EJScreen 2.1 lookup table of p
# 4                             USA_2022 EJScreen 2.1 lookup table of p
setdiff(universe$ST, unique(EJAMejscreendata::EJSCREEN_Full_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique(EJAMejscreendata::EJSCREEN_StatePct_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique(EJAMejscreendata::States_2022$REGION  ))  # ****** THIS NEEDS TO BE FIXED / UPDATED  to include 4 island areas **********
# [1] "AS" "GU" "MP" "VI"  


# ejscreen package   # ****** lookup tables and bg lists TO BE FIXED / UPDATED to include 4 island areas **********

EJAM::datapack(ejscreen)
# 9                               bg21plus 
# 12                              bg22plus 
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
setdiff(universe$ST, ejscreen::lookupStates$REGION)
# [1] "AS" "GU" "MP" "VI"
'PR' %in%  ejscreen::lookupStates$REGION
# [1] TRUE
setdiff(universe$ST, ejscreen::States_2022_LOOKUP$REGION)
'PR' %in%  ejscreen::States_2022_LOOKUP$REGION
# [1] TRUE
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, ejscreen::States_2021_LOOKUP$REGION)
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, ejscreen::bg22$ST)
# [1] "AS" "GU" "MP" "VI"
setdiff(universe$ST, ejscreen::bg22plus$ST)
# [1] "AS" "GU" "MP"  "VI"
get.state.info( setdiff(universe$FIPS.ST, substr(ejscreen::bg22DemographicSubgroups2016to2020$FIPS,1,2)))[,'ST']
# [1] "AS" "GU" "MP" "PR"  "VI"      # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********
setdiff(universe$ST,  acs_B03002_2016_2020_bg_tract$bg$STUSAB)
# [1] "AS" "GU" "MP" "PR"  "VI"  # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********
get.state.info( setdiff(universe$FIPS.ST, substr(tract22DemographicSubgroups2016to2020$FIPS,1,2)))$ST
# [1] "AS" "GU" "MP" "PR"  "VI"  # ****** missing pr here but it was put into bg22plus... may drop this? THIS NEEDS TO BE FIXED / UPDATED **********


# EJAM package # ****** bg lists TO BE FIXED / UPDATED  to include 4 island areas**********

EJAM::datapack('EJAM')
# 2                    bgpts lat lon of popwtd center of bl
# 3          blockgroupstats EJSCREEN demographic and envir
# 22               stateinfo data.frame of state abbreviati
# 23            stateregions data.table that shows which st
# 24               statesshp Shape File with boundaries of 
# 25              statestats data.table of 100 percentiles 
ejanalysis::get.state.info( setdiff(universe$FIPS.ST, substr(EJAM::bgpts$bgfips, 1,2) ))[,'ST']
# [1] "AS" "GU" "MP" "VI"
ejanalysis::get.state.info( setdiff(universe$FIPS.ST, substr(EJAM::blockgroupstats$bgfips, 1,2) ))[,'ST']
# [1] "AS" "GU" "MP" "VI"  
setdiff(universe$ST, EJAM::stateinfo$ST)
# [1] "AS" "GU" "MP" "UM" "VI"
setdiff(universe$ST, EJAM::stateregions$ST)
# [1] "AS" "GU" "MP" "PR" "VI"
setdiff(universe$ST, EJAM::statesshp$STUSPS)
# character(0)
setdiff(universe$ST, EJAM::statestats$REGION)
# [1] "AS" "GU" "MP" "UM" "VI"


# EJAMblockdata package # ****** block lists and bg list TO BE FIXED / UPDATED  to include 4 island areas**********

EJAM::datapack('EJAMblockdata')
# 1     bgid2fips BLOCK GROUP id for each BLOCK 
# 2  blockid2fips block id for each block fips c
# 3   blockpoints Decennial Census block group l
# 5      blockwts Decennial Census block weights
# 6 lookup_states basic information about US Sta
# 7      quaddata quad tree data on locations of
length(unique(substr(unique(EJAMblockdata::bgid2fips[ , bgfips], by = 'bgfips')  ,1,2)) )
# [1] 52 # has DC and PR.
ejanalysis::get.state.info(setdiff(universe$FIPS.ST, unique(substr(unique(EJAMblockdata::bgid2fips[ , bgfips], by = 'bgfips')  ,1,2))))$ST
# [1] "AS" "GU" "MP" "VI"  missing
ejanalysis::get.state.info(setdiff(universe$FIPS.ST, unique(substr(unique(EJAMblockdata::blockid2fips[ , blockfips], by = 'blockfips')  ,1,2))))$ST
# [1] "AS" "GU" "MP" "VI"
all.equal(EJAMblockdata::blockid2fips$blockid , EJAMblockdata::blockpoints$blockid)
# [1] TRUE
all.equal(EJAMblockdata::blockid2fips$blockid , EJAMblockdata::blockwts$blockid)
# [1] TRUE
all.equal(EJAMblockdata::blockid2fips$blockid , EJAMblockdata::quaddata$blockid)
# [1] TRUE
setdiff(universe$ST, EJAMblockdata::lookup_states$ST)
# character(0)
setdiff(EJAMblockdata::lookup_states$ST, universe$ST)
# [1] "UM" "US"


# proxistat package

library(proxistat)
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

# proxistat::countiesall
setdiff( universe$ST, unique(proxistat::countiesall$ST))
# [1] "AS" "GU" "MP" "UM" "VI"

# proxistat::bg.pts
ejanalysis::get.state.info(unique(substr(proxistat::bg.pts$FIPS, 1,2) ))$ST
# [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM"
# [33] "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY" "AS" "GU" "MP" "PR" "VI"
length(unique(substr(proxistat::bg.pts$FIPS,1,2)))  # 56 !
setdiff(ejanalysis::get.state.info(unique(substr(proxistat::bg.pts$FIPS, 1,2) ))$ST, state.abb)
# [1] "DC" "AS" "GU" "MP" "PR" "VI"  # the 2010 version had PR **and also the 4 island areas.** 

# proxistat::lookup.states
toupper(c("as", "gu", "mp", "um", "vi", "us")) %in% proxistat::lookup.states$ST
# TRUE TRUE TRUE TRUE TRUE TRUE


# EJAMfrsdata package

# library(EJAMfrsdata) # no info on states


# ACSdownload package

# ACSdownload::clean.mystates() relies on data(lookup.states, envir = environment(), package = 'proxistat')

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

