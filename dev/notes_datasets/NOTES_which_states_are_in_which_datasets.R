# # WHICH STATES OR TERRITORIES OR PR ARE IN WHICH DATASETS in other packages,
# # what is missing in each dataset as of 10/22?

# also see
browseURL("https://www.census.gov/programs-surveys/decennial-census/decade/2020/planning-management/release/2020-island-areas-data-products.html")

# EJAM package uses blockgroupstats that has race/ethnic subgroups but not in PR (and NEEDS TO BE FIXED / UPDATED  to include 4 island areas)
# EJAMejscreendata package  # ****** lookup table NEEDS TO BE FIXED / UPDATED  to include 4 island areas**********
# ejscreen package   # ****** lookup tables and bg lists TO BE FIXED / UPDATED to include 4 island areas **********

# Define the universe of what states/other places might be included

# stinfo   <- ejanalysis package file get.state.info()
# now this is EJAM :: stateinfo2
# library(
#   EJAM
#   )
stinfo <-  stateinfo2

universe <- stinfo[stinfo$is.usa.plus.pr | stinfo$is.island.areas, 1:10]
universe <- universe[universe$ST != 'UM', ]

# Note that state.abb from base datasets  lacks DC PR VI GU etc. but stateinfo2 has them all.

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



# # what is missing in each dataset in various packages as of 10/22? ####

################ RACE/ETHNIC SUBGROUPS?

# EJAM blockgroupstats has race/ethnic subgroups for US but not PR (and not Island Areas):
# t(table(is.na(blockgroupstats$pcthisp), 
        # ejanalysis ::
        # get.state.info(substr(blockgroupstats$bgfips,1,2))[,"ST"] ))
#    FALSE  TRUE
# AK   503     1
# AL  3915    10
# AR  2292     2
# AS     0    77
# AZ  4729    44
# CA 25516    91
# CO  4037    21
# CT  2702    14
# DC   555    16
# DE   700     6
# FL 13290    98
# GA  7431    15
# GU     0    58
# HI  1036    47
# IA  2701     2
# ID  1279     5
# IL  9883    15
# IN  5273    17
# KS  2425    36
# KY  3548    33
# LA  4267    27
# MA  5076    40
# MD  4036    43
# ME  1174    10
# MI  8135   251
# MN  4628    78
# MO  5013    18
# MP     0   135
# MS  2438     7
# MT   898     2
# NC  7067    44
# ND   627     5
# NE  1648     0
# NH   994     3
# NJ  6521    78
# NM  1598    16
# NV  1944    19
# NY 15620   450
# OH  9416    56
# OK  3310    64
# OR  2956    14
# PA 10026   147
# PR  2487    68
# RI   785     7
# SC  3387    21
# SD   694     0
# TN  4538    24
# TX 18501   137
# UT  2003    17
# VA  5914    49
# VI     0   416
# VT   544     8
# WA  5294    17
# WI  4671    21
# WV  1637     2
# WY   456     1

################ GEOGRAPHIES?


# EJAMejscreendata package  # ****** lookup table NEEDS TO BE FIXED / UPDATED  to include 4 island areas**********

stop('be sure you want to do this... it loads all relevant packages to get the info on each...and detach/unloads each that had not been there\n')
datapack('EJAMejscreendata') # an EJAM function 
# The ones at least 1 MB in size: 
#   
#            Package                                 Item   size                    Title.Short
# 2 EJAMejscreendata                          States_2022   2 MB EJScreen 2.1 lookup table of p
# 3 EJAMejscreendata                                cejst  50 MB CEJST list of communities and 
# 4 EJAMejscreendata EJSCREEN_StatePct_with_AS_CNMI_GU_VI 306 MB EJScreen 2.1 data for each blo
# 5 EJAMejscreendata     EJSCREEN_Full_with_AS_CNMI_GU_VI 317 MB EJScreen 2.1 data for each blo

stop('would need to load these from package EJAMejscreendata')
setdiff(universe$ST, unique( EJSCREEN_Full_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique( EJSCREEN_StatePct_with_AS_CNMI_GU_VI$ST_ABBREV )) 
# character(0)
setdiff(universe$ST, unique( States_2022$REGION  ))  # ****** THIS NEEDS TO BE FIXED / UPDATED  to include 4 island areas **********
# [1] "AS" "GU" "MP" "VI"  


# ejscreen package   # ****** lookup tables and bg lists TO BE FIXED / UPDATED to include 4 island areas **********
stop('be sure you want to do this... it loads all relevant packages to get the info on each...and detach/unloads each that had not been there\n')
datapack("ejscreen") # an EJAM function 
# The ones at least 1 MB in size: 
#   
#     Package                                  Item   size                    Title.Short
# 32 ejscreen                    States_2022_LOOKUP   1 MB                               
# 33 ejscreen                          lookupStates   2 MB The State-level latest version
# 34 ejscreen tract22DemographicSubgroups2016to2020  20 MB Demographic subgroups of race/
#   35 ejscreen    bg22DemographicSubgroups2016to2020  57 MB Demographic subgroups of race/
#   36 ejscreen         acs_B03002_2016_2020_bg_tract 188 MB                               
# 37 ejscreen                                  bg22 361 MB blockgroup data from the EPA E


stop('need lookupStates and several other files for below, from ejscreen and or ejanalysis pkgs')
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

datapack('EJAM') # an EJAM function 

# The ones at least 1 MB in size: 
#   
#     Package                       Item   size                    Title.Short
# 132    EJAM                 statestats   4 MB data.frame of 100 percentiles 
# 133    EJAM              bg_cenpop2020   8 MB data.table with all US Census 
# 134    EJAM                frs_by_mact  14 MB MACT NESHAP subpart(s) that ea
# 135    EJAM           states_shapefile  16 MB US States boundaries 2020 shap
# 136    EJAM testdata_sites2blocks_1000  17 MB test data for 1000 points to u
# 137    EJAM                      bgpts  23 MB lat lon of popwtd center of bl
# 138    EJAM               frs_by_naics  58 MB data.table of NAICS code(s) fo
# 139    EJAM                 frs_by_sic  94 MB data.table of SIC code(s) for 
# 140    EJAM                       bgej 123 MB EJScreen EJ Indexes for Census
# 141    EJAM            blockgroupstats 212 MB EJScreen demographic and envir
# 142    EJAM           frs_by_programid 472 MB data.table of Program System I
# 143    EJAM                        frs 949 MB EPA Facility Registry Service 



# ejanalysis package file get.state.info( setdiff(universe$FIPS.ST, substr( blockgroupstats$bgfips, 1,2) ))[,'ST'] 
# [1] "AS" "GU" "MP" "VI"  
# ejanalysis package file get.state.info( setdiff(universe$FIPS.ST, substr( bgpts$bgfips, 1,2) ))[,'ST']
# [1] "AS" "GU" "MP" "VI"
# setdiff(universe$ST,  stateinfo$ST)
# [1] "AS" "GU" "MP" "UM" "VI"
# setdiff(universe$ST, statestats$REGION)
# [1] "AS" "GU" "MP" "UM" "VI"


#  # ****** block lists and bg list TO BE FIXED / UPDATED  to include 4 island areas**********

 
# 1     bgid2fips BLOCK GROUP id for each BLOCK 
# 2  blockid2fips block id for each block fips c
# 3   blockpoints Decennial Census block group l
# 5      blockwts Decennial Census block weights
# 6 stateinfo2  basic information about US Sta
# 7      quaddata quad tree data on locations of
if (!exists("bgid2fips")) dataload_from_pins("bgid2fips")
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
setdiff(universe$ST,  stateinfo2$ST)
# character(0)
setdiff( stateinfo2$ST, universe$ST)
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

