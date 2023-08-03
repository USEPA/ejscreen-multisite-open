
#  EJAM/data-raw/names_of_indicators_creationscript.R

# *** This was written for the 2022 ver 2.1 EJScreen*** and 
# for 2023 ver 2.2 this needs to be updated or replaced with map_headernames approach.

## see map_headernames which now has most or all of these in a table 
## see   EJAMejscreenapi/data-raw/update_to_ejscreenv2.2.R

# - friendly names differed somewhat
#
# library(EJAMejscreenapi)
# map_headernames[grepl("ej", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# map_headernames[grepl("_d", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# map_headernames[map_headernames$varlist == 'names_e' | grepl("names_e_", map_headernames$varlist), c('varlist', 'newnames_ejscreenapi', 'names_friendly')]
# unique(map_headernames$varlist)
# [1] ""                               "names_d"                        "names_d_avg"                    "names_d_pctile"                 "names_d_state_avg"             
# [6] "names_d_state_pctile"           "names_d_subgroups"              "names_d_subgroups_count"        "names_d_subgroups_pctile"       "names_d_subgroups_state_pctile"
# [11] "names_e"                        "names_e_avg"                    "names_e_pctile"                 "names_e_state_avg"              "names_e_state_pctile"          
# [16] "names_ej"                       "names_ej_pctile"                "names_ej_state_pctile"          "names_ej_supp"                  "names_ej_supp_pctile"          
# [21] "names_ej_supp_state_pctile"     "names_d_median"                 "names_e_median"                 "?"                             
# > 
# for example...
#
# cbind( xlsnames = EJAMejscreenapi::map_headernames['names_ej' == EJAMejscreenapi::map_headernames$varlist, c( 'names_friendly')],  names_ej_friendly )
# xlsnames                              names_ej_friendly     
# [1,] "EJ: PM2.5 (raw)"                     "EJ: PM2.5"           
# [2,] "EJ: Ozone (raw)"                     "EJ: Ozone"           
# [3,] "EJ: Cancer risk (raw)"               "EJ: Cancer risk"     
# [4,] "EJ: Respiratory (raw)"               "EJ: Respiratory"     
# [5,] "EJ: Diesel PM (raw)"                 "EJ: Diesel PM"       
# [6,] "EJ: % built pre-1960 (raw)"          "EJ: % built pre-1960"
# [7,] "EJ: Traffic (raw)"                   "EJ: Traffic"         
# [8,] "EJ: NPL (raw)"                       "EJ: NPL"             
# [9,] "EJ: RMP (raw)"                       "EJ: RMP"             
# [10,] "EJ: TSDF (raw)"                      "EJ: TSDF"            
# [11,] "EJ: NPDES (raw)"                     "EJ: NPDES"           
# [12,] "EJ: Underground storage tanks (raw)" "EJ: UST" 



############################################################################## #
## code to prepare `names_of_indicators` dataset 
# Define lists of names of EJScreen-related variables for use here
############################################################################## #


# FOR NOW I AM putting these all in one list called namez
# AND ALSO AS multiple objects like names_d 
#  But could recode later to use namez$d_friendly instead of names_d_friendly etc.
#  and/or could recode later to use 1 big table, like A MERGED VERSION OF THESE:
# 
#  EJAMejscreenapi::map_headernames which is created from .xlsx using the script in /data-raw folder
#  EJAMbatch.summarizer::varnamesinfo22.rda  which is prob from EJAMbatch.summarizer/inst/map_batch_to_friendly_fieldnames_2022_EJAM.xlsx but needed a creation script in data-raw folder to clarify that
#  EJAM::  ???
#  ejscreen package file ejscreenformulas 

# In the short term, before some mapping file is used to track all variable names, we could simplify a bit by 
# recoding all functions to use namez$d_friendly instead of names_d_friendly, etc., so there is only 1 .rda file and can see all names in 1 command or file,
#  Would need to replace all   names_([a-z|_]*)  with  namez$\1   in all the .R files (functions in EJAM package)
#  and then code to create all the small .rda files via usethis::use_data() in \data-raw\names_of_indicators.R will be obsolete,
#  and we can get rid of all those small data objects in the /EJAM/data/ folder
#    via   something like this:
# file.remove( list.files(path = "./data/", pattern = "names_"))  #  
#
# paste(namesoflistsofnames, collapse = ", ")
#  "names_wts, names_d, names_d_pctile, names_d_state_pctile, names_d_count, names_d_friendly, names_d_avg, names_d_state_avg, names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, names_d_subgroups_avg, names_d_subgroups_state_avg, names_e, names_e_pctile, names_e_state_pctile, names_e_friendly, names_e_avg, names_e_state_avg, names_ej, names_ej_pctile, names_ej_state_pctile, names_ej_friendly, names_other, names_pctile, names_state_pctile, names_need_pctile, names_need_state_pctile"
# names(namez)




# see also:  EJAMejscreenapi/map_headernames.csv/xlsx  
#         or EJAMejscreenapi::map_headernames$newnames_ejscreenapi[ mapheadernames$varlist == "names_d] 

# see also:  EJAM/data-raw/names_of_indicators.R (this file)
# see also:  EJAMbatch.summarizer/data-raw/names_of_variables.R
# see also: ejscreen package file names.e, ejscreenformulas, etc. 

############################################################################## #
# To rename indicator variables ####

#    to change names,   which function to use?
# e.g. this  works but relies on ejscreen package file  pkg, and see EJAMbatch.summarizer::change... and fixnames and fixcolnames and fixnamestype etc. 
   # names(USA_2022)    <- ejscreen package file ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen package file ejscreenformulas$gdbfieldname)] 
   # names(States_2022) <- ejscreen package file ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen package file ejscreenformulas$gdbfieldname)]

# similar to: EJAMbatch.summarizer::names_d_batch_friendly, EJAMbatch.summarizer::names_e_batch_friendly 

############################################################################## #

# POPULATION COUNT (WEIGHTS) ####

# used for weighted means to get stats on avg person nearby any site
# 
names_wts <- "pop"

############################################################################## #

#  DEMOGRAPHICS ####

names_d <- c(
  "Demog.Index",   "Demog.Index.Supp",
  
  "pctlowinc",  
  "pctlingiso",   
  "pctunemployed",
  "pctlths",   # note this sort order was fixed 3/2/23 to put lowinc before min, to match friendly order
   "lowlifex", 
  "pctunder5",  "pctover64", 
 
  "pctmin"
  )

names_d_friendly <- c(
  "Demog.Ind.",   "Suppl Demog Index", 
  
  "% Low-inc.", 
  "% Limited English",
  "% Unemployed",
  "% < High School", 
  "Low life expectancy", 
  "% < age 5", "% > age 64", 
 
  "% People of Color"
)


names_d_count <- gsub('pct', '', names_d); names_d_count <- gsub('min', 'mins', names_d_count); 
names_d_count <- names_d_count[names_d_count != 'Demog.Index']
names_d_count <- names_d_count[names_d_count != 'Demog.Index.Supp']
names_d_count <- names_d_count[names_d_count != 'lowlifex']
names_other <- c("pop","nonmins","povknownratio","age25up", "hhlds","unemployedbase", "pre1960","builtunits")

############################################################################## #

# DEMOG SUBGROUPS ####

names_d_subgroups       <- c("pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", "pctnhwa")
names_d_subgroups_count <- c(   "hisp",    "nhba",    "nhaa",    "nhaiana",    "nhnhpia",    "nhotheralone",    "nhmulti",    "nhwa")
names_d_subgroups_friendly <- c(
  "% Hisp (Hispanic or Latino)", 
  "% Black nha (Black or African American non-Hispanic, single race)", 
  "% Asian nha (Asian non-Hispanic, single race)",
  "% AmIndian nha (American Indian and Alaska Native non-Hispanic, single race)", 
  "% Hawaii nha (Native Hawaiian and Other Pacific Islander non-Hispanic, single race)", 
  "% Other nha (Other race non-Hispanic, single race)",
  "% Multi nha (Two or more races non-Hispanic)",
  "% White nha (White non-Hispanic, single race)" 
)
# names_d_subgroups_friendly <- paste0(   #these were shorter but less explicit? neither is great. and need a shorter version for graphic labels.
#   "% ", c("Hispanic or Latino", "Black or African American", "American Indian and Alaska Native", 
#           "Native Hawaiian and Other Pacific Islander", "Other race", "Two or more races", "White"),
#   " (non-Hispanic, single race)")
# names_d_subgroups_friendly[1] <- "% Hispanic or Latino"
# names_d_subgroups_friendly[6] <- "% Two or more races (non-Hispanic)"

# > EJAMbatch.summarizer::names_d_batch_friendly ==  c(names_d_friendly, names_d_subgroups_friendly)
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

############################################################################## #

# ENVIRONMENTAL  ####

names_e <- c(
  "pm", "o3", "cancer", "resp", "dpm", 
  "pctpre1960", "traffic.score",  "proximity.npl", "proximity.rmp", 
  "proximity.tsdf", "proximity.npdes", "ust",
  "rsei")

names_e_friendly  <- c(
  "PM2.5", "Ozone", "Cancer risk", "Respiratory", "Diesel PM", 
  "% built pre-1960", "Traffic", "NPL proximity", "RMP proximity", 
  "TSDF proximity", "NPDES proximity", "Underground storage tanks",
  "RSEI Air")

############################################################################## #

# EJ INDEXES ####

#####   NEED TO CLARIFY raw vs pctile !    

names_ej <- paste0('EJ.DISPARITY.', names_e, '.eo')
names_ej_friendly <- c( # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  "EJ: PM2.5", 
  "EJ: Ozone", 
  "EJ: Cancer risk", 
  "EJ: Respiratory", 
  "EJ: Diesel PM", 
  "EJ: % built pre-1960", 
  "EJ: Traffic", 
  "EJ: NPL", 
  "EJ: RMP", 
  "EJ: TSDF", 
  "EJ: NPDES", 
  "EJ: UST",
  "EJ: RSEI")

names_ej_supp <- paste0(names_ej, '.supp')
names_ej_supp_friendly <- gsub("EJ", "Supp. EJ", names_ej_friendly)

# not sure I need or will use these, and whether named the same categories in map_headernames:
names_ej_pctile_friendly <- names_ej_friendly
names_ej_state_pctile_friendly <- names_ej_friendly
names_ej_supp_pctile_friendly <- names_ej_friendly
names_ej_supp_state_pctile_friendly <- names_ej_friendly
  
############################################################################## #

# PERCENTILES ####

names_d_pctile             <- paste0(      'pctile.', names_d)
names_d_subgroups_pctile   <- paste0(      'pctile.', names_d_subgroups) # newer
names_e_pctile             <- paste0(      'pctile.', names_e)

names_ej_pctile            <- paste0(      'pctile.', names_ej)
names_ej_supp_pctile       <- paste0(      'pctile.', names_ej_supp) # most recently added

names_d_state_pctile       <- paste0('state.pctile.', names_d)
names_d_subgroups_state_pctile <- 
names_e_state_pctile       <- paste0('state.pctile.', names_e)

names_ej_state_pctile      <- paste0('state.pctile.', names_ej)
names_ej_supp_state_pctile <- paste0('state.pctile.', names_ej_supp) # most recently added

names_pctile <- c( # usa pctiles
  names_d_pctile,
  names_d_subgroups_pctile,
  names_e_pctile,
  names_ej_pctile,
  names_ej_supp_pctile
)
 
names_state_pctile <- c(
  gsub("pctile", "state.pctile", names_pctile)
)

# which base indicators need to be reported as percentiles? ####

names_need_pctile <- gsub("pctile.", "", names_pctile)
names_need_state_pctile <- gsub("state.pctile.", "", names_pctile)

# varsneedpctiles <- c(names_e,  names_d, names_d_subgroups, names_ej)
# varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
# varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)

############################################################################## #

# AVERAGE (AND MEDIAN?) IN US AND STATE ####

names_d_avg <- paste0("avg.", names_d); names_d_state_avg <- paste0("state.avg.", names_d)
names_d_subgroups_avg <- paste0("avg.", names_d_subgroups); names_d_subgroups_state_avg <- paste0("state.avg.", names_d_subgroups)
names_e_avg <- paste0("avg.", names_e); names_e_state_avg <- paste0("state.avg.", names_e)
# names_e_med, names_e_state_med,
# names_d_med, names_d_state_med,

names_d_avg_friendly  <- paste0("US Avg ",  names_d_friendly);  names_d_state_avg_friendly <- paste0("State Avg ", names_d_friendly ) 
names_d_subgroups_avg_friendly  <- paste0("US average ", names_d_subgroups_friendly);  names_d_subgroups_state_avg_friendly <- paste0("State average ", names_d_subgroups_friendly)
names_e_avg_friendly  <- paste0("US Avg ",  names_e_friendly);  names_e_state_avg_friendly <- paste0("State Avg ", names_e_friendly)

############################################################################## #

## see doaggregate() where these are used:
#
# names_these <- c(names_d, names_d_subgroups, names_e)  
# names_avg_these        <- paste0("avg.",       names_these)
# names_state_avg_these  <- paste0("state.avg.", names_these)

# names_ratio_to_avg_these       <-  paste0("ratio.to.", names_avg_these )  
# names_ratio_to_state_avg_these <-  paste0("ratio.to.", names_state_avg_these)  

names_d_ratio_to_avg  <- paste0("ratio.to.", names_d_avg) 
names_d_ratio_to_state_avg <- paste0("ratio.to.", names_d_state_avg) 

names_d_subgroups_ratio_to_avg <- paste0("ratio.to.", names_d_subgroups_avg)  
names_d_subgroups_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_state_avg)

names_e_ratio_to_avg  <- paste0("ratio.to.", names_e_avg)  
names_e_ratio_to_state_avg <- paste0("ratio.to.", names_e_state_avg)

names_d_ratio_to_avg_friendly <- paste0("Ratio to ", names_d_avg_friendly)
names_d_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_state_avg_friendly) 

names_d_subgroups_ratio_to_avg_friendly <- paste0("Ratio to ", names_d_subgroups_avg_friendly)
names_d_subgroups_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_subgroups_state_avg_friendly)

names_e_ratio_to_avg_friendly <- paste0("Ratio to ", names_e_avg_friendly)
names_e_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_e_state_avg_friendly)


names_these              <- c(names_d,     names_d_subgroups,     names_e)
names_avg_these          <- c(names_d_avg, names_d_subgroups_avg, names_e_avg)                         # <- paste0("avg.",       names_these) #
names_state_avg_these    <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
names_ratio_to_avg_these <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_avg_these )
names_ratio_to_state_avg_these <- c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg, names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_state_avg_these)


############################################################################## #

# * names_d_fixed and long_names_d were temporarily used before VSI.eo changed to Demog.Index ####
# BUT THESE ARE NOW OBSOLETE - JUST USE DIRECTLY SOMETHING LIKE  c(names_d,              names_d_subgroups)

# data.frames long_names_e and long_names_d were used  in left_join for renaming in server code
# long_names_e <- data.frame(vars=names_e, var_names= names_e_friendly, stringsAsFactors = FALSE) # just for convenient way to use left_join to rename for plot labels

# names_d_fixed              <- c(names_d,              names_d_subgroups)
# names_d_pctile_fixed       <- c(names_d_pctile,       names_d_subgroups_pctile)
# names_d_state_pctile_fixed <- c(names_d_state_pctile, names_d_subgroups_state_pctile)

# data.frames long_names_e and long_names_d are used  in left_join for renaming in server code
# long_names_d <- data.frame(
#   vars=      c(names_d,          names_d_subgroups),       #  names_d_fixed, 
#   var_names= c(names_d_friendly, names_d_subgroups_friendly), 
#   stringsAsFactors = FALSE) # 

# EJAMejscreenapi::map_headernames$names_friendly[match(names_d_fixed, EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]

############################################################################## #

# **namez__ ####

# try putting these all in one list instead of multiple objects? could recode later to use namez$d_friendly instead of names_d_friendly etc.
# and/or    just store them in a big table

namesoflistsofnames = c(
  'names_wts',
  
  'names_d', 
  'names_d_friendly', # not for percentiles and counts?
  'names_d_avg',          'names_d_state_avg', 
  'names_d_avg_friendly', 'names_d_state_avg_friendly' ,
  'names_d_pctile',       'names_d_state_pctile',           
  # no friendly pctile names here
  'names_d_ratio_to_avg',          'names_d_ratio_to_state_avg',
  'names_d_ratio_to_avg_friendly', 'names_d_ratio_to_state_avg_friendly', 
  # names_d_med,  names_d_state_med,
  'names_d_count',  
  
  'names_d_subgroups',         
  'names_d_subgroups_friendly',
  'names_d_subgroups_avg',         'names_d_subgroups_state_avg',
  'names_d_subgroups_avg_friendly','names_d_subgroups_state_avg_friendly',
  'names_d_subgroups_pctile',      'names_d_subgroups_state_pctile', 
  # no friendly pctile names here
  'names_d_subgroups_ratio_to_avg',          'names_d_subgroups_ratio_to_state_avg',
  'names_d_subgroups_ratio_to_avg_friendly', 'names_d_subgroups_ratio_to_state_avg_friendly',
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  'names_d_subgroups_count', 
  
  'names_e',           
  'names_e_friendly',
  'names_e_avg',          'names_e_state_avg', 
  'names_e_avg_friendly', 'names_e_state_avg_friendly'  ,
  'names_e_pctile',       'names_e_state_pctile', 
  # no friendly pctile names here
  'names_e_ratio_to_avg',          'names_e_ratio_to_state_avg',
  'names_e_ratio_to_avg_friendly', 'names_e_ratio_to_state_avg_friendly',
  # names_e_med, names_e_state_med,  
  
  'names_ej',   # RAW SCORE
  'names_ej_friendly',  # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  'names_ej_pctile',          'names_ej_state_pctile' ,
  # no friendly pctile names here
  
  'names_other', # includes pop and other denominator counts
  
  'names_pctile',  # all US pctile indicators
  'names_state_pctile',
  'names_need_pctile',  # base indicators that need to be reported as US percentiles not just raw scores
  'names_need_state_pctile', 
  
  'names_these',
  'names_avg_these',
  'names_state_avg_these',
  'names_ratio_to_avg_these',
  'names_ratio_to_state_avg_these'
)

namez <- lapply(namesoflistsofnames, get)
names(namez) <- gsub("^names_","", namesoflistsofnames)

# **names_all__ ####
# NOTE THIS IS VERY DIFFERENT THAN names_all_batch !!
names_all <- as.vector(unlist(namez))
names_all <- unique(names_all) # pop would appear twice

############################################################################## #
#   USE_DATA ####

usethis::use_data(namez, overwrite = TRUE)


############################################################################## #

# AND ALSO STORE EACH LITTLE OBJECT ? ####

usethis::use_data(
  
  names_all,
  names_wts, 
  
  names_d, 
  names_d_friendly, # not for percentiles and counts?
  names_d_avg,          names_d_state_avg, 
  names_d_avg_friendly, names_d_state_avg_friendly ,
  names_d_pctile,       names_d_state_pctile,           
  # no friendly pctile names here
  names_d_ratio_to_avg,          names_d_ratio_to_state_avg,
  names_d_ratio_to_avg_friendly, names_d_ratio_to_state_avg_friendly, 
  # names_d_med,  names_d_state_med,
  names_d_count,

  names_d_subgroups,         
  names_d_subgroups_friendly,
  names_d_subgroups_avg,         names_d_subgroups_state_avg,
  names_d_subgroups_avg_friendly,names_d_subgroups_state_avg_friendly,
  names_d_subgroups_pctile,      names_d_subgroups_state_pctile, 
  # no friendly pctile names here
  names_d_subgroups_ratio_to_avg,          names_d_subgroups_ratio_to_state_avg,
  names_d_subgroups_ratio_to_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  names_d_subgroups_count, 
  
  names_e,           
  names_e_friendly,
  names_e_avg,          names_e_state_avg, 
  names_e_avg_friendly, names_e_state_avg_friendly  ,
  names_e_pctile,       names_e_state_pctile, 
  # no friendly pctile names here
  names_e_ratio_to_avg,          names_e_ratio_to_state_avg,
  names_e_ratio_to_avg_friendly, names_e_ratio_to_state_avg_friendly,
  # names_e_med, names_e_state_med,  
 
  names_ej,   
  names_ej_friendly,
  names_ej_pctile,          names_ej_state_pctile ,
  
  # no friendly pctile names here
  
  names_other, # includes pop and other denominator counts

  names_pctile,  # all US pctile indicators
  names_state_pctile,
  names_need_pctile,  # base indicators that need to be reported as US percentiles not just raw scores
  names_need_state_pctile,

  
  names_these,
  names_avg_these,
  names_state_avg_these,
  names_ratio_to_avg_these,
  names_ratio_to_state_avg_these,
  
  overwrite = TRUE
)

# data.frames mapping short to friendly/long names - obsolete
# usethis::use_data(
#   long_names_d,
#   long_names_e
# )
############################################################################## #

# (first make sure all the Envt variables etc are actually in usastates dataset)

stop('make sure all the Demog and Envt variables etc are actually in the latest installed usastates dataset')

# just for conveniently referring to these in server code
avg.in.us <- EJAM::usastats[ EJAM::usastats$PCTILE == "mean", names_these] # note the regular name not avg. name is used in the usastats table
usethis::use_data(avg.in.us, overwrite = TRUE)

