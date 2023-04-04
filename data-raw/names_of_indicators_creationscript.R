

#    EJAM


# C:/Users/mcorrale/R/mysource/EJAM/data-raw/names_of_indicators.R









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
#  ejscreen::ejscreenformulas 

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
# see also: ejscreen::names.e, ejscreenformulas, etc. 

############################################################################## #
# To rename indicator variables ####

#    to change names,   which function to use?
# e.g. this  works but relies on ejscreen:: pkg, and see EJAMbatch.summarizer::change... and fixnames and fixcolnames and fixnamestype etc. 
   # names(USA_2022)    <- ejscreen::ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen::ejscreenformulas$gdbfieldname)] 
   # names(States_2022) <- ejscreen::ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen::ejscreenformulas$gdbfieldname)]

# similar to: EJAMbatch.summarizer::names_d_friendly, EJAMbatch.summarizer::names_e_friendly 

############################################################################## #

# POPULATION COUNT (WEIGHTS) ####

# used for weighted means to get stats on avg person nearby any site
# 
names_wts <- "pop"

############################################################################## #


#  DEMOGRAPHICS ####

names_d <- c(
  "Demog.Index", 
  "Demog.Index.Supp",
  
  "pctlowinc",  "pctmin",    "pctlths",   # note this sort order was fixed 3/2/23 to put lowinc before min, to match friendly order
  "pctlingiso", "pctunder5",  "pctover64", "pctunemployed",
  "lowlifex"
  )

names_d_friendly <- c(
  "Demog.Ind.", 
  
  "Suppl Demog Index", 
  
  "% Low-inc.", "% People of Color", "% <High School", 
  "% Linguistic Isol.", "% < age 5", "% > age 64", "% unemployed",
  "Low life expectancy"
)
# EJAMbatch.summarizer::names_d_friendly

names_d_count <- gsub('pct', '', names_d); names_d_count <- gsub('min', 'mins', names_d_count); 
names_d_count <- names_d_count[names_d_count != 'Demog.Index']
names_d_count <- names_d_count[names_d_count != 'Demog.Index.Supp']
names_d_count <- names_d_count[names_d_count != 'lowlifex']
names_other <- c("pop","nonmins","povknownratio","age25up", "hhlds","unemployedbase", "pre1960","builtunits")

############################################################################## #

# DEMOG SUBGROUPS ####

names_d_subgroups       <- c("pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti")
names_d_subgroups_count <- c(   "nhwa",    "hisp",    "nhba",    "nhaa",    "nhaiana",    "nhnhpia",    "nhotheralone",    "nhmulti")
names_d_subgroups_friendly <- c(
  "% White (non-Hispanic, single race)", 
  "% Hispanic or Latino", 
  "% Black or African American (non-Hispanic, single race)", 
  "% Asian (non-Hispanic, single race)",
  "% American Indian and Alaska Native (non-Hispanic, single race)", 
  "% Native Hawaiian and Other Pacific Islander (non-Hispanic, single race)", 
  "% Other race (non-Hispanic, single race)",
  "% Two or more races (non-Hispanic)"
)
# names_d_subgroups_friendly <- paste0(
#   "% ", c("White", "Hispanic or Latino", "Black or African American", "American Indian and Alaska Native", 
#           "Native Hawaiian and Other Pacific Islander", "Other race", "Two or more races"),
#   " (non-Hispanic, single race)")
# names_d_subgroups_friendly[2] <- "% Hispanic or Latino"
# names_d_subgroups_friendly[7] <- "% Two or more races (non-Hispanic)"

############################################################################## #

# ENVIRONMENTAL  ####

names_e <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score",  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
names_e_friendly  <- c(
  "PM2.5", "Ozone", "Cancer risk", "Respiratory", "Diesel PM", 
  "% built pre-1960", "Traffic", "NPL proximity", "RMP proximity", 
  "TSDF proximity", "NPDES proximity", "Underground storage tanks")


############################################################################## #

# AVERAGE AND MEDIAN IN US AND STATE ####

names_e_avg <- paste0("avg.", names_e); names_e_state_avg <- paste0("state.avg.", names_e)
names_d_avg <- paste0("avg.", names_d); names_d_state_avg <- paste0("state.avg.", names_d)
names_d_subgroups_avg <- paste0("avg.", names_d_subgroups); names_d_subgroups_state_avg <- paste0("state.avg.", names_d_subgroups)
# names_e_med, names_e_state_med,
# names_d_med, names_d_state_med,

############################################################################## #

# EJ INDEXES ####

names_ej <- paste0('EJ.DISPARITY.', names_e, '.eo')
names_ej_friendly <- c(
  "EJ: PM2.5", 
  "EJ: Ozone", 
  "EJ: Cancer risk", 
  "EJ: Respiratory", 
  "EJ: Diesel PM", 
  "EJ: % built pre-1960", 
  "EJ: Traffic", 
  "EJ: NPL proximity", 
  "EJ: RMP proximity", 
  "EJ: TSDF proximity", 
  "EJ: NPDES proximity", 
  "EJ: Underground storage tanks")

############################################################################## #

# PERCENTILES ####

names_d_pctile           <- paste0(      'pctile.', names_d)
names_e_pctile           <- paste0(      'pctile.', names_e)
names_ej_pctile          <- paste0(      'pctile.', names_ej)
names_d_subgroups_pctile <- paste0(      'pctile.', names_d_subgroups) # newer

names_d_state_pctile     <- paste0('state.pctile.', names_d)
names_e_state_pctile     <- paste0('state.pctile.', names_e)
names_ej_state_pctile    <- paste0('state.pctile.', names_ej)
names_d_subgroups_state_pctile <- 
                            paste0('state.pctile.', names_d_subgroups) # newer

names_pctile <- c(
  names_d_pctile,
  names_e_pctile,
  names_ej_pctile,
  names_d_subgroups_pctile
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
# and/or
# just store them in a big table

namesoflistsofnames = c(
  'names_wts',
  
  'names_d',           'names_d_pctile',           'names_d_state_pctile',           'names_d_count',  
  'names_d_friendly', # not for percentiles and counts?
  'names_d_avg', 'names_d_state_avg', 
  # names_d_med,  names_d_state_med,
  
  'names_d_subgroups', 'names_d_subgroups_pctile', 'names_d_subgroups_state_pctile', 'names_d_subgroups_count', 
  'names_d_subgroups_friendly',
  'names_d_subgroups_avg', 'names_d_subgroups_state_avg',
  # names_d_subgroups_med,  names_d_subgroups_state_med,
  
  'names_e',           'names_e_pctile',           'names_e_state_pctile', 
  'names_e_friendly',
  'names_e_avg', 'names_e_state_avg',
  # names_e_med, names_e_state_med,
  
  'names_ej',          'names_ej_pctile',          'names_ej_state_pctile' ,
  'names_ej_friendly',
  
  'names_other', # includes pop and other denominator counts
  
  'names_pctile',  # all US pctile indicators
  'names_state_pctile',
  'names_need_pctile',  # base indicators that need to be reported as US percentiles not just raw scores
  'names_need_state_pctile'
)

namez = lapply(namesoflistsofnames, get)
names(namez) <- gsub("^names_","", namesoflistsofnames)


# **names_all__ ####

names_all <- as.vector(unlist(namez))
names_all <- unique(names_all) # pop would appear twice

############################################################################## #
 

############################################################################## #

#   USE_DATA ####

usethis::use_data(namez, overwrite = TRUE)

# AND ALSO STORE EACH LITTLE OBJECT ?? ####

usethis::use_data(
  names_all,
  names_wts, names_d, names_d_pctile, names_d_state_pctile, names_d_count, names_d_friendly, names_d_avg, names_d_state_avg, 
  names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, names_d_subgroups_avg, names_d_subgroups_state_avg, 
  names_e, names_e_pctile, names_e_state_pctile, names_e_friendly, names_e_avg, names_e_state_avg,
  names_ej, names_ej_pctile, names_ej_state_pctile, names_ej_friendly, 
  names_other, names_pctile, names_state_pctile, names_need_pctile, names_need_state_pctile,  
  overwrite = TRUE
)

# data.frames mapping short to friendly/long names - obsolete
# usethis::use_data(
#   long_names_d,
#   long_names_e
# )


