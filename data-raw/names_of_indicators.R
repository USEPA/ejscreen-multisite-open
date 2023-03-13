## code to prepare `names_of_indicators` dataset goes here


# Define lists of names of EJScreen-related variables for use here

# see also:  EJAMejscreenapi/map_headernames.csv or EJAMejscreenapi::map_headernames$newnames_ejscreenapi
# see also:  EJAM/data-raw/names_of_indicators.R
# see also:  EJAMbatch.summarizer/data-raw/names_of_variables.R
# see also: ejscreen::names.e, ejscreenformulas, etc. 
# rename indicator variables ####
#    to change names,   which function to use?
# e.g. this  works but relies on ejscreen:: pkg, and see EJAMbatch.summarizer::change... and fixnames and fixcolnames and fixnamestype etc. 
   # names(USA_2022)    <- ejscreen::ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen::ejscreenformulas$gdbfieldname)] 
   # names(States_2022) <- ejscreen::ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen::ejscreenformulas$gdbfieldname)]


# names_e_avg, names_d_avg, names_e_state_avg, names_d_state_avg  
names_e_avg <- paste0("avg.", names_e); names_e_state_avg <- paste0("state.", names_e_avg)
names_d_avg <- paste0("avg.", names_d); names_d_state_avg <- paste0("state.", names_d_avg)

# similar to: EJAMbatch.summarizer::names_d_friendly, EJAMbatch.summarizer::names_e_friendly 

names_e <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", "traffic.score",  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", "ust")
names_e_friendly  <- c(
  "PM2.5", "Ozone", "Cancer risk", "Respiratory", "Diesel PM", 
  "% built pre-1960", "Traffic", "NPL proximity", "RMP proximity", 
  "TSDF proximity", "NPDES proximity", "Underground storage tanks")

#  Demog.Index
names_d <- c(
  "VSI.eo",     "pctlowinc",  "pctmin",    "pctlths",   # note this sort order was fixed 3/2/23 to put lowinc before min, to match friendly order
  "pctlingiso", "pctunder5",  "pctover64", "pctunemployed")
names_d_friendly <- c(
  "Demog.Ind.", "% Low-inc.", "% Minority", "% <High School", 
  "% Linguistic Isol.", "% < age 5", "% > age 64", "% unemployed"
) # EJAMbatch.summarizer::names_d_friendly
names_d_count <- gsub('pct', '', names_d); names_d_count <- gsub('min', 'mins', names_d_count); names_d_count <- names_d_count[names_d_count != 'VSI.eo']
names_other <- c("pop","nonmins","povknownratio","age25up", "hhlds","unemployedbase", "pre1960","builtunits")

names_d_subgroups       <- c("pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti")
names_d_subgroups_count <- c(   "nhwa",    "hisp",    "nhba",    "nhaa",    "nhaiana",    "nhnhpia",    "nhotheralone",    "nhmulti")
names_d_subgroups_friendly <- c(
  "% White (non-Hispanic, single race)", 
  "% Hispanic or Latino", 
  "% Black or African American (non-Hispanic, single race)", 
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

names_d_pctile           <- paste0(      'pctile.', names_d)
names_e_pctile           <- paste0(      'pctile.', names_e)
names_ej_pctile          <- paste0(      'pctile.', names_ej)
names_d_subgroups_pctile <- paste0(      'pctile.', names_d_subgroups) # newer

names_d_state_pctile     <- paste0('state.pctile.', names_d)
names_e_state_pctile     <- paste0('state.pctile.', names_e)
names_ej_state_pctile    <- paste0('state.pctile.', names_ej)
names_d_subgroups_state_pctile <- 
                            paste0('state.pctile.', names_d_subgroups) # newer

names_all <- c(
  names_other, # includes pop and other denominator counts
  names_d,           names_d_pctile,           names_d_state_pctile,           names_d_count, 
  names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, 
  names_e,           names_e_pctile,           names_e_state_pctile, 
  names_ej,          names_ej_pctile,          names_ej_state_pctile 
)

usethis::use_data(
  names_d_subgroups_friendly, names_d_friendly, 
  names_e_friendly, names_ej_friendly, 
  
  names_e_avg, names_e_state_avg, 
  names_d_avg, names_d_state_avg,
  
  names_other, # includes pop and other denominator counts
  
  names_d,           names_d_pctile,           names_d_state_pctile,           names_d_count, 
  names_d_subgroups, names_d_subgroups_pctile, names_d_subgroups_state_pctile, names_d_subgroups_count, 
  names_e,           names_e_pctile,           names_e_state_pctile, 
  names_ej,          names_ej_pctile,          names_ej_state_pctile,
  names_all,
  overwrite = TRUE
) 

