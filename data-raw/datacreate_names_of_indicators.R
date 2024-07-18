
#  EJAM/data-raw/datacreate_names_of_indicators.R

########################################################################################################## #
# ***
#   to be reconciled with or replaced with map_headernames approach...
##    map_headernames$varlist  which now has most or all of these in a table 
## but subgroups info is tricky since $varlist was "names_d_subgroups_nh" not "names_d_subgroups" 

########################################################################################################## #
########################################################################################################## #
if ( 1 == 0 ) {
  #  friendly names differ -  script to compare them: 
  
  vvv <- function(vlist = "names_r_all") {
    vars =  get(vlist) # same as names_d
    fr = paste0(vlist, "_friendly")
    if (exists(fr)) {
      vars_f = get(fr) # same as  names_d_friendly  
    } else {
      vars_f = rep(NA, length(vlist))
    }
    # print(
    data.frame(
      vlist = vlist,
      names_x = vars,
      maphead.names_friendly = fixcolnames(vars, 'r', 'names_friendly'),
      shortlabel             = fixcolnames(vars, 'r', 'shortlabel'),
      long                   = fixcolnames(vars, 'r', 'long'),
      names_x_friendly = vars_f
    )
    # )
  }
  lists_to_check <- paste0("names_", grep("friendly|these|all_r|wts|subgroups_alone|subgroups_nh", invert = T, names(namez), value = T))
  # vvv("names_d")
  # vvv(lists_to_check[1])
  out <- list()
  for (i in 1:length(lists_to_check)) {
    out[[i]] <- vvv(lists_to_check[i])
  }
  out <- data.table::rbindlist(out)
  # print(out)
  
  # confirmed that 2 are identical but others differ:
  ## shortlabel == out$maphead.names_friendly 
  # versus  long,  
  # versus  names_x_friendly 
  
  # the same:
  # table(out$shortlabel == out$maphead.names_friendly, useNA = "always")  # 458 TRUE
  
  # not matching:
  # table(out$shortlabel == out$long,               useNA = "always")  # F (but True 69 times)
  # table(out$shortlabel == out$names_x_friendly,   useNA = "always")  # F (or na 122 times)
  # table(out$long       == out$names_x_friendly,   useNA = "always")  #  F (or na 122 times, or 9 times true)
  
  # which are shortest?
  # table(nchar(out$shortlabel) > nchar(out$long), useNA = 'always') # long is almost always longer
  # table(nchar(out$shortlabel) > nchar(out$names_x_friendly), useNA = 'always') # in 63 indicators, names_x_friendly is shorter/better,
  # so we should id those and use the shorter name, from names_x_friendly for those 63 or so.
  # which lists of names are those 63 in?
  # out[nchar(out$shortlabel) > nchar(out$names_x_friendly), 
  #     .( vlist, names_x, shortlabel, names_x_friendly) ] 
  # See which lists of names for which we should replace shortlabel with the shorter one that is in 
  # actually, just do this:
  # for raw EJ scores: fix names_ej rows, $shortlabel column, to say "(raw)" at the end not "EJ Index", and prefix with "EJ: "
  # life expectancy: OK as is. shortlabel is good enough. 
  # "Toxic Releases to Air" is long but ok, for names_ej_pctile and related, and names_ej etc.
  # "Air toxics " is long but ok.
  
  # bottom line is we can leave map_headernames alone and use shortlabel column for plots, etc.
  
  # should we make identical the names_x_friendly and the maphead.names_friendly??
  # when do they disagree? 
  out[maphead.names_friendly != names_x_friendly , .(vlist, names_x_friendly, maphead.names_friendly)]
  out[maphead.names_friendly != names_x_friendly , .(vlist, names_x_friendly, maphead.names_friendly )][1:100, ]
  
  # i think maphead.names_friendly in xlsx should be replaced to be identical to names_x_friendly and get those 
  # from long, since map_headernames$longname  is the longest and most accurate full description.
  # So just keep the very short name in shortlabel and the very long one in longnames_tableheader,
  # but change maphead.names_friendly, names_x_friendly by 
  #copying the map_headernames$longname column and 
  #pasting it onto (replacing) map_headernames$names_friendly column
  # AND manually replace in EJAM/data-raw/datacreate_names_of_indicators.R,
  #  all the names_xyz_friendly lists with what is in map_headernames$longname somehow.
  
  # see out df above  to see the careful names from this file and namez etc.
  ## and maybe put them into mapheadernames longnames and namesfriendly columns where better?
  
}
########################################################################################################## #
########################################################################################################## #

datacreate_names_of_indicators <- function() {

  ### this will create but also assign metadata to and save for pkg via use_data()
  ### unlike other datacreate_  functions that do not do the metadata and use_data steps!
  ### It is really kind of a script, but packaged as a function here so that
  ### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
    
# ** change when ready to switch to using subgroups_alone like white alone not nonhispanic white alone:

subgroups_type <- c(  'nh', 'alone')[1]  # simple names without _nh or _alone should be usable/ what is used in code, and here get set same as nh for now and set to same as alone when want to switch 

#  *** EJScreen refers to EJ indexes without mentioning they are percentiles, since that is the only way they are reported.
# Should EJAM do same, or be more explicit with friendly names? 
# names_ej_friendly can be used instead for pctiles, probably, since it does not mention whether it is raw or percentile

# library(EJAMejscreenapi)
# map_headernames[grepl("ej", map_headernames$varlist), c('varlist', 'rname', 'names_friendly')]
# map_headernames[grepl("_d", map_headernames$varlist), c('varlist', 'rname', 'names_friendly')]
# map_headernames[map_headernames$varlist == 'names_e' | grepl("names_e_", map_headernames$varlist), c('varlist', 'rname', 'names_friendly')]
# unique(map_headernames$varlist)


# for example...
#
# cbind( xlsnames = map_headernames['names_ej' == map_headernames$varlist, c( 'names_friendly')],  names_ej_friendly )
# xlsnames                              names_ej_friendly     
# [1 ,] "EJ: PM2.5 (raw)"                     "EJ: PM2.5"           
# [2, ] "EJ: Ozone (raw)"                     "EJ: Ozone"           
# [3, ] "EJ: Cancer risk (raw)"               "EJ: Cancer risk"     
# [4, ] "EJ: Respiratory (raw)"               "EJ: Respiratory"     
# [5, ] "EJ: Diesel PM (raw)"                 "EJ: Diesel PM"       
# [6, ] "EJ: % built pre-1960 (raw)"          "EJ: % built pre-1960"
# [7, ] "EJ: Traffic (raw)"                   "EJ: Traffic"         
# [8, ] "EJ: NPL (raw)"                       "EJ: NPL"             
# [9, ] "EJ: RMP (raw)"                       "EJ: RMP"             
# [10,] "EJ: TSDF (raw)"                      "EJ: TSDF"            
# [11,] "EJ: NPDES (raw)"                     "EJ: NPDES"           
# [12,] "EJ: Underground storage tanks (raw)" "EJ: UST" 
#      RSEI  


############################################################################## #
## code to prepare `names_of_indicators` dataset 
# Define lists of names of EJScreen-related variables for use here
############################################################################## #

# FOR NOW I AM putting these all in one list called namez, 
#  and also lists in the varlist column of  map_headernames
# AND ALSO AS multiple objects like names_d 
#
#  But could recode later to use namez$d_friendly instead of names_d_friendly etc.
#  and/or could recode later to use 1 big table, like A MERGED VERSION OF THESE:
# 
#  map_headernames which is created from .xlsx using the script in /data-raw folder
#  namez 
#  formulas_all as possible approach using calc_ejam()
#  and older file  EJAMbatch.summarizer  ::  varnamesinfo22.rda  which was prob from EJAMbatch.summarizer/inst/map_batch_to_friendly_fieldnames_2022_EJAM.xlsx

# In the short term, before some mapping file is used to track all variable names, we could simplify a bit by 
# recoding all functions to use namez$d_friendly instead of names_d_friendly, etc., so there is only 1 .rda file and can see all names in 1 command or file,
#  Would need to replace all   names_([a-z|_]*)  with  namez$\1   in all the .R files (functions in EJAM package)
#  and then code to create all the small .rda files via usethis::use_data() in \data-raw\names_of_indicators.R will be obsolete,
#  and we can get rid of all those small data objects in the /EJAM/data/ folder
#    via   something like this:
# file.remove( list.files(path = "./data/", pattern = "names_"))  #  
#
# paste(namesoflistsofnames, collapse = ", ")
#  "names_wts, names_d, names_d_pctile,etc

# names(namez)




# see also:   
#          map_headernames$rname[ mapheadernames$varlist == "names_d] 

# see also:  EJAM/data-raw/names_of_indicators.R (this file)


############################################################################## #
# To rename indicator variables ####

#    to change names,   which function to use?
# e.g. this  works but relies on ejscreen package file  pkg, and see older  EJAMbatch.summarizer function change... and fixnames and fixcolnames and fixnamestype etc. 
# names(USA_2022)    <- ejscreen package file ejscreenformulas$Rfieldname[match(names(USA_2022),    ejscreen package file ejscreenformulas$gdbfieldname)] 
# names(States_2022) <- ejscreen package file ejscreenformulas$Rfieldname[match(names(States_2022), ejscreen package file ejscreenformulas$gdbfieldname)]

# similar to: EJAMbatch.summarizer pkg objects names_d_batch_friendly, names_e_batch_friendly 

############################################################################## #

# POPULATION COUNT (WEIGHTS) ####

# used for weighted means to get stats on avg person nearby any site
# 
names_wts <- "pop"

############################################################################## #

#  DEMOGRAPHICS - rawpct, friendlypct, and count; and other counts like denominators (not pctile) ####

# raw percents
names_d <- map_headernames$rname[map_headernames$varlist == "names_d"]

# friendly raw percents
# > dput(fixcolnames(names_d, 'r', 'long'))
names_d_friendly <- fixcolnames(names_d, 'r', 'long')

# no friendly version of counts?  e.g., 
# dput(fixcolnames(names_d_count, 'r', 'long'))
names_d_count_friendly <- fixcolnames(names_d_count, 'r', 'long')

# names_d_count_friendly <- paste0("Count of ", gsub("% ", "", names_d_friendly))
# all.equal(names_d_count_friendly, map_headernames$longname[map_headernames$varlist == "names_d_count"])
# [1] TRUE
# cbind(names_d_count_friendly, paste0("Count of ", gsub("% ", "", names_d_count_friendly)))
# >  cbind(names_d_count_friendly, paste0("Count of ", gsub("% ", "", names_d_count_friendly)))
# names_d_count_friendly                                                                                    
# [1,] "Low income resident count"                      "Count of Low income resident count"                     
# [2,] "Limited English-speaking Households"            "Count of Limited English-speaking Households"           
# [3,] "Unemployed resident count"                      "Count of Unemployed resident count"                     
# [4,] "Less Than High School Education resident count" "Count of Less Than High School Education resident count"
# [5,] "Under Age 5 resident count"                     "Count of Under Age 5 resident count"                    
# [6,] "Over Age 64 resident count"                     "Count of Over Age 64 resident count"                    
# [7,] "People of Color resident count"                 "Count of People of Color resident count"   
# 

# counts with exceptions, and other counts
names_d_count <- gsub('pct', '', names_d)
names_d_count <- gsub('min', 'mins', names_d_count)
dontuse = names_d_count %in% c('Demog.Index',  'Demog.Index.Supp',  "Demog.Index.State" ,     "Demog.Index.Supp.State" ,'lowlifex') # there is no count for these
names_d_count <- names_d_count[!dontuse]
# names_d_count_friendly <- names_d_count_friendly[!dontuse]
rm(dontuse)
all.equal(names_d_count, map_headernames$rname[map_headernames$varlist == "names_d_count"])

# names_d_other_count <- c("pop", "nonmins", "povknownratio", "age25up", "hhlds", "unemployedbase", "pre1960", "builtunits")
names_d_other_count <- map_headernames$rname[map_headernames$varlist == 'names_d_other_count']
# c("pop", "nonmins", "age25up", "hhlds", "unemployedbase", "pre1960", "builtunits", "povknownratio")
 
names_d_other_count_friendly <- fixcolnames(names_d_other_count, 'r', 'long')
#   c('Population', 
#                                   'Count of non-POC', 
#                                   'Count of age 25+ (denominator for %<high school)',
#                                   'Count of households (denominator for %limited English)',
#                                   'Count of denominator for %unemployed', 
#                                   'Count of housing units built pre-1960',
#                                   'Count of housing units (denominator for %pre-1960)',
#                                   'Count of hhlds with known poverty ratio (denominator for % low income)'
# )
# this was not being used.

# these are not quite the same
# cbind( fixcolnames(names_d_other_count, 'r', 'long'),
#        names_d_other_count_friendly )

############################################################################## #

# DEMOG SUBGROUPS - rawpct, friendlypct, & count (not pctile) ####
# 
#  v2.2 and 2.3  EJAM and EJAMejscreenapi   use 
# names_d_subgroups_nh    for non-hispanic versions 
# names_d_subgroups_alone for black alone, white alone, etc. - not used

# subgroups_type

# raw percents

names_d_subgroups_nh    <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh"]
names_d_subgroups_alone <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone"]
all.equal(names_d_subgroups_nh, 
          c("pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", "pctnhwa"))
all.equal(names_d_subgroups_alone,
          c("pcthisp",  "pctba",   "pctaa",   "pctaiana",   "pctnhpia",   "pctotheralone",   "pctmulti",   "pctwa"))

# counts
names_d_subgroups_nh_count    <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_count"]
names_d_subgroups_alone_count <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_count"]
# or  names_d_subgroups_count <-  gsub("pct", "", names_d_subgroups)
all.equal(names_d_subgroups_nh_count, 
          c("hisp", "nhba",  "nhaa",   "nhaiana",  "nhnhpia",    "nhotheralone",    "nhmulti",    "nhwa"))
all.equal(names_d_subgroups_alone_count,
          c("hisp",   "ba",    "aa",     "aiana",    "nhpia",      "otheralone",      "multi",      "wa")
          )

# friendly raw percents

# OLD version of NAMES
# names_d_subgroups_alone_friendly <- c(
#   "% Hispanic or Latino (any race)", 
#   "% Black or African American, single race", 
#   "% Asian, single race",
#   "% American Indian or Alaska Native, single race", 
#   "% Native Hawaiian or Other Pacific Islander, single race", 
#   "% Other race, single race",
#   "% Multirace (two or more races)",
#   "% White (single race, includes White Hispanic)"
# )
### use fixcolnames (map_headernames) now just to be consistent:
names_d_subgroups_alone_friendly <- fixcolnames(names_d_subgroups_alone, 'r', 'long')
## old vs new naming
# [1,] "% Hispanic or Latino"                                                          "% Hispanic or Latino (any race)"
# [2,] "% Black or African American (single race, includes Hispanic)"                  "% Black or African American, single race"
# [3,] "% Asian (single race, includes Hispanic)"                                      "% Asian, single race"
# [4,] "% American Indian and Alaska Native (single race, includes Hispanic)"          "% American Indian or Alaska Native, single race"
# [5,] "% Native Hawaiian and Other Pacific Islander (single race, includes Hispanic)" "% Native Hawaiian or Other Pacific Islander, single race"
# [6,] "% Other race (single race, includes Hispanic)"                                 "% Other race, single race"
# [7,] "% Two or more races (includes Hispanic)"                                       "% Multirace (two or more races)"
# [8,] "% White (single race, includes Hispanic)"                                      "% White (single race, includes White Hispanic)"   

# new way to be consistent with maphead
names_d_subgroups_nh_friendly <- dput(fixcolnames(names_d_subgroups_nh, 'r', 'long'))
# names_d_subgroups_nh_friendly <- c(   # , non-Hispanic
#   "% Hispanic or Latino (any race)", 
#   "% Black or African American, single race, non-Hispanic", 
#   "% Asian, single race, non-Hispanic",
#   "% American Indian or Alaska Native, single race, non-Hispanic", 
#   "% Native Hawaiian or Other Pacific Islander, single race, non-Hispanic", 
#   "% Other race, single race, non-Hispanic",
#   "% Multirace (two or more races), non-Hispanic",
#   "% White, single race, non-Hispanic"
# )

# Basic subgroup lists

if ('alone' %in% subgroups_type) {
  names_d_subgroups        <-   names_d_subgroups_alone  
  names_d_subgroups_count    <- names_d_subgroups_alone_count 
  names_d_subgroups_friendly <- names_d_subgroups_alone_friendly
} else {
  names_d_subgroups        <-   names_d_subgroups_nh
  names_d_subgroups_count    <- names_d_subgroups_nh_count
  names_d_subgroups_friendly <- names_d_subgroups_nh_friendly
}

#   friendly version of counts 

names_d_subgroups_count_friendly       <-  fixcolnames(names_d_subgroups_count, 'r', 'long') 
all.equal(names_d_subgroups_count_friendly, gsub("% ", "Count of ", names_d_subgroups_friendly))

names_d_subgroups_alone_count_friendly <-  fixcolnames(names_d_subgroups_alone_count, 'r', 'long') 
all.equal(names_d_subgroups_alone_count_friendly, gsub("% ", "Count of ", names_d_subgroups_alone_friendly))

names_d_subgroups_nh_count_friendly    <-  fixcolnames(names_d_subgroups_nh_count, 'r', 'long')
all.equal(names_d_subgroups_nh_count_friendly, gsub("% ", "Count of ", names_d_subgroups_nh_friendly))

############################################################################## #

# ENVIRONMENTAL - raw and friendly (no pctiles) ####

names_e           <- map_headernames$rname[map_headernames$varlist == "names_e"]
names_e_friendly  <- fixcolnames(names_e, 'r', 'long') 
all.equal(names_e_friendly, 
          map_headernames$longname[map_headernames$varlist == "names_e"]
)

############################################################################## #

# EJ INDEXES - raw & friendly; + supp for raw & friendly; (not pctile) ####

#  4 types of EJ-related indicators: US2, US5, ST2, ST5: 
# x.eo versions,
# x.supp versions, 
# state.x.eo versions,  
# state.x.supp versions

# raw EJ actually has all 4 of these: US normal, State normal; US Suppl, State Suppl  versions (+friendly)

# cbind(map_headernames$longname[map_headernames$varlist == "names_e"], map_headernames$longname[map_headernames$varlist == "names_ej"])

names_ej <- map_headernames$rname[map_headernames$varlist == "names_ej"] # paste0('EJ.DISPARITY.', names_e, '.eo')
all.equal(names_ej, paste0('EJ.DISPARITY.', names_e, '.eo'))

names_ej_friendly <- fixcolnames(names_ej, 'r', 'short')
# c( # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED

names_ej_state          <-   map_headernames$rname[map_headernames$varlist == "names_ej_state"]  # paste0('state.', names_ej)
names_ej_state_friendly <- fixcolnames(names_ej_state, 'r', 'short') # paste0('State ', names_ej_friendly)
all.equal(names_ej_state,          paste0('state.', names_ej))
all.equal(names_ej_state_friendly, paste0('State ', names_ej_friendly))  # no 
## not quite the same
# cbind( names_ej_state_friendly, paste0('State ', names_ej_friendly)) 

# dput(fixcolnames(names_ej_state, 'r', 'long'))


names_ej_supp <- map_headernames$rname[map_headernames$varlist == "names_ej_supp"] 
names_ej_supp_friendly <-  fixcolnames(names_ej_supp, 'r', 'short') 
all.equal(names_ej_supp, gsub("\\.eo$", ".supp", names_ej))#  gsub("\\.eo$", ".supp", names_ej) # not just this: # paste0(          names_ej, '.supp')
## not quite the same
# cbind( names_ej_supp_friendly, gsub("EJ", "Supp. EJ", names_ej_friendly))
# names_ej_supp_friendly                                             

# dput(fixcolnames(names_ej_supp, 'r', 'long'))

names_ej_supp_state <- map_headernames$rname[map_headernames$varlist == "names_ej_supp_state"]
# paste0('state.', names_ej_supp)
names_ej_supp_state_friendly <- fixcolnames(names_ej_supp_state, 'r', 'short') 
# gsub("EJ", "Supp. EJ", names_ej_state_friendly)
all.equal(names_ej_supp_state, paste0('state.', names_ej_supp))
all.equal(names_ej_supp_state_friendly, gsub("EJ", "Supp. EJ", names_ej_state_friendly))
## not quite the same
# cbind(names_ej_supp_state_friendly, gsub("EJ", "Supp. EJ", names_ej_state_friendly)) 
# names_ej_supp_state_friendly
#  [1,] "EJ Supp: PM2.5 (state raw)"                     "Supp. EJ: PM2.5 (state raw)"                    
#  [2,] "EJ Supp: Ozone (state raw)"                     "Supp. EJ: Ozone (state raw)"                    
#  [3,] "EJ Supp: NO2 (state raw)"                       "Supp. EJ: NO2 (state raw)"                      
#  [4,] "EJ Supp: Diesel PM (state raw)"                 "Supp. EJ: Diesel PM (state raw)"                
#  [5,] "EJ Supp: Toxic Releases to Air (state raw)"     "Supp. EJ: Toxic Releases to Air (state raw)"    
#  [6,] "EJ Supp: Traffic proximity (state raw)"         "Supp. EJ: Traffic proximity (state raw)"        
#  [7,] "EJ Supp: Lead paint (state raw)"                "Supp. EJ: Lead paint (state raw)"               
#  [8,] "EJ Supp: Superfund Proximity (state raw)"       "Supp. EJ: Superfund Proximity (state raw)"      
#  [9,] "EJ Supp: RMP Facility Proximity (state raw)"    "Supp. EJ: RMP Facility Proximity (state raw)"   
# [10,] "EJ Supp: TSDF (state raw)"                      "Supp. EJ: TSDF (state raw)"                     
# [11,] "EJ Supp: Underground storage tanks (state raw)" "Supp. EJ: Underground storage tanks (state raw)"
# [12,] "EJ Supp: Wastewater discharge (state raw)"      "Supp. EJ: Wastewater discharge (state raw)"     
# [13,] "EJ Supp: Drinking (state raw)"                  "Supp. EJ: Drinking (state raw)"        

############################################################################## #

# PERCENTILES - D & sub; E; EJ (us & st, normal & supp) - friendly versions after that ####

names_d_pctile                 <- map_headernames$rname[map_headernames$varlist == "names_d_pctile"] 
names_d_state_pctile           <- map_headernames$rname[map_headernames$varlist == "names_d_state_pctile"] 
all.equal(names_d_pctile,       paste0(      'pctile.', names_d))
all.equal(names_d_state_pctile, paste0('state.pctile.', names_d))

names_d_subgroups_alone_pctile       <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_pctile"]
names_d_subgroups_alone_state_pctile <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_state_pctile"]
all.equal(names_d_subgroups_alone_pctile, paste0('pctile.', names_d_subgroups_alone))
all.equal(names_d_subgroups_alone_state_pctile, paste0('state.pctile.', names_d_subgroups_alone))

names_d_subgroups_nh_pctile       <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_pctile"]
names_d_subgroups_nh_state_pctile <- map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_state_pctile"]
all.equal(names_d_subgroups_nh_pctile, 
          paste0('pctile.', names_d_subgroups_nh))
all.equal(names_d_subgroups_nh_state_pctile, 
          paste0('state.pctile.', names_d_subgroups_nh))

if ('alone' %in% subgroups_type) {
  names_d_subgroups_pctile            <- names_d_subgroups_alone_pctile
  names_d_subgroups_state_pctile      <- names_d_subgroups_alone_state_pctile
} else {
  names_d_subgroups_pctile            <- names_d_subgroups_nh_pctile
  names_d_subgroups_state_pctile      <- names_d_subgroups_nh_state_pctile
}

names_e_pctile                 <- map_headernames$rname[map_headernames$varlist == "names_e_pctile"] 
all.equal(names_e_pctile, paste0(      'pctile.', names_e))

names_e_state_pctile            <- map_headernames$rname[map_headernames$varlist == "names_e_state_pctile"] 
all.equal(names_e_state_pctile, paste0(      'state.pctile.', names_e))

names_ej_pctile                <- map_headernames$rname[map_headernames$varlist == "names_ej_pctile"]
all.equal(names_ej_pctile, paste0(      'pctile.', names_ej))

names_ej_state_pctile          <- map_headernames$rname[map_headernames$varlist == "names_ej_state_pctile"]
all.equal(names_ej_state_pctile, paste0(      'state.pctile.', names_ej))

names_ej_supp_pctile           <-  map_headernames$rname[map_headernames$varlist == "names_ej_supp_pctile"]
all.equal(names_ej_supp_pctile, paste0(      'pctile.', names_ej_supp))

names_ej_supp_state_pctile     <-  map_headernames$rname[map_headernames$varlist == "names_ej_supp_state_pctile"]
all.equal(names_ej_supp_state_pctile, paste0(      'state.pctile.', names_ej_supp))


# need no percentiles of counts like d_counts ***

################################# ############ #
# FRIENDLY PERCENTILES ??? ####
# - not sure I need or will use these, and whether named the same categories in map_headernames:

# note the description in map_headernames omitted the US and just said Percentile

names_d_pctile_friendly                 <- fixcolnames(names_d_pctile, 'r', 'long')
names_d_state_pctile_friendly           <- fixcolnames(names_d_state_pctile, 'r', 'long')
all.equal(names_d_pctile_friendly,       paste0(   'US percentile for ',    names_d_friendly))
all.equal(names_d_state_pctile_friendly, paste0(   'State percentile for ', names_d_friendly))

names_d_subgroups_alone_pctile_friendly       <- fixcolnames(names_d_subgroups_alone_pctile, 'r', 'long')
names_d_subgroups_alone_state_pctile_friendly <- fixcolnames(names_d_subgroups_alone_state_pctile, 'r', 'long')
all.equal(names_d_subgroups_alone_pctile_friendly,       paste0(   'US percentile for ', names_d_subgroups_alone_friendly) )
all.equal(names_d_subgroups_alone_state_pctile_friendly, paste0('State percentile for ', names_d_subgroups_alone_friendly) )

names_d_subgroups_nh_pctile_friendly          <- fixcolnames(names_d_subgroups_nh_pctile, 'r', 'long')
names_d_subgroups_nh_state_pctile_friendly    <- fixcolnames(names_d_subgroups_nh_state_pctile, 'r', 'long')
all.equal(names_d_subgroups_nh_pctile_friendly,
          paste0(   'US percentile for ', names_d_subgroups_nh_friendly)) # newer 
all.equal(names_d_subgroups_nh_state_pctile_friendly, 
          paste0('State percentile for ', names_d_subgroups_nh_friendly)) # newer 


if ('alone' %in% subgroups_type) {
  names_d_subgroups_pctile_friendly             <- names_d_subgroups_alone_pctile_friendly
  names_d_subgroups_state_pctile_friendly       <- names_d_subgroups_alone_state_pctile_friendly
} else {
  names_d_subgroups_pctile_friendly             <- names_d_subgroups_nh_pctile_friendly
  names_d_subgroups_state_pctile_friendly       <- names_d_subgroups_nh_state_pctile_friendly
}


names_e_pctile_friendly                 <- fixcolnames(names_e_pctile, 'r', 'long') 
all.equal(
  names_e_pctile_friendly,
  paste0(  'US percentile for ',  names_e_friendly)
)

names_e_state_pctile_friendly           <- fixcolnames(names_e_state_pctile, 'r', 'long') 
all.equal(
  names_e_state_pctile_friendly,
  paste0('State percentile for ', names_e_friendly)
)

#  *** EJScreen refers to EJ indexes without mentioning they are percentiles, since that is the only way they are reported.
# Should EJAM do same, or be more explicit with friendly names? 
# names_ej_friendly can be used instead for pctiles, probably, since it does not mention whether it is raw or percentile

# names_ej_pctile_friendly            <- names_ej_friendly
# names_ej_state_pctile_friendly      <- names_ej_friendly
# 
# names_ej_supp_pctile_friendly       <- names_ej_friendly
# names_ej_supp_state_pctile_friendly <- names_ej_friendly

# more explicit/ longer names: 
# raw EJ has all 4 of these: US normal, State normal; US Suppl, State Suppl  versions (+friendly)

names_ej_pctile_friendly                <- paste0(   'US percentile for ', names_ej_friendly)
names_ej_state_pctile_friendly          <- paste0('State percentile for ', names_ej_friendly)
names_ej_supp_pctile_friendly           <- paste0(   'US percentile for ', names_ej_supp_friendly)  
names_ej_supp_state_pctile_friendly     <- paste0('State percentile for ', names_ej_supp_friendly) 

### these are 3 different approaches - ideally a  map_headernames version would be used for names_ej_pctile_friendly etc.
# cbind(
#      fixcolnames(names_ej_pctile, 'r', 'short'), 
#      paste0(   'US percentile for ', names_ej_friendly),
#      fixcolnames(names_ej_pctile, 'r', 'long')
#  )
# names_ej_pctile_friendly                                                                                                
# [1,] "EJ: PM2.5 (US%ile)"             "US percentile for EJ: PM2.5"      "US percentile for EJ Index for Particulate Matter (PM 2.5)"              
# [2,] "EJ: Ozone (US%ile)"             "US percentile for EJ: Ozone"      "US percentile for EJ Index for Ozone"                                    
# [3,] "EJ: NO2 (US%ile)"               "US percentile for EJ: NO2"        "US percentile for EJ Index for Nitrogen Dioxide (NO2)"                   
# [4,] "EJ: Diesel PM (US%ile)"         "US percentile for EJ: Diesel PM"             "US percentile for EJ Index for Air Toxics Diesel Particulate Matter"     
# [5,] "EJ: Toxic Air Release (US%ile)" "US percentile for EJ: Toxic Releases to Air" "US percentile for EJ Index for Toxic Releases to Air"                    
# [6,] "EJ: Traffic (US%ile)"           "US percentile for EJ: Traffic"               "US percentile for EJ Index for Traffic Proximity and Volume"             
# [7,] "EJ: %pre-1960 (US%ile)"         "US percentile for EJ: Lead paint"            "US percentile for EJ Index for Lead Paint Indicator"                     
# [8,] "EJ: NPL (US%ile)"               "US percentile for EJ: NPL"                   "US percentile for EJ Index for Superfund Proximity"                      
# [9,] "EJ: RMP (US%ile)"               "US percentile for EJ: RMP"                   "US percentile for EJ Index for RMP Proximity"                            
# [10,] "EJ: TSDF (US%ile)"              "US percentile for EJ: TSDF"                 "US percentile for EJ Index for Hazardous Waste Proximity"                
# [11,] "EJ: UST (US%ile)"               "US percentile for EJ: UST"                  "US percentile for EJ Index for Underground Storage Tanks (UST) indicator"
# [12,] "EJ: NPDES (US%ile)"             "US percentile for EJ: Wastewater"           "US percentile for EJ Index for Wastewater Discharge Indicator"           
# [13,] "EJ: Drinking (US%ile)"          "US percentile for EJ: Drinking Water"       "US percentile for EJ Index for Drinking Water Non-Compliance"            
# 
# dput(fixcolnames(names_ej_state_pctile, 'r', 'short'))
# dput(fixcolnames(names_ej_supp_pctile, 'r', 'short'))
# dput(fixcolnames(names_ej_supp_state_pctile, 'r', 'short'))
# 
# dput(fixcolnames(names_ej_state_pctile, 'r', 'long'))
# dput(fixcolnames(names_ej_supp_pctile, 'r', 'long'))
# dput(fixcolnames(names_ej_supp_state_pctile, 'r', 'long'))

################################# ############ #


# compiled list of raw, root indicators for which the US and State pctiles needing to be looked up ( no need for   friendly versions)

names_pctile <- c( # USA pctiles   # not including friendly version
  names_d_pctile,
  
  #  DO NOT INCLUDE 3 versions here (subgroups, subgroups_alone, subgroups_nh) - just the one we are actually going to use
  names_d_subgroups_pctile,     # only include one or the other type if at all, but note EJScreen 2.2 does NOT report these as percentiles - only raw percentage.
  # names_d_subgroups_nh_pctile,                   #  will start to use when ready  xxx
  # names_d_subgroups_alone_pctile,                #  will start to use when ready  xxx
  # 
  names_e_pctile,
  names_ej_pctile,      
  names_ej_supp_pctile 
)
# all STATE pctiles, but not friendly versions 
names_state_pctile <- c( # STATE pctiles   # not including friendly version
  gsub("pctile", "state.pctile", names_pctile)
)

# which base indicators need to be reported as percentiles? ####

names_need_pctile       <- gsub(      "pctile.", "", names_pctile)   # NOTE THIS IS US ONLY - NOT STATE
names_need_state_pctile <- gsub("state.pctile.", "", names_state_pctile)  # NOTE THIS IS STATE
# no friendly version need for that list
rm(names_pctile, names_state_pctile)
# varsneedpctiles <- c(names_e,  names_d, names_d_subgroups, names_ej)  # _nh   and  _alone  ?
# varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
# varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)



############################################################################## #

# AVERAGE (not MEDIAN?) IN US AND STATE - raw & friendly versions - not for counts and not for EJ indexes ??  ####

names_d_avg                       <- paste0(      "avg.", names_d) 
names_d_state_avg                 <- paste0("state.avg.", names_d)
all.equal(names_d_avg,       map_headernames$rname[map_headernames$varlist == "names_d_avg"])
all.equal(names_d_state_avg, map_headernames$rname[map_headernames$varlist == "names_d_state_avg"])

names_d_subgroups_alone_avg       <- paste0(      "avg.", names_d_subgroups_alone)
names_d_subgroups_alone_state_avg <- paste0("state.avg.", names_d_subgroups_alone)
all.equal(names_d_subgroups_alone_avg,       map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_avg"])
all.equal(names_d_subgroups_alone_state_avg, map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_state_avg"])

names_d_subgroups_nh_avg          <- paste0(      "avg.", names_d_subgroups_nh)  
names_d_subgroups_nh_state_avg    <- paste0("state.avg.", names_d_subgroups_nh)
all.equal(names_d_subgroups_nh_avg,       map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_avg"])
all.equal(names_d_subgroups_nh_state_avg, map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_state_avg"])

names_d_subgroups_avg             <- paste0(      "avg.", names_d_subgroups)   # names_d_subgroups_nh_avg       or  names_d_subgroups_alone_avg
names_d_subgroups_state_avg       <- paste0("state.avg.", names_d_subgroups)   # names_d_subgroups_nh_state_avg  or names_d_subgroups_alone_state_avg
all.equal(names_d_subgroups_avg,       map_headernames$rname[map_headernames$varlist == "names_d_subgroups_avg"])
all.equal(names_d_subgroups_state_avg, map_headernames$rname[map_headernames$varlist == "names_d_subgroups_state_avg"])

names_e_avg       <- paste0(      "avg.", names_e)
names_e_state_avg <- paste0("state.avg.", names_e)
all.equal(names_e_avg,       map_headernames$rname[map_headernames$varlist == "names_e_avg"])
all.equal(names_e_state_avg, map_headernames$rname[map_headernames$varlist == "names_e_state_avg"])

# names_e_med, names_e_state_med,
# names_d_med, names_d_state_med,

#########   map_headernames differs / inconsistent for these:

names_d_avg_friendly       <- paste0("US average ",    names_d_friendly)
names_d_state_avg_friendly <- paste0("State average ", names_d_friendly) 
all.equal(names_d_avg_friendly,       fixcolnames(names_d_avg, 'r', 'long'))
all.equal(names_d_state_avg_friendly, fixcolnames(names_d_state_avg, 'r', 'long'))

names_d_subgroups_nh_avg_friendly       <- paste0("US average ",    names_d_subgroups_nh_friendly)
names_d_subgroups_nh_state_avg_friendly <- paste0("State average ", names_d_subgroups_nh_friendly)
cbind(names_d_subgroups_nh_avg_friendly, fixcolnames(names_d_subgroups_nh_avg, 'r', 'long'))
cbind(names_d_subgroups_nh_state_avg_friendly, fixcolnames(names_d_subgroups_nh_state_avg, 'r', 'long'))

names_d_subgroups_alone_avg_friendly       <- paste0("US average ",    names_d_subgroups_alone_friendly)
names_d_subgroups_alone_state_avg_friendly <- paste0("State average ", names_d_subgroups_alone_friendly)
# dput(fixcolnames(names_d_subgroups_alone_avg, 'r', 'long'))
# dput(fixcolnames(names_d_subgroups_alone_state_avg, 'r', 'long'))

names_d_subgroups_avg_friendly       <- paste0("US average ",    names_d_subgroups_friendly)  #  
names_d_subgroups_state_avg_friendly <- paste0("State average ", names_d_subgroups_friendly)  # 
# dput(fixcolnames(names_d_subgroups_avg, 'r', 'long'))
# dput(fixcolnames(names_d_subgroups_state_avg, 'r', 'long'))

names_e_avg_friendly       <- paste0("US Avg ",    names_e_friendly)
names_e_state_avg_friendly <- paste0("State Avg ", names_e_friendly)
# dput(fixcolnames(names_e_avg, 'r', 'long'))
# dput(fixcolnames(names_e_state_avg, 'r', 'long'))

# no ratios used for raw EJ indexes ?

# no averages no ratios for counts like count of POC, count of low income, etc.

############################################################################## #

# RATIOS TO AVERAGE ####

## should switch to map_headernames way or at least confirm same **

names_d_ratio_to_avg  <- paste0("ratio.to.", names_d_avg) 
names_d_ratio_to_state_avg <- paste0("ratio.to.", names_d_state_avg) 
all.equal(names_d_ratio_to_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_ratio_to_avg"]
)
all.equal(names_d_ratio_to_state_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_ratio_to_state_avg"]
)

names_d_subgroups_nh_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_nh_avg)  
names_d_subgroups_nh_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_nh_state_avg)
all.equal(names_d_subgroups_nh_ratio_to_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_ratio_to_avg"]
)
all.equal(names_d_subgroups_nh_ratio_to_state_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_nh_ratio_to_state_avg"]
)

names_d_subgroups_alone_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_alone_avg)  
names_d_subgroups_alone_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_alone_state_avg)
all.equal(names_d_subgroups_alone_ratio_to_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_ratio_to_avg"]
)
all.equal(names_d_subgroups_alone_ratio_to_state_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_alone_ratio_to_state_avg"]
)

names_d_subgroups_ratio_to_avg       <- paste0("ratio.to.", names_d_subgroups_avg)            
names_d_subgroups_ratio_to_state_avg <- paste0("ratio.to.", names_d_subgroups_state_avg)    
all.equal(names_d_subgroups_ratio_to_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_ratio_to_avg"]
)
all.equal(names_d_subgroups_ratio_to_state_avg,
          map_headernames$rname[map_headernames$varlist == "names_d_subgroups_ratio_to_state_avg"]
)

names_e_ratio_to_avg       <- paste0("ratio.to.", names_e_avg)  
names_e_ratio_to_state_avg <- paste0("ratio.to.", names_e_state_avg)
all.equal(names_e_ratio_to_avg,
          map_headernames$rname[map_headernames$varlist == "names_e_ratio_to_avg"]
)
all.equal(names_e_ratio_to_state_avg,
          map_headernames$rname[map_headernames$varlist == "names_e_ratio_to_state_avg"]
)

names_d_ratio_to_avg_friendly       <- paste0("Ratio to ", names_d_avg_friendly)
names_d_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_state_avg_friendly) 

##### these names are somewhat different in map_headernames

# # all.equal
# cbind(names_d_ratio_to_avg_friendly,
#           fixcolnames(names_d_ratio_to_avg, 'r', 'long')
# )
# # all.equal
# cbind(names_d_ratio_to_state_avg_friendly,
#           fixcolnames(names_d_ratio_to_state_avg, 'r', 'long')
# )


names_d_subgroups_nh_ratio_to_avg_friendly          <- paste0("Ratio to ", names_d_subgroups_nh_avg_friendly)
names_d_subgroups_nh_ratio_to_state_avg_friendly    <- paste0("Ratio to ", names_d_subgroups_nh_state_avg_friendly)
  ### differ
# all.equal(names_d_subgroups_nh_ratio_to_avg_friendly,  fixcolnames(names_d_subgroups_nh_ratio_to_avg, 'r', 'long'))
# all.equal(names_d_subgroups_nh_ratio_to_state_avg_friendly,  fixcolnames(names_d_subgroups_nh_ratio_to_state_avg, 'r', 'long'))

names_d_subgroups_alone_ratio_to_avg_friendly       <- paste0("Ratio to ", names_d_subgroups_alone_avg_friendly)
names_d_subgroups_alone_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_d_subgroups_alone_state_avg_friendly)
  ### differ
# all.equal(names_d_subgroups_alone_ratio_to_avg_friendly,  fixcolnames(names_d_subgroups_alone_ratio_to_avg, 'r', 'long'))
# all.equal(names_d_subgroups_alone_ratio_to_state_avg_friendly,  fixcolnames(names_d_subgroups_alone_ratio_to_state_avg, 'r', 'long'))

names_d_subgroups_ratio_to_avg_friendly             <- paste0("Ratio to ", names_d_subgroups_avg_friendly)      
names_d_subgroups_ratio_to_state_avg_friendly       <- paste0("Ratio to ", names_d_subgroups_state_avg_friendly)  
  ### differ
# all.equal(names_d_subgroups_ratio_to_avg_friendly,  fixcolnames(names_d_subgroups_ratio_to_avg, 'r', 'long'))
# all.equal(names_d_subgroups_ratio_to_state_avg_friendly,  fixcolnames(names_d_subgroups_ratio_to_state_avg, 'r', 'long'))

names_e_ratio_to_avg_friendly       <- paste0("Ratio to ", names_e_avg_friendly)
names_e_ratio_to_state_avg_friendly <- paste0("Ratio to ", names_e_state_avg_friendly)
  all.equal(names_e_ratio_to_avg_friendly,  fixcolnames(names_e_ratio_to_avg, 'r', 'long'))
  all.equal(names_e_ratio_to_state_avg_friendly,  fixcolnames(names_e_ratio_to_state_avg, 'r', 'long'))
# dput(fixcolnames(names_e_ratio_to_avg, 'r', 'long'))
# dput(fixcolnames(names_e_ratio_to_state_avg, 'r', 'long'))

############################################################################## #
# these ####

# names_these is just another way to conveniently refer to these much-used variables from within server code

## see doaggregate() where these are used:

#  Only one of the 3 ways can be used here:
#  DO NOT INCLUDE 3 versions here (subgroups, subgroups_alone, subgroups_nh) - just the one we are actually going to use
# 
# if (subgroups_type == 'alone') {
#   names_these                    <- c(names_d,              names_d_subgroups_alone,              names_e)   
#   names_these_avg                <- c(names_d_avg,          names_d_subgroups_alone_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
#   names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_alone_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
#   names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_alone_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
#   names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_alone_ratio_to_state_avg,   names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# } else {
#   names_these                    <- c(names_d,              names_d_subgroups_nh,              names_e)   
#   names_these_avg                <- c(names_d_avg,          names_d_subgroups_nh_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
#   names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_nh_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
#   names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_nh_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
#   names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_nh_ratio_to_state_avg,      names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# }
# if (subgroups_type == 'original') {  #  ONLY 
names_these                    <- c(names_d,              names_d_subgroups,              names_e)
names_these_avg                <- c(names_d_avg,          names_d_subgroups_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,    names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
# }

# no need for friendly versions?

# counts and e and ej not included here in names_these  ***

############################################################################## #
#  compilations of names (these lack friendly version of the group)

# [14,] "names_wts"         

# [1,] "names_all_r"      

# [13,] "names_these"        
# [2,]  "names_these_avg"          
# [11,] "names_these_state_avg"         
# [9,]  "names_these_ratio_to_avg"      
# [10,] "names_these_ratio_to_state_avg"

# [6,] "names_need_pctile"             
# [7,] "names_need_state_pctile"       


############################################################################## #
# all_r ####
#   this is not used by the app but may be useful in maintaining map_headernames or namez, etc.

names_all_r =  c(
  names_d_other_count,
  names_d_count, 
  names_d, 
  names_d_avg,           names_d_state_avg, 
  names_d_pctile,        names_d_state_pctile,       
  names_d_ratio_to_avg,  names_d_ratio_to_state_avg,
  
  # subgroups_lists,   #   ONLY INCLUDE THE ONE BEING USED BY CODE ?  # subgroups   # not sure we need to include here all 3 versions of names of subgroups indicators
  names_d_subgroups,         
  # names_d_subgroups_friendly,
  names_d_subgroups_avg,         names_d_subgroups_state_avg,
  # names_d_subgroups_avg_friendly,names_d_subgroups_state_avg_friendly,
  names_d_subgroups_pctile,      names_d_subgroups_state_pctile, 
  # names_d_subgroups_pctile_friendly, names_d_subgroups_state_pctile_friendly, 
  names_d_subgroups_ratio_to_avg,          names_d_subgroups_ratio_to_state_avg,
  # names_d_subgroups_ratio_to_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  names_d_subgroups_count,  # no avg, no ratio, no pctiles, for counts
  
  
  c(names_e,  
    names_e_avg,          names_e_state_avg, 
    names_e_pctile,       names_e_state_pctile),
  names_e_ratio_to_avg, names_e_ratio_to_state_avg,
  
  # c(names_these_ratio_to_avg, names_these_ratio_to_state_avg), 
  
  c(names_ej,             names_ej_state,
    names_ej_supp,        names_ej_supp_state,
    names_ej_pctile,      names_ej_state_pctile,
    names_ej_supp_pctile, names_ej_supp_state_pctile)  # no ratios for EJ
)
names_all_r <- unique(names_all_r)

############################################################################## #
# sub_nh_text    <- c( 'names_d_subgroups_nh_count',           
#                   c('names_d_subgroups_nh',    'names_d_subgroups_nh_avg',    'names_d_subgroups_nh_state_avg',    'names_d_subgroups_nh_pctile',    'names_d_subgroups_nh_state_pctile') )
sub_nh_text    <- c('names_d_subgroups_nh',        
                    'names_d_subgroups_nh_friendly', 
                    'names_d_subgroups_nh_avg',             'names_d_subgroups_nh_state_avg',   
                    'names_d_subgroups_nh_avg_friendly',    'names_d_subgroups_nh_state_avg_friendly',  
                    'names_d_subgroups_nh_pctile',          'names_d_subgroups_nh_state_pctile',   
                    'names_d_subgroups_nh_pctile_friendly', 'names_d_subgroups_nh_state_pctile_friendly',   
                    'names_d_subgroups_nh_ratio_to_avg',          'names_d_subgroups_nh_ratio_to_state_avg',  
                    'names_d_subgroups_nh_ratio_to_avg_friendly', 'names_d_subgroups_nh_ratio_to_state_avg_friendly',   
                    # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,       
                    'names_d_subgroups_nh_count'  # no avg, no ratio, no pctiles, for counts 
)

# sub_alone_text <- c( 'names_d_subgroups_alone_count',  
#                   c('names_d_subgroups_alone', 'names_d_subgroups_alone_avg', 'names_d_subgroups_alone_state_avg', 'names_d_subgroups_alone_pctile', 'names_d_subgroups_alone_state_pctile') )   
sub_alone_text <- c(
  'names_d_subgroups_alone',        
  'names_d_subgroups_alone_friendly', 
  'names_d_subgroups_alone_avg',             'names_d_subgroups_alone_state_avg',   
  'names_d_subgroups_alone_avg_friendly',    'names_d_subgroups_alone_state_avg_friendly',  
  'names_d_subgroups_alone_pctile',          'names_d_subgroups_alone_state_pctile',   
  'names_d_subgroups_alone_pctile_friendly', 'names_d_subgroups_alone_state_pctile_friendly',   
  'names_d_subgroups_alone_ratio_to_avg',          'names_d_subgroups_alone_ratio_to_state_avg',  
  'names_d_subgroups_alone_ratio_to_avg_friendly', 'names_d_subgroups_alone_ratio_to_state_avg_friendly',   
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,       
  'names_d_subgroups_alone_count' # no avg, no ratio, no pctiles, for counts   
)

if ("alone" %in% subgroups_type) {
  sub_o_text <-  sub_alone_text
} else {
  sub_o_text <-  sub_nh_text
}

subgroups_lists_text <-  sub_o_text  



# 'names_d_subgroups',               
# 'names_d_subgroups_friendly',      
# 'names_d_subgroups_avg',             'names_d_subgroups_state_avg',      
# 'names_d_subgroups_avg_friendly',    'names_d_subgroups_state_avg_friendly',      
# 'names_d_subgroups_pctile',          'names_d_subgroups_state_pctile',       
# 'names_d_subgroups_pctile_friendly', 'names_d_subgroups_state_pctile_friendly',       
# 'names_d_subgroups_ratio_to_avg',          'names_d_subgroups_ratio_to_state_avg',      
# 'names_d_subgroups_ratio_to_avg_friendly', 'names_d_subgroups_ratio_to_state_avg_friendly',        
# # names_d_subgroups_med,     names_d_subgroups_state_med,          
# 'names_d_subgroups_count', # no avg, no ratio, no pctiles, for counts         


############################################################################## #

# **namez__ ####

# try putting these all in one list instead of multiple objects? could recode later to use namez$d_friendly instead of names_d_friendly etc.
# and/or    just store them in a big table

namesoflistsofnames = c(
  'names_all_r',  # and names_all will get created 
  'names_wts',
  ############################################ # 
  
  'names_d', 
  'names_d_friendly', # not for percentiles and counts?
  'names_d_avg',             'names_d_state_avg', 
  'names_d_avg_friendly',    'names_d_state_avg_friendly' ,
  'names_d_pctile',          'names_d_state_pctile',       
  'names_d_pctile_friendly', 'names_d_state_pctile_friendly' , # new
  'names_d_ratio_to_avg',          'names_d_ratio_to_state_avg',
  'names_d_ratio_to_avg_friendly', 'names_d_ratio_to_state_avg_friendly', 
  # names_d_med,  names_d_state_med,
  
  'names_d_count',     
  'names_d_count_friendly', 
  'names_d_other_count', # includes pop and other denominator counts. no avg, no ratio, no pctiles for this needed.
  'names_d_other_count_friendly',  # no avg, no ratios, no percentiles, for counts
  
  ############################################ # 
  # subgroups 
  
  # 'names_d_subgroups',
  # 'names_d_subgroups_nh',
  # 'names_d_subgroups_alone',
  # subgroups_lists_text, 
  # sub_nh_text,
  # sub_alone_text,
  
  #  plain subgroups,   subgroups_alone will match ejscreen v 2.2
  'names_d_subgroups',         
  'names_d_subgroups_friendly',
  'names_d_subgroups_avg',         'names_d_subgroups_state_avg',
  'names_d_subgroups_avg_friendly','names_d_subgroups_state_avg_friendly',
  'names_d_subgroups_pctile',      'names_d_subgroups_state_pctile', 
  'names_d_subgroups_pctile_friendly', 'names_d_subgroups_state_pctile_friendly', 
  'names_d_subgroups_ratio_to_avg',          'names_d_subgroups_ratio_to_state_avg',
  'names_d_subgroups_ratio_to_avg_friendly', 'names_d_subgroups_ratio_to_state_avg_friendly',
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  'names_d_subgroups_count',  # no avg, no ratio, no pctiles, for counts
  
  'names_d_subgroups_nh',         
  'names_d_subgroups_nh_friendly',
  'names_d_subgroups_nh_avg',         'names_d_subgroups_nh_state_avg',
  'names_d_subgroups_nh_avg_friendly','names_d_subgroups_nh_state_avg_friendly',
  'names_d_subgroups_nh_pctile',      'names_d_subgroups_nh_state_pctile', 
  'names_d_subgroups_nh_pctile_friendly', 'names_d_subgroups_nh_state_pctile_friendly', 
  'names_d_subgroups_nh_ratio_to_avg',          'names_d_subgroups_nh_ratio_to_state_avg',
  'names_d_subgroups_nh_ratio_to_avg_friendly', 'names_d_subgroups_nh_ratio_to_state_avg_friendly',
  # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,
  'names_d_subgroups_nh_count',  # no avg, no ratio, no pctiles, for counts
  
  'names_d_subgroups_alone',         
  'names_d_subgroups_alone_friendly',
  'names_d_subgroups_alone_avg',         'names_d_subgroups_alone_state_avg',
  'names_d_subgroups_alone_avg_friendly','names_d_subgroups_alone_state_avg_friendly',
  'names_d_subgroups_alone_pctile',      'names_d_subgroups_alone_state_pctile', 
  'names_d_subgroups_alone_pctile_friendly', 'names_d_subgroups_alone_state_pctile_friendly', 
  'names_d_subgroups_alone_ratio_to_avg',          'names_d_subgroups_alone_ratio_to_state_avg',
  'names_d_subgroups_alone_ratio_to_avg_friendly', 'names_d_subgroups_alone_ratio_to_state_avg_friendly',
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,
  'names_d_subgroups_alone_count',  # no avg, no ratio, no pctiles, for counts
  
  ############################################ # 
  
  'names_e',           
  'names_e_friendly',
  'names_e_avg',             'names_e_state_avg', 
  'names_e_avg_friendly',    'names_e_state_avg_friendly'  ,
  'names_e_pctile',          'names_e_state_pctile', 
  'names_e_pctile_friendly', 'names_e_state_pctile_friendly', # new
  'names_e_ratio_to_avg',          'names_e_ratio_to_state_avg',
  'names_e_ratio_to_avg_friendly', 'names_e_ratio_to_state_avg_friendly',
  # names_e_med, names_e_state_med,  
  ############################################ # 
  
  'names_ej',           'names_ej_state', # RAW EJ SCORE
  'names_ej_friendly',  'names_ej_state_friendly', # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  
  'names_ej_supp',          'names_ej_supp_state', 
  'names_ej_supp_friendly', 'names_ej_supp_state_friendly', 
  
  'names_ej_pctile',          'names_ej_state_pctile' ,
  'names_ej_pctile_friendly', 'names_ej_state_pctile_friendly',  # new
  
  'names_ej_supp_pctile',          'names_ej_supp_state_pctile',  ################################ # had been missing
  'names_ej_supp_pctile_friendly', 'names_ej_supp_state_pctile_friendly', # new
  ############################################ # 
  
  # 'names_pctile',  # all US pctile indicators #  - this is not used though
  # 'names_state_pctile', #  - this is not used though
  'names_need_pctile',  # base indicators that need to be reported as US percentiles not just raw scores
  'names_need_state_pctile', 
  
  'names_these',
  'names_these_avg',
  'names_these_state_avg',
  'names_these_ratio_to_avg',
  'names_these_ratio_to_state_avg'
)

namez <- lapply(namesoflistsofnames, get)
names(namez) <- gsub("^names_","", namesoflistsofnames)

# **names_all__ ####
# NOTE THIS IS VERY DIFFERENT THAN names_all_batch !!
names_all <- as.vector(unlist(namez))
names_all <- unique(names_all) # pop would appear twice

namesoflistsofnames <- c('names_all', namesoflistsofnames)
############################################################################## #

# could double check to see if fixcolnames and map_headernames give same answers as the lists here. 



############################################################################## ############################################################################### #
############################################################################## ############################################################################### #


#  metadata_add & USE_DATA ####


namez <- metadata_add(namez)
usethis::use_data(namez, overwrite = TRUE)

#  STORE EVERY OBJECT but dont add metadata to each ####

names_d_subgroups <- metadata_add(names_d_subgroups)
names_d     <- metadata_add(names_d)
names_e     <- metadata_add(names_e)

names_ej            <- metadata_add(names_ej)
names_ej_supp       <- metadata_add(names_ej_supp)
names_ej_state      <- metadata_add(names_ej_state)
names_ej_supp_state <- metadata_add(names_ej_supp_state)

names_all_r <- metadata_add(names_all_r)

usethis::use_data(
  
  names_all, names_all_r,
  names_wts, 
  ############################################ # 
  
  names_d, 
  names_d_friendly, # not for percentiles and counts?
  names_d_avg,          names_d_state_avg, 
  names_d_avg_friendly, names_d_state_avg_friendly ,
  names_d_pctile,       names_d_state_pctile,           
  names_d_pctile_friendly, names_d_state_pctile_friendly , # new
  names_d_ratio_to_avg,          names_d_ratio_to_state_avg,
  names_d_ratio_to_avg_friendly, names_d_ratio_to_state_avg_friendly, 
  # names_d_med,  names_d_state_med,
  names_d_count,
  names_d_count_friendly,
  names_d_other_count,  # includes pop and other denominator counts. no avg, no ratio, no pctiles for this needed.
  names_d_other_count_friendly, # no avg, no ratios, no percentiles, for counts
  
  ############################################ # 
  # subgroups 
  
  #  plain subgroups,   subgroups_alone will match ejscreen v 2.2
  names_d_subgroups,         
  names_d_subgroups_friendly,
  names_d_subgroups_avg,         names_d_subgroups_state_avg,
  names_d_subgroups_avg_friendly,names_d_subgroups_state_avg_friendly,
  names_d_subgroups_pctile,      names_d_subgroups_state_pctile, 
  names_d_subgroups_pctile_friendly, names_d_subgroups_state_pctile_friendly, 
  names_d_subgroups_ratio_to_avg,          names_d_subgroups_ratio_to_state_avg,
  names_d_subgroups_ratio_to_avg_friendly, names_d_subgroups_ratio_to_state_avg_friendly,
  # names_d_subgroups_med,     names_d_subgroups_state_med,
  names_d_subgroups_count,  # no avg, no ratio, no pctiles, for counts
  
  names_d_subgroups_nh,         
  names_d_subgroups_nh_friendly,
  names_d_subgroups_nh_avg,         names_d_subgroups_nh_state_avg,
  names_d_subgroups_nh_avg_friendly,names_d_subgroups_nh_state_avg_friendly,
  names_d_subgroups_nh_pctile,      names_d_subgroups_nh_state_pctile, 
  names_d_subgroups_nh_pctile_friendly, names_d_subgroups_nh_state_pctile_friendly, 
  names_d_subgroups_nh_ratio_to_avg,          names_d_subgroups_nh_ratio_to_state_avg,
  names_d_subgroups_nh_ratio_to_avg_friendly, names_d_subgroups_nh_ratio_to_state_avg_friendly,
  # names_d_subgroups_nh_med,     names_d_subgroups_nh_state_med,
  names_d_subgroups_nh_count,  # no avg, no ratio, no pctiles, for counts
  
  names_d_subgroups_alone,         
  names_d_subgroups_alone_friendly,
  names_d_subgroups_alone_avg,         names_d_subgroups_alone_state_avg,
  names_d_subgroups_alone_avg_friendly,names_d_subgroups_alone_state_avg_friendly,
  names_d_subgroups_alone_pctile,      names_d_subgroups_alone_state_pctile, 
  names_d_subgroups_alone_pctile_friendly, names_d_subgroups_alone_state_pctile_friendly, 
  names_d_subgroups_alone_ratio_to_avg,          names_d_subgroups_alone_ratio_to_state_avg,
  names_d_subgroups_alone_ratio_to_avg_friendly, names_d_subgroups_alone_ratio_to_state_avg_friendly,
  # names_d_subgroups_alone_med,     names_d_subgroups_alone_state_med,
  names_d_subgroups_alone_count,  # no avg, no ratio, no pctiles, for counts
  
  ############################################ # 
  
  names_e,           
  names_e_friendly,
  names_e_avg,          names_e_state_avg, 
  names_e_avg_friendly, names_e_state_avg_friendly,
  names_e_pctile,       names_e_state_pctile, 
  names_e_pctile_friendly, names_e_state_pctile_friendly, # new
  names_e_ratio_to_avg,          names_e_ratio_to_state_avg,
  names_e_ratio_to_avg_friendly, names_e_ratio_to_state_avg_friendly,
  # names_e_med, names_e_state_med,  
  ############################################ # 
  
  names_ej,           names_ej_state, # RAW EJ SCORE
  names_ej_friendly,  names_ej_state_friendly, # FRIENDLY (RAW) SCORE BUT CAN USE FOR PCTILE SINCE THAT IS THE ONLY THING REPORTED
  
  names_ej_supp,          names_ej_supp_state, 
  names_ej_supp_friendly, names_ej_supp_state_friendly, 
  
  names_ej_pctile,          names_ej_state_pctile ,
  names_ej_pctile_friendly, names_ej_state_pctile_friendly,  # new
  
  names_ej_supp_pctile,          names_ej_supp_state_pctile,  ################################ # had been missing
  names_ej_supp_pctile_friendly, names_ej_supp_state_pctile_friendly, # new
  
  ############################################ # 
  
  # names_pctile,  # all US pctile indicators - this is not used though
  # names_state_pctile,  #  - this is not used though
  names_need_pctile,  # base indicators that need to be reported as US percentiles not just raw scores
  names_need_state_pctile,
  
  names_these,                # used in code, and was set up to contain only subgroups or _alone or _nh but only one of those types
  names_these_avg,
  names_these_state_avg,
  names_these_ratio_to_avg,
  names_these_ratio_to_state_avg,
  
  overwrite = TRUE
)

############################################################################## #

if (exists("usastats") && exists("statestats")) {
  cat("checking new names_these vs colnames of whatever versions of usastats and statestats are attached or just created or else lazy loaded from installed pkg\n")
  notfound    = setdiff(names_these, names(usastats))   # uses attached possibly new version if different than installed version. fails if pkg not attached and new usastats not just made
  notfound_st = setdiff(names_these, names(statestats)) # ditto
  if (length(notfound   ) > 0) {warning('some of names_these are not column names found in usastats  ... ',        paste0(notfound,    collapse = ', '), '\n')} else {print('ok')}
  if (length(notfound_st) > 0) {warning('some of names_these are not column names found in statestats  ... ',      paste0(notfound_st, collapse = ', '), '\n')} else {print('ok')}
  rm(notfound, notfound_st)
} else {
  warning("did not check if all names_these are in names(statestats) and names(usastats) because usastats or statestats is missing")
}
if (exists("blockgroupstats")) {
  cat("checking new names_these vs colnames of whatever versions of blockgroupstats is attached or just created or else lazy loaded from installed pkg\n")
  notfound_bg = setdiff(names_these, names(blockgroupstats))   # ditto
  if (length(notfound_bg) > 0) {warning('some of names_these are not column names found in blockgroupstats  ... ', paste0(notfound_bg, collapse = ', '), '\n')} else {print('ok')}
  rm(notfound_bg)
}
return(names_these)
}
# ( make sure all the Envt variables etc are actually in usastats dataset)
# message('make sure all the Demog and Envt variables etc are actually in the latest installed usastats dataset')

# USE FUNCTION #### 
datacreate_names_of_indicators()    # this does metadata and use_data inside the function


cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())
