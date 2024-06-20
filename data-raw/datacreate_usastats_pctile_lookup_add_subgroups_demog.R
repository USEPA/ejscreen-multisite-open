############################################################################ #
# This script being used for  2023 version 2.2   

# First, usastats got created via  EJAM/data-raw/datacreate_usastats_pctile_lookup_add_subgroups_demog.R

# Then, this script below was to add columns to usastats and statestats with info on demographic subgroups and lowlifex
# since basic ftp site lookup tables for v2.2 lacked demog subgroups and other variables like lowlifex 
#   but we want to be able to analyze ratio to mean and percentiles for those.
# And to sort columns the same way they are ordered in names_d_subgroups_alone and names_d_subgroups_nh
#
# Also, need to confirm answer to questions of what States/places to include when calculating US percentiles.

names_d_subgroups_both <- c(EJAM::names_d_subgroups_nh, EJAM::names_d_subgroups_alone)

# setdiff(names(statestats2.2), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej_state, EJAM::names_ej_supp_state  )) #  
# [1] "PCTILE"             "REGION"             "LIFEEXP"            "HEARTDISEASE"       "ASTHMA"             "cancer.rate.adults"
# [7] "DISABILITYPCT"      "LIMITEDBBPCT"       "NOHINCPCT"          "flood_y00"          "flood_y30"          "fire_y00"          
# [13] "fire_y30"  
setdiff(names(statestats2.2), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej_state, EJAM::names_ej_supp_state))
setdiff(names(  usastats2.2), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej  ,     EJAM::names_ej_supp      ))

# fix a couple names not renamed by older map_headernames
names(statestats2.2)[names(statestats2.2) == "LIFEEXP"] <- "lifexyears"
names(  usastats2.2)[names(  usastats2.2) == "LIFEEXP"] <- "lifexyears"
# setdiff(names(  usastats2.2), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej  ,     EJAM::names_ej_supp      )) %in% names(EJAM::blockgroupstats)
# [1] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# lifexyears is in blockgroupstats 
# "lifexyears" %in% names(EJAM::blockgroupstats) # TRUE
# "lowlifex" %in% names(EJAM::blockgroupstats) # TRUE

############################################################################# # 
# to add percentile lookup info on demog subgroups, we need that info from blockgroupstats
library(data.table)
bg <- data.table::copy(EJAM::blockgroupstats)
bg <- data.table::setDF(bg)
golem::detach_all_attached() # unattach EJAM:: pkg while doing this to avoid confusion with lazyloaded vars

# check those subgroup demog columns in bg
  if (  0 == length(intersect(names_d_subgroups_both, names(bg)))) {
    stop('need subgroup data to create lookup percentile info for subgroups')
  } else {
    print('all variables are columns in bg')
    if (all(0 == bg[ , names_d_subgroups_both])) {
      #placeholder was there but all 0
      stop('need subgroup data to create lookup percentile info for subgroups')
    } else {print('ok not all zero values')}
  }
cbind(percent.NA   =  round(sapply(bg[,names_d_subgroups_both], function(x) sum(is.na(x) )) / NROW(bg), 3) * 100) # count is.na values for each indicator
cbind(percent.zero =  round(sapply(bg[,names_d_subgroups_both], function(x) sum(0 == (x), na.rm = T)) / NROW(bg), 3) * 100) # count zero  values for each indicator
  
# need ST column in blockgroupstats to create the statestats lookup table of demog subgroup info,
# so add FIPS.ST and ST columns to blockgroupstats if not already there

if (!("ST" %in% names(bg))) {
  # bg = ejanalysis package file addFIPScomponents(bg, fipscolname = "bgfips",  clean = FALSE)
  # bg$FIPS.ST <- substr(bg$bgfips,1,2)
  # bg$ST <- EJAM::stateinfo$ST[match(bg$FIPS.ST, EJAM::stateinfo$FIPS.ST)]
  bg$ST <- bg$ST_ABBREV
}
  
  ############################################################################# # 
  stop()
    # need to first RESOLVE QUESTION OF WHETHER US PCTILES ARE AMONG 50 STATES PLUS DC, OR ALSO PR, OR EVEN ALSO ISLAND AREAS.
  # SEEMS LIKE US PCTILE SHOULD BE AMONG 50+DC, EXCLUDING PR?
  # THEN STATE-SPECIFIC PCTILES WILL TREAT DC AND PR AS STATES, 
  # AND NOT SURE IF ISLAND AREAS GET PCTILES CREATED AND WHAT DATA ARE AVAILABLE IN THEM.
  
  
  
  ############################################################################# # 
  # statestats has some NA values, and some missing columns:
  # 
  # also, no Island Areas here at all as rows
  # also, no d_subgroups numbers yet - only names_d_subgroups_alone were there and zero values were there, not NA values.
  
  
############################################################################# # 

updated = FALSE

# drop the std.dev rows since never used and dropping them again for each indicator in looping use of pctile_from_raw_lookup() is kind of slow.
if (("std.dev" %in% (  usastats2.2$PCTILE))) {  usastats2.2 <-   usastats2.2[  usastats2.2$PCTILE != "std.dev", ] ; updated = TRUE}
if (("std.dev" %in% (statestats2.2$PCTILE))) {statestats2.2 <- statestats2.2[statestats2.2$PCTILE != "std.dev", ] ; updated = TRUE}

############################################################################ # 

################################################ #
# Add to demog subgroups to percentile lookup tables #### 


################################################ #
## CREATE USA LOOKUP ####
################################################ #


 names(usastats2.2)

if (all(usastats2.2[,intersect(names_d_subgroups_both, names(usastats2.2))] == 0)  | 
    any(!(names_d_subgroups_both %in% names(usastats2.2))))  {
  # Error in `[.data.frame`(usastats2.2, , names_d_subgroups_both) : 
  #   undefined columns selected
  
  usastats_subgroups   <- pctiles_lookup_create(data.frame(bg)[ , names_d_subgroups_both]) # function from EJAM package
  usastats_subgroups <- rbind(0, usastats_subgroups); usastats_subgroups$PCTILE[1] <- 0
  usastats_subgroups[1, c("OBJECTID", "REGION")] <- c(0, "USA")

  usastats2 <- cbind(usastats2.2, usastats_subgroups[ , setdiff(names(usastats_subgroups), names(usastats2.2))  ])

  # sort cols as sorted in names_d_subgroups_both
  subvars <- intersect(names_d_subgroups_both, names(usastats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(usastats2), subvars)
    usastats2 <- usastats2[ , c(othervars, subvars)]
  }
  all.equal(usastats2.2, usastats2[,1:length(names(usastats2.2))]) # usastats2 has same plus more columns
  usastats2.2 <- usastats2
   
  rm( usastats2, usastats_subgroups)
  
} # done with usastats

################################################ #
##  CREATE STATESTATS LOOKUP TABLE ####
################################################ #

# if (all(statestats2.2[,names_d_subgroups_both] == 0)  | any(!(names_d_subgroups_both %in% names(statestats2.2)))) {
  
  statestats_subgroups <- pctiles_lookup_create(data.frame(bg)[ , names_d_subgroups_both], zone.vector = bg$ST) # from EJAM package
  
  # names(statestats_subgroups)
  morecols = data.frame(as.list(rep(0,length(names_d_subgroups_both))))
  names(morecols) <- names_d_subgroups_both
  zerorowperstate <- data.frame(
    OBJECTID = 0,
    REGION = unique(statestats_subgroups$REGION),
    PCTILE = 0, 
    morecols
  )
  statestats_subgroups <- rbind(statestats_subgroups, zerorowperstate)
  
  statestats_subgroups <- statestats_subgroups[order(statestats_subgroups$REGION, as.numeric(statestats_subgroups$PCTILE)), ]
  # NAs introduced by coercion
  
  statestats_subgroups$OBJECTID <- paste0(statestats_subgroups$REGION, statestats_subgroups$PCTILE) #1:NROW(statestats_subgroups)
  
  if (length(setdiff(names(statestats_subgroups), names(statestats2.2))) > 0) {
    statestats2 <- merge(
      statestats2.2, 
      statestats_subgroups[,  unique(c("PCTILE", "REGION", setdiff(names(statestats_subgroups), names(statestats2.2))))], 
      all.x = TRUE, all.y = FALSE, 
      by = c("PCTILE", "REGION")
    )
    statestats2$OBJECTID.x <- NULL
    statestats2$OBJECTID.y <- NULL
  } else {
    statestats2 <- statestats2.2
  }
  statestats2 <- statestats2[order(statestats2$REGION, as.numeric(statestats2$PCTILE)), ]
  statestats2$OBJECTID <- 1:NROW(statestats2)
   rownames(statestats2) <- paste0(statestats2$REGION, statestats2$PCTILE) # 1:NROW(statestats2) # they had been named based on combo of ST and PCTILE
  
  # sort cols as sorted in names_d_subgroups_both
  subvars <- intersect(names_d_subgroups_both, names(statestats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(statestats2), subvars)
    statestats2 <- statestats2[ , c(othervars, subvars)]
  }
  statestats2   <- EJAM::metadata_add(statestats2)
  attr(statestats2, "ejscreen_releasedate") <- '2023-08-21'
  statestats2.2 <- statestats2
  # done with state file
 rm(statestats2)
  ################################################ #
  ################################################ #
  rm(morecols, updated, subvars, statestats_subgroups, zerorowperstate, othervars)
  ########################################################## # 
  # now save these within the EJAM package as datasets
  
  
  data.table::setDF(usastats2.2) # keep as data.frame actually in the package
  data.table::setDF(statestats2.2) # keep as data.frame actually
  
  usastats   <- usastats2.2
  statestats <- statestats2.2
  
  rm(usastats2.2, statestats2.2)
  
  setwd(file.path(Sys.getenv("R_USER"), "EJAM")); getwd() # just make sure this is the right one
  
  usastats   <- EJAM::metadata_add(usastats)
  statestats <- EJAM::metadata_add(statestats)
  
  attr(  usastats, 'ejscreen_releasedate') <- "2023-09"
  attr(statestats, 'ejscreen_releasedate') <- "2023-09"
  attr(  usastats, 'download_date') <- Sys.time()
  attr(statestats, 'download_date') <- Sys.time()
  
    attributes(  usastats)[!("row.names" == names(attributes(usastats)))] 
  attributes(statestats)[!("row.names" == names(attributes(statestats)))] 
  
  # fix duplicate name where hisp was in alone and nh versions
  usastats$pcthisp.1 <- NULL
  statestats$pcthisp.1 <- NULL
  usastats$OBJECTID <- NULL
  statestats$OBJECTID <- NULL
  
# fix scaling of percentages of new groups:
    usastats[, unique(c(EJAM::names_d_subgroups_alone, EJAM::names_d_subgroups_nh))] <-   usastats[, unique(c(EJAM::names_d_subgroups_alone, EJAM::names_d_subgroups_nh))]  / 100
  statestats[, unique(c(EJAM::names_d_subgroups_alone, EJAM::names_d_subgroups_nh))] <- statestats[, unique(c(EJAM::names_d_subgroups_alone, EJAM::names_d_subgroups_nh))]  / 100
  
  # COMPARE AUGUST AND SEPTEMBER VERSIONS: 
  # > all.equal(EJAM::usastats, usastats)
  # [1] "Attributes: < Component “download_date”: Mean absolute difference: 8980007 >" "Attributes: < Component “ejscreen_releasedate”: 1 string mismatch >"         
  # [3] "Component “pcthisp”: Mean relative difference: 1.067595e-05"                  "Component “pctnhba”: Mean relative difference: 1.589355e-05"                 
  # [5] "Component “pctnhaa”: Mean relative difference: 1.501667e-05"                  "Component “pctnhaiana”: Mean relative difference: 7.836065e-05"              
  # [7] "Component “pctnhnhpia”: Mean relative difference: 3.172088e-05"               "Component “pctnhotheralone”: Mean relative difference: 6.434336e-05"         
  # [9] "Component “pctnhmulti”: Mean relative difference: 2.125638e-05"               "Component “pctnhwa”: Mean relative difference: 2.685011e-06"                 
  # [11] "Component “pctba”: Mean relative difference: 1.229241e-05"                    "Component “pctaa”: Mean relative difference: 2.621459e-05"                   
  # [13] "Component “pctaiana”: Mean relative difference: 8.697249e-05"                 "Component “pctnhpia”: Mean relative difference: 0.0002228006"                
  # [15] "Component “pctotheralone”: Mean relative difference: 1.192427e-05"            "Component “pctmulti”: Mean relative difference: 1.705527e-05"                
  # [17] "Component “pctwa”: Mean relative difference: 2.403642e-06"                   
  # > all.equal(EJAM::statestats, statestats)
  # [1] "Attributes: < Component “download_date”: Mean absolute difference: 8980007 >" "Attributes: < Component “ejscreen_releasedate”: 1 string mismatch >"         
  # [3] "Component “pcthisp”: Mean relative difference: 0.001028853"                   "Component “pctnhba”: Mean relative difference: 0.000977502"                  
  # [5] "Component “pctnhaa”: Mean relative difference: 0.001942243"                   "Component “pctnhaiana”: Mean relative difference: 0.008011628"               
  # [7] "Component “pctnhnhpia”: Mean relative difference: 0.004812776"                "Component “pctnhotheralone”: Mean relative difference: 0.007710334"          
  # [9] "Component “pctnhmulti”: Mean relative difference: 0.00149952"                 "Component “pctnhwa”: Mean relative difference: 0.000319588"                  
  # [11] "Component “pctba”: Mean relative difference: 0.0009840309"                    "Component “pctaa”: Mean relative difference: 0.001968099"                    
  # [13] "Component “pctaiana”: Mean relative difference: 0.007705694"                  "Component “pctnhpia”: Mean relative difference: 0.004470268"                 
  # [15] "Component “pctotheralone”: Mean relative difference: 0.001997212"             "Component “pctmulti”: Mean relative difference: 0.001217564"                 
  # [17] "Component “pctwa”: Mean relative difference: 0.0002985361"  
  
  usethis::use_data(  usastats, overwrite = T)
  usethis::use_data(statestats, overwrite = T) 
  rm(bg)
  # save.image(file = 'work on usastats and statestats 2023-08-23 end.rda')
  save.image(file = 'work on usastats and statestats 2023-12-12 end.rda')
  
  rm(list = ls())
  
  stop('now need to rebuild EJAM package with those new datasets and push changes')
  
  ################################################ #
# }

