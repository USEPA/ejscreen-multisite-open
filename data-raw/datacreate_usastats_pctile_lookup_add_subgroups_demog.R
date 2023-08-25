############################################################################ #

# This script was used for 2022 version 2.1 EJScreen,
#  and then can be for  2023 version 2.2 IF get blockgroup data on subgroups 
#  (and we know lookup tables are
#  missing the subgroups  ).

# This script is
# to add columns to usastats and statestats with info on demographic subgroups
# and to sort columns the same way they are ordered in names_d_subgroups


############################################################################# # 
# to add percentile lookup info on demog subgroups, we need that info from blockgroupstats

    
 bg <- data.table::copy(EJAM::blockgroupstats)
  bg <- data.table::setDF(bg)

  if (  0 == length(intersect(names_d_subgroups, names(bg)))) {
    stop('need subgroup data to create lookup percentile info for subgroups')
  } else {
    if (0 == all(bg[ , names_d_subgroups])) {
      #placeholder was there but all 0
      stop('need subgroup data to create lookup percentile info for subgroups')
    }
  }
   
  
# need ST column in blockgroupstats to create the statestats lookup table of demog subgroup info,
# so add FIPS.ST and ST columns to blockgroupstats if not already there

if (!("ST" %in% names(bg))) {
  # bg = ejanalysis package file addFIPScomponents(bg, fipscolname = "bgfips",  clean = FALSE)
  # bg$FIPS.ST <- substr(bg$bgfips,1,2)
  # bg$ST <- EJAM::stateinfo$ST[match(bg$FIPS.ST, EJAM::stateinfo$FIPS.ST)]
  bg$ST <- bg$ST_ABBREV
}
############################################################################# # 

updated = FALSE

# drop the std.dev rows since never used and dropping them again for each indicator in looping use of pctile_from_raw_lookup() is kind of slow.
if (("std.dev" %in% (  usastats$PCTILE))) {  usastats <-   usastats[  usastats$PCTILE != "std.dev", ] ; updated = TRUE}
if (("std.dev" %in% (statestats$PCTILE))) {statestats <- statestats[statestats$PCTILE != "std.dev", ] ; updated = TRUE}

############################################################################ #
# if missing, TEMPORARILY PUT IN ZEROES FOR THESE VARIABLES UNTIL get DOWNLOAD ####
# 
# if (!('rsei'                 %in% names(  usastats))) {  usastats$rsei <- 0; updated = TRUE}
# if (!('EJ.DISPARITY.rsei.eo' %in% names(  usastats))) {  usastats$EJ.DISPARITY.rsei.eo <- 0; updated = TRUE}
# if (!('rsei'                 %in% names(statestats))) {statestats$rsei <- 0; updated = TRUE}
# if (!('EJ.DISPARITY.rsei.eo' %in% names(statestats))) {statestats$EJ.DISPARITY.rsei.eo <- 0; updated = TRUE}
# 
# if (!('lowlifex'         %in% names(  usastats))) {  usastats$lowlifex <- 0; updated = TRUE}
# if (!('Demog.Index.Supp' %in% names(  usastats))) {  usastats$Demog.Index.Supp <- 0; updated = TRUE}
# if (!('lowlifex'         %in% names(statestats))) {statestats$lowlifex <- 0; updated = TRUE}
# if (!('Demog.Index.Supp' %in% names(statestats))) {statestats$Demog.Index.Supp <- 0; updated = TRUE}
# 
# if (updated) { # if either got updated, just replace both
#   usethis::use_data(  usastats, overwrite = TRUE)
#   usethis::use_data(statestats, overwrite = TRUE)
# }
# 

################################################ #
# Add to demog subgroups to percentile lookup tables #### 

## CREATE USA LOOKUP ####

if (all(usastats[,names_d_subgroups] == 0)  | any(!(names_d_subgroups %in% names(usastats))))  {
  
  usastats_subgroups   <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups])
  usastats_subgroups <- rbind(0, usastats_subgroups); usastats_subgroups$PCTILE[1] <- 0
  usastats_subgroups[1, c("OBJECTID", "REGION")] <- c(0, "USA")
  
  usastats2 <- cbind(usastats, usastats_subgroups[ , setdiff(names(usastats_subgroups), names(usastats))  ])
  
  # sort cols as sorted in names_d_subgroups
  subvars <- intersect(names_d_subgroups, names(usastats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(usastats2), subvars)
    usastats2 <- usastats[ , c(othervars, subvars)]
  }
  
  usastats2     <- EJAM::metadata_add(usastats2)
  usastats <- usastats2
  data.table::setDF(usastats) # keep as data.frame actually
  
  
  usethis::use_data(usastats, overwrite = TRUE)
  
} # done with usastats


##  CREATE STATESTATS LOOKUP TABLE ####

if (all(statestats[,names_d_subgroups] == 0)  | any(!(names_d_subgroups %in% names(statestats)))) {
  
  statestats_subgroups <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups], zone.vector = bg$ST)
  
  # names(statestats_subgroups)
  morecols = data.frame(as.list(rep(0,length(names_d_subgroups))))
  names(morecols) <- names_d_subgroups
  zerorowperstate <- data.frame(
    OBJECTID=0,
    REGION=unique(statestats_subgroups$REGION),
    PCTILE=0, 
    morecols
  )
  statestats_subgroups <- rbind(statestats_subgroups, zerorowperstate)
  
  statestats_subgroups <- statestats_subgroups[order(statestats_subgroups$REGION, as.numeric(statestats_subgroups$PCTILE)), ]
  # NAs introduced by coercion
  
  statestats_subgroups$OBJECTID <- 1:NROW(statestats_subgroups)
  # dim(usastats)
  # [1] 102  35
  # > dim(usastats_subgroups)
  # [1] 102  11     
  
  if (length(setdiff(names(statestats_subgroups), names(statestats))) > 0) {
    statestats2 <- merge(
      statestats, 
      statestats_subgroups[, setdiff(names(statestats_subgroups), names(statestats))], 
      all.x=TRUE, all.y=FALSE, 
      by= c("PCTILE", "REGION")
    )
    statestats2$OBJECTID.x <- NULL
    statestats2$OBJECTID.y <- NULL
  } else {
    statestats2 <- statestats
  }
  statestats2 <- statestats2[order(statestats2$REGION, as.numeric(statestats2$PCTILE)), ]
  statestats2$OBJECTID <- 1:NROW(statestats2)
  rownames(statestats2) <- 1:NROW(statestats2)
  
  # sort cols as sorted in names_d_subgroups
  subvars <- intersect(names_d_subgroups, names(statestats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(statestats2), subvars)
    statestats2 <- statestats[ , c(othervars, subvars)]
  }
  statestats2   <- EJAM::metadata_add(statestats2)
  statestats <- statestats2
  data.table::setDF(statestats) # keep as data.frame actually
  
  
  usethis::use_data(statestats, overwrite = TRUE)
  
}

