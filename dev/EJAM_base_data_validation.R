# Author: Parker Malek, Abt Associates
# script for validating EJAM base datasets
# Base datasets include   quaddata,  frs,   blockwts, EJAM::blockgroupstats, EJAM::statestats, EJAM::usastats


#  quaddata

#check for existing   quaddata, make sure localtree is being correctly loaded
out <- tryCatch( {
localtree <- SearchTrees::createTree(
  quaddata, treeType = "quad", dataType = "point"
)
},error=function(cond) {
  message(paste("Check for existing  quaddata"))
  message(cond)
  # Choose a return value in case of error
  return(NA)
})

#check for expected column names
expected_columns_quad <- c("BLOCK_X","BLOCK_Z","BLOCK_Y","blockid")

if(any(colnames( quaddata) != expected_columns_quad)){
  print(paste0("Unexpected columns ", expected_columns_quad[colnames( quaddata) != expected_columns_quad]))
} else {
  print("Expected columns exist for   quaddata)")
}



 #frs

#Used to link frs with lat/lons for processing

expected_columns_frs <- c("lat","lon","REGISTRY_ID","PRIMARY_NAME","NAICS","PGM_SYS_ACRNMS")

if(any(colnames( frs) != expected_columns_frs)){
  print(paste0("Unexpected columns ",expected_columns_frs[colnames( frs) != expected_columns_frs]))
} else {
  print("Expected columns exist for  frs)")
}


 
#  blockwts

#used to weight block groups for doaggregate site/overall summary output

expected_columns_bgw <- c("blockid","bgid","blockwt")

if(any(colnames( blockwts) != expected_columns_bgw)){
  print(paste0("Unexpected columns ",expected_columns_frs[colnames( blockwts) != expected_columns_bgw]))
} else {
  print("Expected columns exist for  blockwts)")
}


#EJAM::blockgroupstats

#EJSCREEN stats by block group


#merge blockgroupstats onto EJAMejscreendata::EJSCREEN_Full_with_AS_CNMI_GU_VI and confirm correct cleaning

mp_islands <- c("AS","GU","MP","VI") #expected missing census state/territory abbreviations

ejscreen <- EJAMejscreendata::EJSCREEN_Full_with_AS_CNMI_GU_VI
bgstat <- EJAM::blockgroupstats
bgstat$merged <- TRUE

bg_ejscreen_merge <- merge(ejscreen,bgstat,by.x = "ID",by.y = "bgfips",all=T)

bg_ejscreen_nomerge <- bg_ejscreen_merge[is.na(bg_ejscreen_merge$merged),]

bg_ejscreen_merge <- bg_ejscreen_merge[!is.na(bg_ejscreen_merge$merged),]

missing_states <- unique(bg_ejscreen_nomerge$ST_ABBREV)

if(!all(missing_states %in% mp_islands)){
  print(paste0("Unexpected missing states/territories in blockgroupstats ",missing_states[missing_states %in% mp_islands]))
} else {
  print("Expected states/territories present in blockgroupstats")
}


#confirm 242,335 rows (raw EJSCREEN - terroritory cleaning)
if(nrow(EJAM::blockgroupstats) != nrow(bg_ejscreen_merge)){
  print("Unexpected row count for blockgroupstats dataset. Please check/update EJSCREEN_Full_with_AS_CNMI_GU_VI at")
} else {
  print("Row counts in blockgroupstats are expected numbers")
}


#EJAM::statestats

#lookup stats by states *used in doaggregate for state percentile calculations*

#confirm total available states in data (52), can be updated once other territories are incorporated

expected_states <-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                    "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                    "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY","PR")# "AS", "GU", "MP","VI",UM" , #### Territories U.S. Minor Outlying Islands # "US"

statestats_current <- unique(EJAM::statestats$REGION)
if(!all(statestats_current %in% expected_states)){
  print(paste0("Missing state/territory ",statestats_current[!(statestats_current %in% expected_states)]))
} else {
  print("Expected states/territories present in statestats")
}

#EJAM::usastats

#lookup stats by states *used in doaggregate for national percentile calculations*

#confirm 103 rows (one for each percentile from 0-100 plus mean and std)
if(nrow(EJAM::usastats) != 103){
  print("Unexpected row count for blockgroupstats dataset")
} else {
  print("Expected percentile/statistics present in usastats")
}
