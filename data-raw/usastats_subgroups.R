
# This script was used to add columns to usastats and statestats with info on demographic subgroups

# and fixed the Demog.Index vs VSI.eo issue here too (maybe already changed usastats):
# names(usastats) <- gsub("VSI.eo","Demog.Index",names(usastats))
# names(statestats) <- gsub("VSI.eo","Demog.Index",names(statestats))

# drop the std.dev rows since never used and dropping them again for each indicator in looping use of pctile_from_raw_lookup() is kind of slow.
usastats     <- usastats[usastats$PCTILE   != "std.dev", ]
statestats <- statestats[statestats$PCTILE != "std.dev", ]

############################################################################ #

# TEMPORARILY PUT IN ZEROES FOR THESE 2 VARIABLES UNTIL EJSCREEN PROVIDES THEM FOR DOWNLOAD ####
usastats$lowlifex <- 0
usastats$Demog.Index.Supp <- 0
statestats$lowlifex <- 0 
statestats$Demog.Index.Supp <- 0
usethis::use_data(usastats,overwrite = TRUE)
# ✔ Setting active project to 'C:/Users/mcorrale/R/mysource/EJAM'
# ✔ Saving 'usastats' to 'data/usastats.rda'
# • Document your data (see 'https://r-pkgs.org/data.html')
usethis::use_data(statestats,overwrite = TRUE)
# ✔ Saving 'statestats' to 'data/statestats.rda'
# • Document your data (see 'https://r-pkgs.org/data.html')
############################################################################# # 


# Create percentile lookup tables for extra variables like demographic race/ethnicity subgroups
bg <- data.table::copy(EJAM::blockgroupstats)
bg <- data.table::setDF(bg)
# bg = ejanalysis::addFIPScomponents(bg, fipscolname = "bgfips",  clean = FALSE)
bg$FIPS.ST <- substr(bg$bgfips,1,2)
bg$ST <- EJAM::stateinfo$ST[match(bg$FIPS.ST, EJAM::stateinfo$FIPS.ST)]

   usastats_subgroups   <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups])
   usastats_subgroups <- rbind(0, usastats_subgroups); usastats_subgroups$PCTILE[1] <- 0
   usastats_subgroups[1, c("OBJECTID", "REGION")] <- c(0,"USA")
   
   statestats_subgroups <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups], zone.vector = bg$ST)
    zerorowperstate <- data.frame(
     OBJECTID=0,
     REGION=unique(statestats_subgroups$REGION),
     PCTILE=0,
     pctnhwa=0, pcthisp=0, pctnhba=0, pctnhaa=0, pctnhaiana=0, pctnhnhpia=0, pctnhotheralone=0,pctnhmulti=0
     )
   statestats_subgroups <- rbind(statestats_subgroups, zerorowperstate)
   statestats_subgroups <- statestats_subgroups[order(statestats_subgroups$REGION, as.numeric(statestats_subgroups$PCTILE)), ]
   statestats_subgroups$OBJECTID <- 1:NROW(statestats_subgroups)
   # > dim(usastats)
   # [1] 103  35
   # > dim(usastats_subgroups)
   # [1] 103  11     
   
   usastats2 <- cbind(usastats, usastats_subgroups[, setdiff(names(usastats_subgroups), names(usastats))  ])
 
  statestats2 <- merge(statestats, statestats_subgroups, all.x=TRUE, all.y=FALSE, by= c("PCTILE", "REGION"))
statestats2$OBJECTID.x <- NULL
statestats2$OBJECTID.y <- NULL
statestats2 <- statestats2[order(statestats2$REGION, as.numeric(statestats2$PCTILE)), ]
statestats2$OBJECTID <- 1:NROW(statestats2)
rownames(statestats2) <- 1:NROW(statestats2)
statestats2 <- statestats2[, c(names(statestats), names_d_subgroups)]

statestats2   <- EJAM::metadata_add(statestats2)
usastats2     <- EJAM::metadata_add(usastats2)

usastats <- usastats2
data.table::setDF(usastats) # keep as data.frame actually

usethis::use_data(usastats, overwrite = TRUE)

statestats <- statestats2
data.table::setDF(statestats) # keep as data.frame actually

usethis::use_data(statestats, overwrite = TRUE)
