
# TO BE CONTINUED....  NOT DONE WITH THIS YET....  

# Create percentile lookup tables for extra variables like demographic race/ethnicity subgroups
bg <- data.table::copy(EJAM::blockgroupstats)
bg = setDF(bg)
bg = addFIPScomponents(bg, fipscolname = "bgfips",  clean = FALSE)


   usastats_subgroups   <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups])
   usastats_subgroups <- rbind(NA, usastats_subgroups); usastats_subgroups$PCTILE[1] <- 0
   # but...
   statestats_subgroups <- write_pctiles_lookup(data.frame(bg)[ , names_d_subgroups], zone.vector = bg$ST)
   # not as easy to create 0th row for each state here...
   # not as easy to merge this with existing, either....  to be completed later..,, 
   
   # THOSE FAIL TO CREATE THE ZERO PERCENTILE ROW:    **********   FIX METHOD TO MATCH CURRENT EJScreen method, or fit rows in with merge?
   
   # > dim(usastats)
   # [1] 103  35
   # > dim(usastats_subgroups)
   # [1] 102  11     MISSING THE PCTILE == 0  ROW
   
 
   usastats2 <- cbind(usastats, usastats_subgroups[, setdiff(names(usastats_subgroups), names(usastats))  ])
 
   # not tried yet:
  statestats2 <- merge(statestats, statestats_subgroups, all.x=TRUE, all.y=FALSE, by= c("PCTILE", "REGION"))

