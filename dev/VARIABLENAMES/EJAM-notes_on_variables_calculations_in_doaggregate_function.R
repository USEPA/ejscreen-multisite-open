 
# To aggregate in a buffer, there are
# 
# 1) counts to be summed (see list below)
# 2) environmental scores to be population weighted as averages (see list below)
# 3) variables to be calculated using formulas (e.g. %lowincome) based on sums of counts, generally (formulas below or in ejscreenformulas.rda file here)
# 4) percentiles to be looked up in lookup tables, via code and data lookup tables.
 
##############################################
# see   EJAM::doaggregate.R
 
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
   
    ###################################################################################################
    
    # These must be calculated after aggregating count variables and using those at site id level. 
    # e.g. Use ejscreen package file ejscreenformulas$formula to calculate these.
  

    # popmeancols <- c(ejscreen package file names.e, ejscreen package file names.ej)
    # or to avoid depending on ejscreen package, 
    # dput(c(ejscreen package file names.e, ejscreen package file names.ej) )
    
      
      # QUESTION... DO WE CALCULATE THE STATE-FOCUSED EJ INDEXES USING THE STATE AVERAGE DEMOG?? ejscreen was not doing that as of mid 2022 even though it should have been?, 
      # & ASSIGN STATE PERCENTILES OF EJ INDEXES AND THEN ROLL UP IN BUFFER BY FINDING POPWTD MEAN STATE PERCENTILE???
      #  OR DO WE ASSUME THE ENTIRE BUFFER IS MAINLY OR ALL IN ONE STATE AND LOOK UP THE RAW EJ INDEX IN THAT STATE'S LOOKUP TO ASSIGN THE PERCENTILE. THE LATTER, I THINK. 
  
    # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
    # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
    #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
    # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
    # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
    # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
 
 
