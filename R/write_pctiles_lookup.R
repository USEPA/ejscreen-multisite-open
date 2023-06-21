#' create lookup table of percentiles 0 to 100 and mean for each indicator by State or USA total
#' @details EJScreen assigns each indicator in each block group a percentile value via python script, using 
#' <https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.percentileofscore.html>
#' 
#'   The way the python function is used as of 2023 is that percentileofscore is 80% if
#'   
#'   80% of all indicator values (statewide or nationwide, depending on the type being calculated) 
#'   
#'   are less than (NOT equal to) the indicator value 
#'   
#'   in the specified block group (since kind="strict").
#'   
#'   The percentile recorded in the EJScreen dataset is the floor of that, 
#'   
#'   meaning if the 81.9% of values are less than x, the percentile is reported as 81.
#'   
#'   The EJScreen python script used to create percentile lookup tables is in a file 
#' 
#'   called cal_statepctile_0222.py and the key lines of code and functions it uses are 
#'   
#'   pctile = math.floor(stats.percentileofscore(barray, indicatorscore, kind="strict"))
#'   
#'   binvalue = getBinvalue(pctile)
#'   
#'   and 
#'   
#'   def getBinvalue(pct):
#'   
#' if pct is None:
#'   return 0
#' else:
#'     if pct >= 95:
#'     return 11
#' elif pct >= 90 and pct < 95:
#'   return 10
#' elif pct >= 80 and pct < 90:
#'   return 9
#' elif pct >= 70 and pct < 80:
#'   return 8
#' elif pct >= 60 and pct < 70:
#'   return 7
#' elif pct >= 50 and pct < 60:
#'   return 6
#' elif pct >= 40 and pct < 50:
#'   return 5
#' elif pct >= 30 and pct < 40:
#'   return 4
#' elif pct >= 20 and pct < 30:
#'   return 3
#' elif pct >= 10 and pct < 20:
#'   return 2
#' else:
#'   return 1
#'   
#' @param x data.frame with numeric data. Each column will be examined to calculate 
#'   mean,   and percentiles, for each zone
#' @param zone.vector optional names of states or regions, for example. same length as wts, or rows in mydf
#' @param zoneOverallName optional. Default is USA.
#' @param wts not used in EJScreen percentiles anymore
#'
#' @export
#'
write_pctiles_lookup <- function(x, zone.vector=NULL, zoneOverallName='USA', wts=NULL) {
  mydf <- x
  
  pctiles.exact <- function(x) {
    cbind(quantile(x, type=1, probs=(1:100)/100, na.rm=TRUE) ) 
    # actually this should be written to assign the floor of exact % of scores that are < given bg score
    # floor(sum(x < thisx)/length(x))
  }
  
  wtd.pctiles.exact <-  function(x, wts=NULL, na.rm=TRUE, type="i/n", probs=(1:100)/100) {
    #  PERCENTILES, WEIGHTED, SO DISTRIBUTION OVER PEOPLE NOT PLACES
    cbind(Hmisc::wtd.quantile(x, wts, type=type, probs=probs, na.rm=na.rm))
  }
  
  if (is.null(zone.vector)) {
    r <- data.frame(                     sapply(mydf, function(x) pctiles.exact(x)))
    
    r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
    #   STOP INCLUDING STANDARD DEVIATION HERE SINCE WE NEVER USE IT
    # r <- rbind(r, t(data.frame(std.dev = sapply(mydf, function(x) sd(  x, na.rm = TRUE)))))
    r$REGION <- "USA" # zoneOverallName
    r$PCTILE <- rownames(r) # 1:100,'mean','std.dev'
  } else {
    
    r <- list()
    for (i in 1:length(unique(zone.vector))) {
      z <- unique(zone.vector)[i]
      if (is.null(wts)) {
        
        # ZONES BUT NO WEIGHTS ####
        
        r[[i]] <- data.frame(sapply(mydf[zone.vector==z,  ], function(x) pctiles.exact(x)))
        
        r[[i]] <- rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector==z,  ], function(x) mean(x, na.rm = TRUE)))))
        # r[[i]] <- rbind(r[[i]], t(data.frame(std.dev = sapply(mydf[zone.vector==z,  ], function(x) sd(x, na.rm = TRUE)))))
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean','std.dev'
      } else {
        
        # ZONES AND WEIGHTS ####
        
        r[[i]] = data.frame(sapply(mydf[zone.vector==z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector==z]) ) )
        r[[i]] = rbind(r[[i]], t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) Hmisc::wtd.mean(x, wts[zone.vector==z], na.rm=TRUE) ) ) ))
        # r[[i]] = rbind(r[[i]], t(data.frame(std.dev=sapply(mydf[zone.vector==z,  ], function(x) sqrt(Hmisc::wtd.var(x, wts[zone.vector==z], na.rm=TRUE) ) )) ))
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean','std.dev'
      }
    }
    # ZONE LOOP DONE
    r <- do.call(rbind, r)
    
  }
  
  r <- data.frame(
    OBJECTID = 1:NROW(r),
    REGION = r$REGION,
    PCTILE = r$PCTILE,
    r[ , !(colnames(r) %in% c('REGION', 'PCTILE'))],
    stringsAsFactors = FALSE
  )
  rownames(r) <- NULL
  invisible(r)
}

