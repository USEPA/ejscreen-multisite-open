#' create lookup table of percentiles 0 to 100 and mean for each indicator by State or USA total
#'
#' @param x data.frame with numeric data. Each column will be examined to calculate 
#'   mean, sd, and percentiles, for each zone
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
  }
  wtd.pctiles.exact <-  function(x, wts=NULL, na.rm=TRUE, type="i/n", probs=(1:100)/100) {
    #  PERCENTILES, WEIGHTED, SO DISTRIBUTION OVER PEOPLE NOT PLACES
    cbind(Hmisc::wtd.quantile(x, wts, type=type, probs=probs, na.rm=na.rm))
  }
  
  if (is.null(zone.vector)) {
    r <- data.frame(                     sapply(mydf, function(x) pctiles.exact(x)))
    r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
    r <- rbind(r, t(data.frame(std.dev = sapply(mydf, function(x) sd(  x, na.rm = TRUE)))))
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
        r[[i]] <- rbind(r[[i]], t(data.frame(std.dev = sapply(mydf[zone.vector==z,  ], function(x) sd(x, na.rm = TRUE)))))
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean','std.dev'
      } else {
        
        # ZONES AND WEIGHTS ####
        
        r[[i]] = data.frame(sapply(mydf[zone.vector==z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector==z]) ) )
        r[[i]] = rbind(r[[i]], t(data.frame(mean=sapply(mydf[zone.vector==z,  ], function(x) Hmisc::wtd.mean(x, wts[zone.vector==z], na.rm=TRUE) ) ) ))
        r[[i]] = rbind(r[[i]], t(data.frame(std.dev=sapply(mydf[zone.vector==z,  ], function(x) sqrt(Hmisc::wtd.var(x, wts[zone.vector==z], na.rm=TRUE) ) )) ))
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

