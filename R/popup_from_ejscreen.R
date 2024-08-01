
#' Map popups - Create the popup text for maps of EJ results
#' 
#' @description This creates the HTML text that appears in map popups.
#' @details Popup shows up in a window when you 
#'   click on a site on the map, when viewing the results of EJ analysis of 
#'   each site. 
#'   
#'   THIS IS CURRENTLY HARD CODED TO USE EJScreen VARIABLE NAMES.
#'   
#'   It provides raw scores (but not for EJ indexes) and US and State percentiles if available:
#'   
#'   - some site id info fields if found
#'   - latitude, longitude, and size of area around site
#'   - Demographic Indicators like population count, Demographic Indicator, etc.
#'   - Environmental Indicators
#'   - EJ Indexes
#'   - web link(s) to map or report
#'
#' @param out raw data in data.frame form, with results of EJ buffer analysis
#' @param linkcolname Name of one column in the table that has links to some URL
#' @param linkcolname2 Another like linkcolname
#' @param linkcolname3 another 
#' @param verbose TRUE or FALSE, can see more details reported when function is used.
#'
#' @return HTML ready to be used for map popups
#' @examples \dontrun{
#'   out <- testoutput_ejscreenapi_plus_50
#'   x <- popup_from_ejscreen(out)
#'   popup_print(x[1])
#'   mapfastej(out)
#' }
#' 
#' @keywords internal
#' @export
#' 
popup_from_ejscreen <- function(out, linkcolname='EJScreen Report', linkcolname2='EJScreen Map', linkcolname3='EJScreenACS', verbose=FALSE) {
  
  if (data.table::is.data.table(out)) {out <- data.table::copy(out); data.table::setDF(out)}
  
  ############################################ #
  # SPECIFY indicators/VARIABLE NAMES  ####
  
  names.d.api        <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d"] #, 
    # map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d_subgroups"]      # disabled because had been coded to require pctiles and raw and friendly all be the same length.
  )   
  names.d.nice.api   <- c(
    # map_headernames$names_friendly[map_headernames$varlist == "names_d"]  #, or   
    fixcolnames(names.d.api, 'r', 'shortlabel')
    # map_headernames$names_friendly[map_headernames$varlist == "names_d_subgroups"] 
  )

    names.d.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d_pctile"]  #  , 
    # map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d_subgroups_pctile"]   # ejscreen 2.2 does not provide the percentiles of subgroups
  )
  names.d.state.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d_state_pctile"] #, 
    # map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_d_subgroups_state_pctile"]   # ejscreen 2.2 does not provide the percentiles of subgroups
  )  
  
  
  names.e.api       <- map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_e"]  # EJAM ::names_e
  # names.e.nice.api   <- map_headernames$names_friendly[map_headernames$varlist == "names_e"] 
  names.e.nice.api   <-   fixcolnames(names.e.api, 'r', 'shortlabel')
  names.e.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_e_pctile"]
  )
  names.e.state.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_e_state_pctile"] 
  )
  
  # EJ:  include the us, normal and supplemental, all in this one variable list:
  names.ej.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_ej_pctile"],
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_ej_supp_pctile"]
  )
  names.ej.state.pctile.api <- c(
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_ej_state_pctile"],
    map_headernames$newnames_ejscreenapi[map_headernames$varlist == "names_ej_supp_state_pctile"] 
  )
  names.ej.pctile.api_USORSTATE <- fixcolnames(names.ej.pctile.api, 'r', 'shortlabel')
  # names.ej.pctile.api_USORSTATE
  names.ej.pctile.api_USORSTATE <- gsub("(.*)( \\(.*\\%ile.*)$", "\\1", names.ej.pctile.api_USORSTATE) # removes " (US%ile)" 
  # names.ej.pctile.api_USORSTATE
  
  # names.ej.pctile.nice.api <- c( # do we need this ?? # ???????????????
  #   map_headernames$names_friendly[map_headernames$varlist == "names_ej_pctile"],
  #   map_headernames$names_friendly[map_headernames$varlist == "names_ej_supp_pctile"]
  # )
  # map_headernames$names_friendly[map_headernames$varlist == "names_ej_state_pctile"],
  # map_headernames$names_friendly[map_headernames$varlist == "names_ej_supp_state_pctile"] 
  # 
  
  ############################################ #
  # how many significant digits to report?
  
  esigfigs <- map_headernames[ "" != (map_headernames$sigfigs), c("sigfigs", 'newnames_ejscreenapi')]
  names(esigfigs) <- c("sigfigs", "evar")
  esigfigs.api <- esigfigs
  # esigfigs.api <- structure(list(
  #   sigfigs = c(3, 3,  2,   2, 3,
  #               2, 
  #               2, 2, 2, 2, 2, 2), 
  #   evar = names.e.api), 
  #   row.names = c(NA, 12L), 
  #   class = "data.frame")
  
  ############################################ #
  # text explaining units of measure like ug/m3
  
  popupunits <- map_headernames[ "" != (map_headernames$units), c("units", 'newnames_ejscreenapi')]
  names(popupunits) <- c("units","evar")
  popupunits.api <- popupunits
  #   popupunits.api <- structure(list(
  #   evar = names.e.api, 
  #   units = c("ug/m3", "ppb", "lifetime risk per million", "ratio", "ug/m3", 
  #             "fraction built pre-1960",
  #             "daily vehicles/meters distance", "sites/km distance", "facilities/km distance", "facilities/km distance", "facilities/km distance", "tox-weighted score/distance")
  # ), row.names = c(NA, 12L), class = "data.frame")
  # 
  # cbind(names.e.api, names.e.pctile.api,names.e.nice.api,esigfigs.api,popupunits.api)
  
  ############################################ #
  # ensure expected colnames are in outputs; replace empty values with NA, or this crashes
  
  colstofix <- c(
    names.e.api, 
    names.e.pctile.api, names.e.state.pctile.api,
    names.d.api, 
    names.d.pctile.api, names.d.state.pctile.api, # does not include  D subgroups
    names.ej.pctile.api, names.ej.state.pctile.api # includes  supplementary EJ indexes too
  )
  if (verbose) {
    # if (any(!(colstofix %in% names(out) ))) {
    # warning('column names in out are not what were expected')
    cat('\n')
    cat('These column names were expected but not found in outputs:\n\n'); print(setdiff(colstofix, names(out)))
    cat('\n')
    cat('These column names were in outputs but not expected by code:\n\n'); print(setdiff(names(out), colstofix))
    # }
  }
  colstofix <- intersect(colstofix, names(out))
  out[, colstofix] <- sapply(out[, colstofix], function(mycol) {ifelse(mycol == '', NA, mycol)})
  # actually we probably need to remove from consideration all columns that are not in out...
  # but what if there are differences between names.e.api, the pctile version of that, the nice names version of that, etc.
  # ie their lengths differ. the code is not robust to that. 
  
  ############################################ #
  # define functions to write map popup text with given indicator names, signif digits
  # but this may get replaced by decimal places not sigfigs, at least for EJAM Excel formatting, since that seems easier to implement in that code.
  signifarray.api <- function(dat, digits = 6) {
    if (!(is.data.frame(dat))) {
      dat <- as.data.frame(dat)
    }
    y <- mapply(FUN = signif, x = dat, digits = digits)
    return(y)
  }
  ejscreensignifarray.api <- function(dat, digits = 'ejscreen') {
    if (!missing(digits)) {
      digits <- as.numeric(esigfigs.api[match(colnames(dat), esigfigs.api$evar), 'sigfigs'])
    } 
    return( signifarray.api(dat = dat, digits = digits))
  }
  pctileAsText <- function(x) {
    # note: x should be a vector or data.frame of percentiles from 0 to 100, not 0 to 1
    result <-
      sapply(x, function(z) {
        paste(floor(z), '%ile', sep = '')
      })
    result[is.na(x)] <- NA
    return(result)
  }
  
  ############################################################################# #  
  
  make.popup.d.api <- function(d, 
                               pctile,   state.pctile, 
                               prefix = 'pctile.text.',  # state.prefix = 'state.pctile.text.',
                               basenames = colnames(d)) {
    
    # function to MAKE THE POPUP TEXT FOR JUST THE DEMOGRAPHIC INDICATORS ####
    
    # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
    # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
    # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
    
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(d)
    }
    
    ptext <- function(x, y.us, y.st) {
      paste(round(100 * x), '% (', pctileAsText(y.us), ' in US, ', pctileAsText(y.st),' in State)', sep = '')
    }
    # > ptext(0.5, 60, 63)
    # [1] "50% (60%ile in US, 63%ile in State)"
    
    x <-
      mapply(
        FUN = ptext,
        d,
        pctile, 
        state.pctile
      )
    
    if (NCOL(x) == 1) {x <- as.data.frame(as.list(x)) } else {
      x <- data.frame(x, stringsAsFactors = FALSE) # as.data.frame(as.list(x)) # need to confirm this works for case where only 1 valid row (site)
    }
    
    # this is a bit confusing since it will be columns called pctile.pm but actually contain value, us pctile, and state pctile
    colnames(x) <- paste(prefix, basenames, sep = '') 
    
    x[is.na(d)] <- NA
    rownames(x) <- rownames(d)
    return(x)
  }
  ############################################################################# #  
  
  make.popup.ej.api <- function(pctile, state.pctile, labels) {
    
    ## pctile and state.pctile should be named lists of values where names are rnames like state.pctile.EJ.DISPARITY.pm.supp 
    #        pctile <- 1:100; names(pctile) <- c(names_ej_pctile,  names_ej_supp_pctile)
    #  state.pctile <- 1:100; names(pctile) <- c(names_ej_state_pctile, names_ej_supp_state_pctile)
    # make.popup.ej.api(pctile = pctile, state.pctile = state.pctile)
    
    # function to MAKE THE POPUP TEXT FOR JUST EJ INDEXES
    # (for us and state, for either regular or suppl, for vectors of named values) ####
    
    if (missing(labels)) {
      names.ej.pctile.api_USORSTATE_here <- fixcolnames(names(pctile), 'r', 'shortlabel')
      names.ej.pctile.api_USORSTATE_here <- gsub("(.*)( \\(.*\\%ile.*)$", "\\1", names.ej.pctile.api_USORSTATE_here) # removes " (US%ile)" 
      labels <- names.ej.pctile.api_USORSTATE_here

      ### too long like this:
      # labels <- gsub("^.*percentile for ", "", fixcolnames(
      #   ## c( names(pctile), names(state.pctile)), ## do not need to repeat since US and State merged into 1 row for each index
      #   names(pctile),
      #   'r',  'long'))
    }
    
    ptext_ej <- function(y.us, y.st) {
      paste(pctileAsText(y.us), ' in US, ', pctileAsText(y.st),' in State', sep = '')
    }
    # > ptext_ej(60, 63)
    # [1] "60%ile in US, 63%ile in State"
    
    x <- mapply(
      FUN = ptext_ej,
      pctile, state.pctile
    )
    
    if (NCOL(x) == 1) {x <- as.data.frame(as.list(x)) } else {
      x <- data.frame(x, stringsAsFactors = FALSE)
    }
    names(x) <- labels
    return(x)
  }
  ############################################################################# #  
  
  make.popup.e.api <- function(e,
                               pctile, state.pctile,
                               prefix = 'pctile.text.',
                               basenames = colnames(e),
                               units,
                               sigfigs) {
    
    # function to MAKE THE POPUP TEXT FOR JUST THE ENVT INDICATORS ####
    
    # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
    # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
    # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(e)
    } 
    if (missing(sigfigs)) {
      sigfigs <- esigfigs.api$sigfigs[match(basenames, esigfigs.api$evar)]
      sigfigs[is.na(sigfigs)] <- 2
    }
    
    e <-
      data.frame(ejscreensignifarray.api(e, digits = as.numeric(sigfigs)), stringsAsFactors = FALSE)
    #   print(e)
    #   print(str(e))
    
    if (missing(units)) {
      units <- popupunits.api$units[match(basenames, popupunits.api$evar)]
      units[is.na(units)] <- ''
    }
    
    if (NCOL(e) == 1) {e <- data.frame(t(e)); rownames(e) <- NULL}
    x <-
      mapply(
        FUN = function(x, u, y.us, y.st) {
          paste(x, ' ', u, ' (', pctileAsText(y.us), ' in US, ', pctileAsText(y.st), ' in State)', sep = '')
        },
        e,
        units,
        pctile, 
        state.pctile
      )
    
    if (NCOL(x) == 1) {x <- as.data.frame(as.list(x)) } else {
      x <- data.frame(x, stringsAsFactors = FALSE) # as.data.frame(as.list(x)) # need to confirm this works for case where only 1 valid row (site)
    }
    
    # this is a bit confusing since it will be columns called pctile.pm but actually contain value, us pctile, and state pctile
    colnames(x) <- paste(prefix, basenames, sep = '') 
    
    x[is.na(e)] <- NA
    rownames(x) <- rownames(e)
    return(x)
  }
  ############################################################################# #  
  ############################################################################# #  
  
  
  # USE THOSE FUNCTIONS TO CREATE POPUPS ####
  
  # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
  # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
  # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
  # handle case where some of names.d.api are not in out, and same for other variables
  # assume that pctile and nice versions are not missing if the base version is here!
  dok <- which(names.d.api %in% names(out)) 
  eok <- which(names.e.api %in% names(out))
  pok <- which(names.ej.pctile.api %in% names(out))
  names.d.api <- names.d.api[dok] 
  
  if (length(dok) == 0) {
    warning('none of names.d.api were found in out')
    poptext.d <- NULL
  } else {
    names.d.nice.api <- names.d.nice.api[dok]
    names.d.pctile.api <- names.d.pctile.api[names.d.pctile.api %in% names(out)] #  there are state and US ones, so 2x as many as there are names.d.api
    names.d.state.pctile.api <- names.d.state.pctile.api[names.d.state.pctile.api %in% names(out)] # there are state and US ones, so 2x as many as there are names.d.api
    poptext.d <-  make.popup.d.api(d = out[, names.d.api] / 100, pctile = out[, names.d.pctile.api], state.pctile = out[, names.d.state.pctile.api], prefix = '') # includes state pctiles too, now
    names(poptext.d) <- names.d.nice.api
  }
  
  if (length(eok) == 0) {
    warning('none of names.e.api were found in out')
    poptext.e <- NULL
  } else {
    names.e.api        <- names.e.api[eok]
    names.e.nice.api   <- names.e.nice.api[eok]
    names.e.pctile.api <- names.e.pctile.api[names.e.pctile.api %in% names(out)] #  there are state and US ones, so 2x as many as there are names.d.api
    names.e.state.pctile.api <- names.e.state.pctile.api[names.e.state.pctile.api %in% names(out)] #  there are state and US ones, so 2x as many as there are names.d.api
    poptext.e <-  make.popup.e.api(
      e = out[, names.e.api],
      pctile = out[, names.e.pctile.api], 
      state.pctile = out[, names.e.state.pctile.api], 
      prefix = ''
      ) # includes state pctiles too, now
    names(poptext.e) <- names.e.nice.api  # ejscreen::names.e.nice  # longer form is   ejscreen::nicenames(names(poptext.e))
  }
  
  # make popups text for the EJ INDEXES (US AND STATE PCTILES, FOR NORMAL AND SUPPLEMENTARY EJ INDEXES) ####
  if (length(pok) == 0) {
    warning('none of names.ej.pctile.api found in out')
    poptext.ej <- NULL
  }else{
    poptext.ej <- make.popup.ej.api(pctile = out[, names.ej.pctile.api], state.pctile = out[, names.ej.state.pctile.api], labels = names.ej.pctile.api_USORSTATE)
  }
  
  # 'EJScreen Report', linkcolname2='EJScreen Map', linkcolname3='EJScreenACS'
  # if (!(linkcolname %in% names(out)))  {linkcolname  <- 'EJScreen Report'} # needed for ejscreenapi_plus() but not sure if for server.R
  # if (!(linkcolname2 %in% names(out))) {linkcolname2 <- 'EJScreen Map'} 
  # if (!(linkcolname3 %in% names(out))) {linkcolname3 <- 'EJScreenACS'} 
  if (linkcolname %in% names(out))  {pops_link1 <- paste0(out[ , linkcolname] ,           '<br>')}  else{ pops_link1 <- paste0(rep(NA, NROW(out)),'<br>')}
  if (linkcolname2 %in% names(out)) {pops_link2 <- paste0(out[ , linkcolname2] ,           '<br>')} else{ pops_link2 <- paste0(rep(NA, NROW(out)),'<br>')}
  if (linkcolname3 %in% names(out)) {pops_link3 <- paste0(out[ , linkcolname3] ,           '<br>')} else{ pops_link3 <- paste0(rep(NA, NROW(out)),'<br>')}
  
  if ('ejam_uniq_id'   %in% names(out)) {pops_ejam_uniq_id <- paste0('ejam_uniq_id: ', out$ejam_uniq_id, '<br>')} else {pops_ejam_uniq_id <- ''}
  if ('id'       %in% names(out)) {pops_id       <- paste0('id: ',       out$id,       '<br>')} else {pops_id       <- ''}
  if ('siteid'   %in% names(out)) {pops_siteid   <- paste0('siteid: ',   out$siteid,   '<br>')} else {pops_siteid   <- ''}
  if ('sitenumber'     %in% names(out)) {pops_sitenumber   <- paste0('sitenumber: ',   out$sitenumber,   '<br>')} else {pops_sitenumber <- ''}
  if ('sitename' %in% names(out)) {pops_sitename <- paste0('sitename: ', out$sitename, '<br>')} else {pops_sitename <- ''}
  if ('radius.miles' %in% names(out)) {pops_radmile <- paste0('Area within ', out$radius.miles, ' miles of site', '<br>')} else {
    pops_radmile <- ''}
  
  if (length(poptext.d) > 0) {
    pops_d <- 
      paste0(
        '<b>', 'Demographic Indicators: ', '</b>',                 '<br>',
        'Population: ', prettyNum(out$pop, big.mark = ','),        '<br>',
        apply(poptext.d, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
      )
  } else {
    pops_d <- ""
  }
  
  if (length(poptext.e) > 0) {
    pops_e <- paste0(
      '<b>', 'Environmental Indicators: ', '</b>',               '<br>',
      apply(poptext.e, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
    )
  } else {
    pops_e <- ''
  }
  
  if (length(poptext.ej) > 0) {
    pops_ej <- paste0(
      '<b>', 'EJ Indexes: ', '</b>',               '<br>',
      apply(poptext.ej, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
    )
  } else {
    pops_ej <- ''
  }
  
  
  z <- paste0(
    '<b>',
    pops_ejam_uniq_id,
    pops_id,
    pops_siteid,
    pops_sitenumber, 
    pops_sitename,
    '</b>',
    pops_radmile,
    paste0('long, lat: ',  out$lon, ', ', out$lat,             '<br>'),
    
    pops_d, 
    pops_e,
    pops_ej,
    
    
    # LINK IN POPUP
    pops_link1, # out[ , linkcolname] ,           '<br>',
    # url_linkify(out[ , linkcolname], 'EJScreen Report'),           '<br>',
    pops_link2, # out[ , linkcolname2] ,    '<br>',
    # url_linkify(out[ , linkcolname2], 'EJScreen Map'),  '<br>',
    pops_link3, 
    sep = '<br>'
  )
  # for (i in 1:NCOL(x)) { x[,i] <- paste(labels[i] , x[,i], sep = ': ' )}
  # as.vector(apply(x, MARGIN = 1, FUN = function(thisrow) paste(thisrow, collapse='<br>')) )
  
  if (verbose) {popup_print(z)} #  was  cat(gsub('<br>','  <br>\n',z))  #  see in console   
  return(z) # invisible(z) did not seem to work for some reason
}
############################################################################# #  


#' Helper function to view popup info in an interactive session - easier format to view
#'
#' @param x output of [popup_from_ejscreen()]
#' @param linkregex see source
#' @param linksimple see source
#' @seealso [popup_from_ejscreen()]
#' 
#' @keywords internal
#' @export
#'
popup_print <- function(x, linkregex='<a href.*>(.*)<.*', linksimple='\\1') {
  
  # simple utility to let you view contents of map popups, in the console:
  cat(paste0(gsub(linkregex, linksimple, gsub('<br>','  \n', x)), collapse = '\n\n'))
}
############################################################################# #  
