
#  EJAM/data-raw/datacreate_names_of_indicators.R

########################################################################################################## #
# ***
#   to be eventually replaced with a newer approach... see issue #491

## note subgroups info is tricky since $varlist was "names_d_subgroups_nh" not "names_d_subgroups" 

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
  
  ############################################################################## ############################################################################### #
  
  #   to be eventually replaced with a newer approach... see issue #491
  
  ################################################################ # 
  # Below creates all the lists of variable names and save them to the package as lazyloaded datasets
  # relying entirely on map_headernames$varlist and map_headernames$rname
  # 
  ## note these are the same:  all(names_climate == names_from_varlist("names_climate"))
  
  # to create ALL the lists all at once, would be this: 
  # 
  # load_all() # needs EJAM to work, including map_headernames has to be available
  vlists <- unique(map_headernames$varlist)
  
  # assign all the lists of variable names to the global environment
  for (i in seq_along(vlists)) {
    vns <- varlist2names(vlists[i])
    assign(vlists[i], vns)
  }
  # assign metadata to all those lists
  for (i in seq_along(vlists)) {
    assign(vlists[i], metadata_add(get(vlists[i])))
  }
  # save all those lists to the package as lazyloaded datasets
  # golem::detach_all_attached() # to avoid any lazyloaded versions of these objects
  for (i in seq_along(vlists)) {
    # for (i in 1:2) {
    codetosource <- paste0("usethis::use_data(", vlists[i], ", overwrite=TRUE)")
    eval(parse(text = codetosource)) 
  }
  # make placeholders for any missing documentation of those datasets
  # - note this does not remove obsolete or update existing documentation
  
  ## but will improve this to avoid so many files of documentation... 
  ## see https://jsta.rbind.io/blog/automated-roxygen-documentation-of-r-package-data/
  ## see https://roxygen2.r-lib.org/reference/tags-reuse.html
  for (i in seq_along(vlists)) {
    if (!file.exists(paste0("./R/data_", vlists[i], ".R"))) {
      cat(paste0("Creating documentation placeholder: data_", vlists[i], ".R \n"))
      filecontents <- paste0(
        "#' @name ", vlists[i], " 
#' @docType data
#' @title a list of variable names for internal use in EJAM
'", vlists[i],"'
"                   # vlists[i] here is the name of the object and is quoted, as the last line in the documentation file for a dataset in \data\ folder, but must be NULL if it is documentation for a large dataset stored only in pins.
      )
      fname = paste0("./R/data_", vlists[i], ".R")
      writeChar(filecontents, con = fname)             ############# #
      # file.exists(fname)
    }
  }
  rm(vns, i, codetosource, vlists)
  
  ############################################################################## ############################################################################### #
  
  # **names_these_ ####
  
  names_these                    <- c(names_d,              names_d_subgroups,              names_e)
  names_these_avg                <- c(names_d_avg,          names_d_subgroups_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
  names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
  names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
  names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,    names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
  
  names_these <- metadata_add(names_these) 
  names_these_avg <- metadata_add(names_these_avg) 
  names_these_state_avg <- metadata_add(names_these_state_avg) 
  names_these_ratio_to_avg <- metadata_add(names_these_ratio_to_avg) 
  names_these_ratio_to_state_avg <- metadata_add(names_these_ratio_to_state_avg) 
  # pctile and friendly not used here
  
  use_data(names_these, overwrite = TRUE)
  use_data(names_these_avg, overwrite = TRUE)
  use_data(names_these_state_avg, overwrite = TRUE)
  use_data(names_these_ratio_to_avg, overwrite = TRUE)
  use_data(names_these_ratio_to_state_avg, overwrite = TRUE)
  
  ############################################################################## #
  
  # **names_all_r ####
  
  names_all_r <- varlist2names(sapply(ls(pattern = "^names_"), c)) # all the ones that are in lists already
  names_all_r <- sort(unique(names_all_r))
  names_all_r <- metadata_add(names_all_r)
  use_data(names_all_r, overwrite = TRUE)
  
  ############################################################################## #
  # y = askYesNo("Do you want to continue and create the metadata for all the lists of variable names and save them to the package as lazyloaded datasets?")
  # if (is.na(y) || !y) {return()} 
  
  # **namez__ ####
  
  # try putting these all in one list instead of multiple objects? could recode later to use namez$d_friendly instead of names_d_friendly etc.
  # and/or    just store them in a big table
  
  namesoflistsofnames = c('names_all_r', sort(unique(map_headernames$varlist)))
  namez <- lapply(namesoflistsofnames, get)
  names(namez) <- gsub("^names_","", namesoflistsofnames)
  #  metadata_add & USE_DATA # 
  namez <- metadata_add(namez)
  usethis::use_data(namez, overwrite = TRUE)
  ############################################################################## #
  
  # **names_all ####
  
  # NOTE THIS IS VERY DIFFERENT THAN names_all_batch !!
  names_all <- as.vector(unlist(namez))
  names_all <- unique(names_all) # pop would appear twice
  names_all <- metadata_add(names_all)
  use_data(names_all, overwrite = TRUE)
  
  ############################################################################## #
  
  # check what was not created
  
  namesoflistsofnames <- c('names_all', namesoflistsofnames)
  for (vl in unique(map_headernames$varlist)) {
    if (nchar(vl) > 0 && !exists(vl)) {warning(paste0(vl, " was not created as a data object."))}
  }
  ############################################################################## #
  
  # create other missing documentation ####
  
  vlists =  sapply(ls(pattern = "^names_"), c)
  
  for (i in seq_along(vlists)) {
    if (!file.exists(paste0("./R/data_", vlists[i], ".R"))) {
      cat(paste0("Creating documentation placeholder: data_", vlists[i], ".R \n"))
      filecontents <- paste0(
        "#' @name ", vlists[i], " 
#' @docType data
#' @title a list of variable names for internal use in EJAM
'", vlists[i],"'
"
      )
      fname = paste0("./R/data_", vlists[i], ".R")
      writeChar(filecontents, con = fname)             ############# #
      # file.exists(fname)
    }
  }
  ############################################################################## #
  
  ## check if usastats and statestats colnames match names_these ####
  
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
  ############################################################################## #
  return(names_these)
  ############################################################################## #
  
  ## (older code that created   _friendly versions) ####
  
  if ("not replaced by code above" == "not yet")  {
    
    #  *** EJScreen refers to EJ indexes without mentioning they are percentiles, since that is the only way they are reported.
    # Should EJAM do same, or be more explicit with friendly names? 
    # names_ej_friendly can be used instead for pctiles, probably, since it does not mention whether it is raw or percentile
    
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
    
    ############################################################################## #
    
    names_d_friendly                       <- fixcolnames(names_d, 'r', 'long')
    names_d_count_friendly                 <- fixcolnames(names_d_count, 'r', 'long')
    names_d_other_count_friendly           <- fixcolnames(names_d_other_count, 'r', 'long')
    names_d_subgroups_friendly             <- fixcolnames(names_d_subgroups, 'r', 'long')
    names_d_subgroups_count_friendly       <-  fixcolnames(names_d_subgroups_count, 'r', 'long') 
    names_d_subgroups_alone_count_friendly <-  fixcolnames(names_d_subgroups_alone_count, 'r', 'long') 
    names_d_subgroups_nh_count_friendly    <-  fixcolnames(names_d_subgroups_nh_count, 'r', 'long')
    names_e_friendly  <- fixcolnames(names_e, 'r', 'long') 
    names_ej_friendly <- fixcolnames(names_ej, 'r', 'short')
    names_ej_state_friendly  <- fixcolnames(names_ej_state, 'r', 'short')  
    names_ej_supp_friendly  <-  fixcolnames(names_ej_supp, 'r', 'short') 
    names_ej_supp_state_friendly <- fixcolnames(names_ej_supp_state, 'r', 'short') 
    names_d_pctile_friendly                 <- fixcolnames(names_d_pctile, 'r', 'long')
    names_d_state_pctile_friendly           <- fixcolnames(names_d_state_pctile, 'r', 'long')
    names_d_subgroups_alone_pctile_friendly       <- fixcolnames(names_d_subgroups_alone_pctile, 'r', 'long')
    names_d_subgroups_alone_state_pctile_friendly <- fixcolnames(names_d_subgroups_alone_state_pctile, 'r', 'long')
    names_d_subgroups_nh_pctile_friendly          <- fixcolnames(names_d_subgroups_nh_pctile, 'r', 'long')
    names_d_subgroups_nh_state_pctile_friendly    <- fixcolnames(names_d_subgroups_nh_state_pctile, 'r', 'long')
    names_d_subgroups_pctile_friendly        <- fixcolnames(names_d_subgroups_pctile, 'r', 'long')    
    names_d_subgroups_state_pctile_friendly    <- fixcolnames(names_d_subgroups_state_pctile, 'r', 'long') 
    names_e_pctile_friendly                 <- fixcolnames(names_e_pctile, 'r', 'long') 
    names_e_state_pctile_friendly           <- fixcolnames(names_e_state_pctile, 'r', 'long') 
    names_ej_pctile_friendly               <- fixcolnames(names_ej_pctile, 'r', 'long')
    names_ej_state_pctile_friendly          <- fixcolnames(names_ej_state_pctile, 'r', 'long')
    names_ej_supp_pctile_friendly          <- fixcolnames(names_ej_supp_pctile, 'r', 'long')
    names_ej_supp_state_pctile_friendly     <- fixcolnames(names_ej_supp_state_pctile, 'r', 'long')
    
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
    
    #########   map_headernames differs / inconsistent for these:
    
    names_d_subgroups_avg_friendly       <- paste0("US average ",    names_d_subgroups_friendly)  #  
    names_d_subgroups_state_avg_friendly <- paste0("State average ", names_d_subgroups_friendly)  # 
    # dput(fixcolnames(names_d_subgroups_avg, 'r', 'long'))
    # dput(fixcolnames(names_d_subgroups_state_avg, 'r', 'long'))
    
    names_e_avg_friendly       <- paste0("US Avg ",    names_e_friendly)
    names_e_state_avg_friendly <- paste0("State Avg ", names_e_friendly)
    # dput(fixcolnames(names_e_avg, 'r', 'long'))
    # dput(fixcolnames(names_e_state_avg, 'r', 'long'))
    
    ############################################################################## #
    
    # RATIOS TO AVERAGE  
    
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
  }
  ############################################################################## #
}

# USE THE FUNCTION #### 

datacreate_names_of_indicators()    # this does metadata and use_data inside the function

rm(datacreate_names_of_indicators)

cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())
