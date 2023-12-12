#' dataload_from_local
#' utility for R analysts/developers to store large block data locally instead of redownloading from AWS
#' @details  
#'   rm(bgid2fips, blockid2fips, blockpoints, blockwts, quaddata)
#'   
#'   dataload_from_local(folder_local_source = '.')
#'   
#' @param varnames use defaults
#' @param ext  use defaults
#' @param fun  use defaults
#' @param envir  use defaults
#' @param folder_local_source Your local folder path
#' @param justchecking  use defaults
#' @param testing  use defaults
#' @return vector of paths to files (as derived from varnames) that were 
#'   actually found in folder_local_source,
#'   but only for those not already in memory, so it is 
#'   just the ones loaded from disk because not already in memory and found on disk locally.
#' @export
#'
dataload_from_local <- function(varnames = c(
  c('blockwts', 'quaddata', 'blockpoints', 'blockid2fips', 'bgid2fips', 'bgej'),
  c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")), 
  ext = c(".arrow", ".rda")[1],
  fun = c("arrow::read_ipc_file", "load")[1],
  envir = globalenv(),  # should it be parent or global or package EJAM envt ??
  folder_local_source = "~/../Downloads", 
  justchecking = FALSE, 
  testing = FALSE) {
  
  # if (interactive()) {
  if (!is.character(fun)) {stop('must specify function in fun parameter as a quoted character string')}
  if (length(ext) > 1)    {stop('must specify only one file extension for all the files')}
  if (ext == "arrow") ext <- ".arrow"
  if (ext == "rda")   ext <- ".rda"
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::read_ipc_file"}
  
  fnames     <- paste0(varnames, ext) # varnames are like bgid2fips, ext is .rda, fnames are like bgid2fips.rda
  # objectnames <- paste0(mybucketfolder,      '/', fnames) # EJAM/bgid2fips.rda 
  localpaths  <- paste0(folder_local_source, '/', fnames)
  localpaths_found <- NULL
  # make output in console easier to read:  
  spacing <- sapply(1:length(varnames), function(x) paste0(rep(" ", max(nchar(varnames)) - nchar(varnames[x])), collapse = ''))
  
  for (i in 1:length(fnames)) {
    
    if (!exists(varnames[i], envir = envir) ) {
      # NOT in memory  ################################################################ #
      
      if (justchecking) {
        cat(  varnames[i],spacing[i],
              'NOT in memory\n')
      }
      if (file.exists(localpaths[i] )) {
        #  NOT in memory, but is on local drive ##################### #
        
        localpaths_found <- c(localpaths_found, localpaths[i])
        
        if (!justchecking) {
          if (ext == '.rda') {
            cat( varnames[i], spacing[i],
                 'is being loaded from', localpaths[i],'...')
            load(localpaths[i], envir = envir)
            cat("done.\n")
            next
          } else {
            assign(varnames[i], arrow::read_ipc_file(file = localpaths[i]), envir = envir)
          }
        } else {
          cat(varnames[i],spacing[i],
              'is found locally on disk at', localpaths[i], '\n')
        }
      } else { 
        # NOT in memory, NOT on local disk ##################### #

        cat(  varnames[i],spacing[i],
              'NOT found locally at', localpaths[i], '\n')
        next
      }
      
    } else {
      #  in memory  ################################################################ #
      
      if (justchecking) {
        cat( varnames[i],spacing[i],
             'is already in memory\n') # redundant with similar lines in dataload_from_aws()
        
        if (file.exists(localpaths[i] )) {
          # in memory, AND on local disk ##################### #
          
          cat( varnames[i],spacing[i],
               'is found locally on disk at', localpaths[i], '\n')
        } else {
          # in memory, NOT on local disk ##################### #
          
          cat( varnames[i],spacing[i],
               'NOT found locally on disk at', localpaths[i], '\n')
        }
      next
      }
      
    }
  } # end of loop
  
  cat("\n")
  return(localpaths_found)
  # } else {
  # message("Must be in interactive mode not on server to load from local disk using dataload_from_local()")
  # }
}
