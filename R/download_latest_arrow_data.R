
#' Download latest versions of arrow datasets if user doesn't have them
#'
#' Used when EJAM is loaded
#'
#' @param varnames use defaults, or vector of names like "bgej" or use "all" to get all available
#' @param envir if needed to specify environment other than default, e.g., globalenv() or parent.frame()
#'
#' @export
#'

download_latest_arrow_data <- function(
  varnames = .arrow_ds_names,
  envir = globalenv()) {
  
  # Check if dataset(s) already loaded
  files_not_loaded <- sapply(varnames, function(v) !exists(v, envir = envir))
  if(!all(files_not_loaded)) return(NULL)
  
  # get latest Arrow version (from EJAMData repo's latest release tag) 
  # and user's Arrow version (from DESCRIPTION's ArrowVersion attribute)
  latestArrowVersion <- piggyback::pb_releases(
    repo = "USEPA/ejamdata",
    .token = NULL
  )[1, "tag_name"]
  ejamdata_version_fpath <- paste0(EJAM:::app_sys('data'),"/ejamdata_version.txt")
  if(!file.exists(ejamdata_version_fpath)) {
    usersArrowVersions <- NULL
  } else {
    usersArrowVersions <- readLines(ejamdata_version_fpath)
  }

  filenames <- paste0(varnames, ".arrow")
  
  # if user has latest release, check if any requested files are missing
  # if so, need to re-download (default to all files). Otherwise, all set
  if(isTRUE(usersArrowVersions == latestArrowVersion)) {
    message("Arrow data is up-to-date!")
    full_paths <- file.path(EJAM:::app_sys('data'), filenames)
    missing_files <- filenames[!file.exists(full_paths)]
    if(length(missing_files) == 0) return(NULL)
    
    message("However, some files are missing. Downloading them from EJAMData.")
  } else {
    message("Arrow data is out-of-date!")
    missing_files <- filenames
  }

  # otherwise, download the data from EJAM's release assets
  piggyback::pb_download(
    file = missing_files,
    dest = EJAM:::app_sys('data'),
    repo = 'USEPA/ejamdata', 
    tag = "latest",
    use_timestamps = FALSE
  )

  message("Finished downloading. Updating stored EJAMData version.")
  # update user's arrowversion
  writeLines(latestArrowVersion, ejamdata_version_fpath)
  message("Finished updating stored EJAMData version.")
}
