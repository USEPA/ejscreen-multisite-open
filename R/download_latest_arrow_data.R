
#' Download latest versions of arrow datasets if user doesn't have them
#'
#' Used when EJAM is loaded
#'
#' @param varnames use defaults, or vector of names like "bgej" or use "all" to get all available
#'
#' @export
#'
download_latest_arrow_data <- function(
  varnames = c(
    "blockpoints", 
    "blockwts", 
    "quaddata",
    "bgej",
    "bgid2fips",
    "blockid2fips",
    "frs",
    "frs_by_mact",
    "frs_by_naics",
    "frs_by_programid"
    "frs_by_sic"
  )) {
  
  # get latest release and user's release
  latestRelease <- piggyback::pb_releases(repo = "USEPA/ejamdata")
  usersDesc <- desc::desc(file = "DESCRIPTION")
  usersRelease <- paste0('v', usersDesc$get("ArrowVersion"))

  # if user has latest release, check if any requested files are missing
  # if so, need to re-download. Otherwise, all set
  if(usersRelease == latestRelease) {
    filenames <- paste0(varnames, ".arrow")
    full_paths <- file.path(app.sys('data'), filenames)
    missing_files <- filenames[!file.exists(full_paths)]
    if(length(missing_files) == 0) return
  }

  # otherwise, download the data from EJAM's release assets
  lapply(
    missing_files, 
    function(f) {
      piggyback::pb_download(
        file = f,
        dest = app.sys('data'),
        repo = 'USEPA/EJAM', 
        tag = "latest"
      )
    }
  )

  # update DESCRIPTION file
  usersDesc$set("ArrowVersion", latestVersion)
}
