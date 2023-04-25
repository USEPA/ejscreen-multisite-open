#' installejamsource
#' utility for when working on the source code - redoes documentation and reinstalls packages
#' @param pkg vector of names of folders with source packages, 
#'   but not the full path - just the package name. 
#'   Assumes that R_USER points to the folder that has all those source package folders in it.
#'
#' @return nothing. side effect is it does document() and INSTALL
#' @export
#'
installejamsource <- function(pkg=EJAM::ejampackages) {

# rstudioapi::openProject("hellpdocs", newSession = TRUE)
# pkg <- EJAM::ejampackages  # c("EJAMfrsdata", "EJAM")
cat("Updating documentation and reinstalling from source:\n")
cat(paste0(pkg, collapse = ", "), "\n")
oldir = getwd()
on.exit(setwd(oldir))
for (i in 1:length(pkg)) {
  setwd(file.path(Sys.getenv("R_USER"), pkg[i])) 
  devtools::document( pkg = file.path(Sys.getenv("R_USER"), pkg[i]) , roclets = c('rd', 'collate', 'namespace', 'vignette'))
  pkgbuild::rcmd_build_tools("INSTALL", list("--preclean --no-multiarch", "--with-keep.source",   file.path(Sys.getenv("R_USER"), pkg[i])))
  
  # devtools::install( file.path(Sys.getenv("R_USER"), pkg[i]), quick = T, build = F, reload = F, upgrade = F)
  #  but that tries to install from github based on Remotes in DESCRIPTION file
}
cat("done\n")

}
