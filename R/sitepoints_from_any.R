

# interactively pick file if relevant, 
# then accept object or file and check lat/lon colnames via latlon_from_anything() 
# then add ejam_uniq_id column


sitepoints_from_any <- function(sitepoints, silentinteractive = TRUE) {
  
  # note this overlaps or duplicates code in ejamit() and app_server.R
  #   for data_up_latlon() around lines 81-110 and data_up_frs() at 116-148
  
  # select file interactively in RStudio
  if (missing(sitepoints)) {
    if (interactive() & !silentinteractive & !shiny::isRunning()) {
      sitepoints <- rstudioapi::selectFile(caption = "Select xlsx or csv with FIPS column of Census fips values", path = '.' )
      # that returns the path to the file
      # Do not interactively ask for radii
      sitepoints <- latlon_any_format(sitepoints) # read file and infer colnames with lat lon
    } else {
      if (shiny::isRunning()) {
        warning("sitepoints (locations to analyze) is missing but required.")
        return(NULL)
      } else {
        stop("sitepoints (locations to analyze) is missing but required.")
      }
    }
  }
  # Do these steps here even though ejamit() has the same code, so it won't have to happen once per loop on radius in ejamit_compare_distances_fulloutput() or ejamit_compare_distances()
  
  # If user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
  #  by using sitepoints <- latlon_from_anything(sitepoints) which gets done by getblocksnearby()
  sitepoints <- latlon_from_anything(sitepoints)
  stopifnot(is.data.frame(sitepoints), "lat" %in% colnames(sitepoints), "lon" %in% colnames(sitepoints), NROW(sitepoints) >= 1, is.numeric(sitepoints$lat))
  
  ## check for ejam_uniq_id column; warn and add if not present
  if (!("character" %in% class(sitepoints)) & !c('ejam_uniq_id') %in% names(sitepoints)) {
    # message('sitepoints did not contain a column named ejam_uniq_id, so one was added')
    sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))
  }
  
  return(sitepoints)
}
################################################################################## #  
