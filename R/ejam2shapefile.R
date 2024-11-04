


#' DRAFT - export EJAM results as shapefile for use in ArcPro, EJScreen, etc.
#'
#' @param ejamitout output of EJAM such as from [ejamit()]
#' @param fname optional filename with no path and must be .shp extension
#' @param folder optional - If omitted (and not running in shiny and if interactive() mode),
#'   this function prompts you to specify the folder, path to where the .zip should be saved.
#' @param crs optional coord ref system
#' @param shortcolnames Whether to cut colnames to 10 characters for .shp format
#' @return path to saved .zip file
#' @param save whether to save file - if FALSE, it returns the object not the file path
#' @param varnames optional vector of which colnames of ejamitout$results_bysite 
#'   to include in shapefile. DJefault is all other than averages, ratios, and raw EJ scores.
#'   Can be "all" or NULL to include all columns.
#' @examples \dontrun{
#'   # folder = "~/../Downloads"
#'   # out <- ejamit(testpoints_100 , radius = 3.1)
#'   # fname <- ejam2shapefile(out, "test100_3miles.shp", folder = folder)
#'   
#'   out <- testoutput_ejamit_10pts_1miles
#'   fname <- ejam2shapefile(out)
#'   shp <- shapefile_from_any(fname)
#'   map_shapes_leaflet(shp)
#'   }
#' @details FIELD NAMES (indicator names) CURRENTLY ARE TRUNCATED AND NUMBERED TO BE ONLY 10 CHARACTERS MAX. 
#' 
#' see 
#'   [Shapefile format basics from arcgis.com](https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm) 
#' 
#' @export
#' 
ejam2shapefile <- function(ejamitout, fname = "EJAM_results_bysite_date_time.shp",  folder = ".", 
                           crs = 4269, shortcolnames=TRUE, save = TRUE, 
                           varnames = "basic250"
) { 
  # ,   ...) {
  
  #  ejamitout <- testoutput_ejamit_10pts_1miles; crs = 4269; fname = "bysite.shp" ;  folder =  "~/../Downloads"  # getwd()
  
  df <- data.table::setDF(ejamitout$results_bysite)
  
  if (is.null(varnames) || all(is.na(varnames)) || varnames[1] == "all") {
    varnames <- "all"
    # df <- df
  } else {
    if (all(varnames[1] == "basic250")) {
      # because shapefiles have a cap on number of fields in some implementations
      # omits averages, ratios, and raw EJ scores, which are not essential or are not in typical EJScreen outputs
      names_basic250 <- sort(grep("^avg|^state.avg|^ratio|^EJ.D|^state.EJ", names(df), invert = T, value = T)) 
      ok <- names(df) %in% names_basic250
      if (any(!ok)) {warning("Some specified varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("No specified varnames found in ejamitout$results_bysite") }
      df <- df[ , ok]
      message("Using only basic 250 or so columns - 
To include averages, ratios, and raw EJ scores, set varnames = 'all' or NULL.
To include specific columns provides those as a character vector of varnames.")
    } else {
      ok <- names(df) %in% varnames
      if (any(!ok)) {warning("Some specified varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("No specified varnames found in ejamitout$results_bysite") }
      df <- df[ , ok]
    }
  }
  
  ## shapefile may not support 255 or larger number of characters in a field like the URLs so they get truncated
  ## perhaps should alter or remove the url columns
urlcols = which(grepl("a href=", names(df)))
if (length(urlcols) > 0) {
  df[ , urlcols] <- unlinkify(df[ , urlcols])
}  

  if (all(is.na(df$lat)) || all(is.na(df$lon))) {
    # *** probably it was analysis of FIPS or Shapefile, not latlon
    cat(
    'Shapefile of results gets mapped in the shiny app, but 
this save function only handles ejamit analysis of proximity to latlon points --
it is not yet implemented here for ejamit analysis of polygons from Shapefile or analysis of FIPS -- 
would need original shapefile to join it to table of results.
Except, if Counties were analyzed, see  mapfastej_counties() \n')
    warning(   "latlon at all sites are NA values")
    
    return(NA)

  } else {
    if (any(is.na(df$lat)) || any(is.na(df$lon))) {warning("latlon at some sites are NA values")}
  }
  bysite_shp <- shapefile_from_sitepoints(df, crs = crs)
  
  ## just does sf::st_as_sf()
  ## later will want to handle ejamit() outputs where shapefile was analyzed not just circles around points
  # usedpoints <- "sfc_POINT" %in% class(st_geometry(bysite_shp))
  
  ### ADD CIRCULAR BUFFERS FOR MAPPING ETC. 
  radius <- df$radius.miles
  bysite_shp <-  shape_buffered_from_shapefile_points(bysite_shp, radius.miles = radius, crs = crs)
  
  ## note this removes the columns lat,lon   and  adds a column at the end called geometry
  ## so its columns are not directly comparable to column names of ejamitout$results_bysite
  bysite_shp$lat = df$lat
  bysite_shp$lon = df$lon
  if (!save) {
    return(bysite_shp)
    
  } else {
    
    ### need >=10 character colnames to save as .shp file format. see sf:::abbreviate_shapefile_names etc.
    ### so shortening them but "geometry" must not be changed
    if (shortcolnames) {
      names(bysite_shp)[names(bysite_shp) != "geometry"] <- paste0(
        substr(names(bysite_shp)[names(bysite_shp) != "geometry"] , 1, 7),
        1:length(names(bysite_shp)[names(bysite_shp) != "geometry"]))
      names(bysite_shp) <- tolower(names(bysite_shp))
      ###  but renaming ejam_uniq_id  is not ideal - try to keep it?
      # bysite_shp$ejam_uniq_id <- bysite_shp$ejam_4
    }
    #  Creating a 256th field, but some DBF readers might only support 255 fields
    
    
    if (interactive() && !shiny::isRunning()) {
      if (missing(folder)) {
        folder <- rstudioapi::selectDirectory("Select/confirm Folder to Save .zip in", path = folder)
      }
    }
    if (missing(fname)) {
      fname <- create_filename(ext = ".shp", file_desc = "results_bysite") # e.g.,  "EJAM_results_bysite_20240901_162119.shp"
    }
    # folder <- normalizePath(folder) # ?? converts from x/y/z  to  x\\y\\z  on windows.
    if (tools::file_ext(fname) != "shp") {stop("fname extension must be .shp, and the saved file in specified folder will be a .zip file with the .shp etc.")}
    tds <- file.path(tempdir(), "shp")
    if (!dir.exists(tds)) {dir.create(tds)}
    
    sf::st_write(
      obj = bysite_shp,
      dsn = file.path(tds, fname),
      append = FALSE, delete_layer = TRUE # ,  # should overwrite any existing, but fails if already exists?
      # ...
    )
    
    zipname <- paste0(fname, ".zip")
    fname_noext <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", dir(tds, pattern = fname))  # ?? 
    fnames <- dir(tds, pattern = fname_noext)
    fnames <- fnames[!grepl("zip$", fnames)]
    if (file.exists(zipname)) {file.remove(zipname)}

    zipfullpath <- paste0(normalizePath(folder), "/", zipname)
    zip(zipfullpath, files = file.path(tds, fnames), extras = c('-j', '-D')) # unzip from tempdir to folder specified by parameter. -D should prevent storing Directory info, -j is supposed to use no path info so files are all in root of .zip and there are not folders inside the .zip
    
    return(zipfullpath)
  }
}
################################################################################### #
