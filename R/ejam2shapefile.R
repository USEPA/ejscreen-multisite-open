

########################    #    DRAFT 


#' DRAFT - export EJAM results as shapefile for use in ArcPro, EJScreen, etc.
#'
#' @param ejamitout output of EJAM such as from [ejamit()]
#' @param fname a filename no path and must be .shp extension
#' @param folder optional folder (directory), path to where it should be saved
#' @param crs optional coord ref system
#' @param shortcolnames Whether to cut colnames to 10 characters for .shp format
#' @return path to saved .zip file
#' @param save whether to save file - if FALSE, it returns the object not the file path
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
#' @details see 
#'   [Shapefile format basics from arcgis.com](https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm) 
#' 
#' @export
#' 
ejam2shapefile <- function(ejamitout, fname = "bysite.shp",  folder = ".", 
                           crs = 4269, shortcolnames=TRUE, save = TRUE
) { 
  # ,   ...) {
  
  # library(EJAM);  library(data.table); library(sf)
  #    ejamitout <- testoutput_ejamit_10pts_1miles; crs = 4269; fname = "bysite.shp" ;  folder =  "~/../Downloads"  # getwd()
  df <- data.table::setDF(ejamitout$results_bysite)
  ## and may not support 255 or larger number of characters in a field like the URLs so they get truncated
  ## perhaps should alter or remove the url columns
  unlinkify = function(x) {gsub('.*https', 'https', gsub('=report.*', '=report', gsub('., target.*', '', x)))}
  df[ , 1] <- unlinkify(df[ , 1])
  df[ , 2] <- unlinkify(df[ , 2])
  df[ , 3] <- unlinkify(df[ , 3])
  
  if (all(is.na(df$lat)) | all(is.na(df$lat))) {stop(   "latlon at all sites are NA values")}
  if (any(is.na(df$lat)) | any(is.na(df$lat))) {warning("latlon at some sites are NA values")}
  
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
    
    folder <- normalizePath(folder)
    # cat("working directory is ", folder, "\n")
    if (tools::file_ext(fname) != "shp") {stop("fname extension must be .shp, and the saved file in specified folder will be a .zip file with the .shp etc.")}
    tds <- file.path(tempdir(), "shp")
    if (!dir.exists(tds)) {dir.create(tds)}
    
    # oldwd <- getwd()
    # on.exit(setwd(oldwd))
    # setwd(tds)
    
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
    
    sf::st_write(
      obj = bysite_shp,
      dsn = file.path(tds, fname),
      append = FALSE, delete_layer = TRUE # ,  # should overwrite any existing, but fails if already exists?
      # ...
    )
    
    zipname <- paste0(fname, ".zip")
    fname_noext <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", dir(tds, pattern = fname))
    fnames <- dir(tds, pattern = fname_noext)
    fnames <- fnames[!grepl("zip$", fnames)]
    if (file.exists(zipname)) {file.remove(zipname)}
    zip(zipname, files = fnames)
    zipfullpath <- paste0(normalizePath(folder), "\\", zipname)
    # cat(paste0("saving .zip as ", zipfullpath, '\n'))
    ## save zipfile in temp directory then copy it to folder specified and return the folder/zipname
    file.copy(from = zipname, to = file.path(folder, zipname), overwrite = TRUE)
    
    # setwd(oldwd)
    
    return(zipfullpath)
  }
}
################################################################################### #

