############################################################################################## #

#' shapefile_from_folder  -  read shapefile from a folder
#'
#' @param folder path of folder that contains the files (.shp, .shx, .dbf, and .prj)
#'
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @examples
#'   testfolder <- system.file("testdata/shapes/Portland_neighborhoods", package = "EJAM")
#'   testshape <- shapefile_from_folder(testfolder)
#'   
#'   testpaths <- shapefile_filepaths_from_folder(testfolder)
#'   testshape <- shapefile_from_filepaths(testpaths)
#'   
#'   ## if interactive(), R user can point to right folder or select the right set of files:
#'   # testshape <- shapefile_from_filepaths()
#'   # testshape <- shapefile_from_folder()
#'   
shapefile_from_folder <- function(folder = NULL) {
  
  if (is.null(folder)) {
    if (interactive()) {
      folder <- rstudioapi::selectDirectory(caption = "Select a folder that contains the files (.shp, .shx, .dbf, and .prj)", path = getwd())
      # and cpg is ok but not essential?
    } else {
      stop("need to specify folder where shapefiles are")} #
  }
  
  shapefile_from_filepaths(filepaths = shapefile_filepaths_from_folder(folder))
}
############################################################################################## #


#' shapefile_filepaths_from_folder  -  get list of valid filenames comprising shapefile including paths
#'
#' @param folder path of folder that contains the files (.shp, .shx, .dbf, and .prj)
#'
#' @return string vector of filenames including full paths 
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_filepaths_from_folder <- function(folder) {
  list.files(path = folder, 
             full.names = TRUE, 
             pattern = ".*[dbf|prj|shp|shx|cpg]$",   # with cpg
             # pattern = ".*[dbf|prj|shp|shx]$", 
             ignore.case = TRUE)
}
############################################################################################## #


#' shapefile_filepaths_valid  -  confirm files have all the extensions .shp, .shx, .dbf, and .prj
#'
#' @param filepaths vector of full paths with filenames (types .shp, .shx, .dbf, and .prj) as strings
#' 
#' @return logical, indicating if all 4 extensions are found among the filepaths
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_filepaths_valid <- function(filepaths) {
  infile_ext <- tools::file_ext(filepaths)
  # does not need .cpg ?
  ok <- all(c('shp','shx','dbf','prj') %in% tolower(infile_ext)) # note it ignores case here now
  if (ok) {
    return(TRUE) } else {
      warning("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")
      # and cpg is ok but not essential?
      return(FALSE)
    }
}
############################################################################################## #

#' shapefile_from_filepaths  -  Read shapefile from disk based on the filenames given
#'
#' @param filepaths vector of full paths with filenames (types .shp, .shx, .dbf, and .prj) as strings
#' @param cleanit set to FALSE if you want to skip validation and dropping invalid rows
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_from_filepaths <- function(filepaths = NULL, cleanit = TRUE) {
  
  if (is.null(filepaths)) {
    if (interactive()) {
      filepaths <- rstudioapi::selectFile(caption = "Select all the files at once (.shp, .shx, .dbf, and .prj) to upload", path = getwd())
      # and cpg is ok but not essential?
    } else {
      stop("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")} # and cpg is ok but not essential?
  }
  
  if (shapefile_filepaths_valid(filepaths = filepaths)) {
    
    if (cleanit) {  
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)] # one or more files that end in .shp 
      return(
        shapefile_clean(
          sf::read_sf(shpfilepath)
        )
      )
      
    } else {
      # for shiny, do cleaning/check in server so it can offer messages
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)] # one or more files that end in .shp 
      return(
        sf::read_sf(shpfilepath)
      )
    }
  } else {
    return(NULL) # validation did the warning
  }
}

############################################################################################## #


#' shapefile_clean  -  drop invalid rows and warn if all invalid
#' @details it assumes crs = 4269 when doing sf::st_transform(shp, crs = 4269) 
#' @param shp a shapefile object using sf::read_sf()
#'
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_clean <- function(shp) {
  if (nrow(shp) > 0) {
    shp <- shp[sf::st_is_valid(shp),]          # determines valid shapes, to use those and drop the others
    shp <- dplyr::mutate(shp, siteid = dplyr::row_number())
    shp <- sf::st_transform(shp, crs = 4269)  # NEED TO DOCUMENT THE ASSUMPTION IT USES THIS CRS ***
    
  } else {
    
    warning('No shapes found in file uploaded.')
    shp <- NULL
  }
  return(shp) 
}
############################################################################################## #


#' shapefile_from_sitepoints - convert table of lat,lon points/sites into sf:: shapefile
#' Creates a simple feature (sf) dataframe from points
#' @param sitepoints a data.table or data.frame with columns called lat,lon
#' @import sf
#' @return A shapefile via [sf::st_as_sf()]
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @export
#'
shapefile_from_sitepoints <- function(sitepoints) {
  #data.table::setDF(sitepoints)
  shpcoord <- sf::st_as_sf(sitepoints, coords = c('lon', 'lat'), crs = 4269)
  return(shpcoord) # but want 4269
}
############################################################################################## #


#' shape_buffered_from_shapefile_points - add buffer around shape (points, here)
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shapefile_points spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius.miles width of buffer to add to shapefile_points 
#'   (in case dist is a units object, it should be 
#'   convertible to arc_degree if x has geographic coordinates, 
#'   and to st_crs(x)$units otherwise)
#' @param ... passed to st_buffer()
#' @import sf
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points]
#' @export
#' 
shape_buffered_from_shapefile_points <- function(shapefile_points, radius.miles, ...) {
  
  return(sf::st_buffer(shapefile_points %>%  sf::st_transform(4269), # was "ESRI:102005" but want 4269
                       dist = units::set_units(radius.miles, "mi"), ...))
  
}
############################################################################################## #

