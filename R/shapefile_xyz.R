
# shapefile_from_folder()
# shapefile_filepaths_from_folder()
# shapefile_filepaths_valid()
# shapefile_from_filepaths()
# shapefile_clean()
# shapefile_from_sitepoints()
# shape_buffered_from_shapefile()
# shape_buffered_from_shapefile_points()

############################################################################################## #

#' shapefile_from_folder  -  read shapefile from a folder
#'
#' @param folder path of folder that contains the files (.shp, .shx, .dbf, and .prj)
#' @param cleanit set to FALSE if you want to skip validation and dropping invalid rows
#' @param crs passed to shapefile_from_filepaths() default is crs = 4269 or Geodetic CRS NAD83 
#'
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @examples \dontrun{
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
#'   x <- get_blockpoints_in_shape(testshape)
#'   leaflet(x$polys) %>% addTiles() %>% addPolygons(color = "blue")
#'   DT::datatable(out$results_bysite)
#'   
#'   }
#'   
shapefile_from_folder <- function(folder = NULL, cleanit = TRUE, crs = 4269) {
  
  if (is.null(folder)) {
    if (interactive()) {
      folder <- rstudioapi::selectDirectory(caption = "Select a folder that contains the files (.shp, .shx, .dbf, and .prj)", path = getwd())
      # and cpg is ok but not essential?
    } else {
      if(shiny::isRunning()){
        warning("need to specify folder where shapefiles are") #
        return(NULL)
      } else {
        stop("need to specify folder where shapefiles are")
      } #
    }
  }
  
  shapefile_from_filepaths(filepaths = shapefile_filepaths_from_folder(folder), cleanit = cleanit, crs = crs)
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
shapefile_filepaths_from_folder <- function(folder = NULL) {
  if (is.null(folder)) {
    if (interactive()) {
      folder <- rstudioapi::selectDirectory(caption = "Select a folder that contains the files (.shp, .shx, .dbf, and .prj)", path = getwd())
      # and cpg is ok but not essential?
    } else {
      if(shiny::isRunning()){
        warning("need to specify folder where shapefiles are") #
        return(NULL)
      } else {
        stop("need to specify folder where shapefiles are")
      } #
    }
  }
  list.files(path = folder, 
             full.names = TRUE, 
             pattern = ".*(dbf|prj|shp|shx|cpg)$",   # with cpg
             ignore.case = TRUE, include.dirs = FALSE, recursive = FALSE)
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
    return(TRUE) 
  } else {
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
#' @param crs if cleanit = TRUE, crs is passed to shapefile_clean() 
#'   default is crs = 4269 or Geodetic CRS NAD83 
#'    Also can check this via x <- sf::st_crs(sf::read_sf()); x$input
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_from_filepaths <- function(filepaths = NULL, cleanit = TRUE, crs = 4269) {
  
  if (is.null(filepaths)) {
    if (interactive()) {
      filepaths <- shapefile_filepaths_from_folder() # must select multiple files, which selectFile() will not do # rstudioapi::selectFile(caption = "Select all the files at once (.shp, .shx, .dbf, and .prj) to upload", path = getwd())
      # and cpg is ok but not essential?
    } else {
      if(shiny::isRunning()){
        warning("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ") # and cpg is ok but not essential?
         return(NULL)
      } else {
        stop("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")
      } # and cpg is ok but not essential?
    }
  }
  
  if (shapefile_filepaths_valid(filepaths = filepaths)) {
    
    if (cleanit) {
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)]  # one (not more) files that end in .shp 
      if (length(shpfilepath) > 1) {warning("using only ", shpfilepath[1], ", the first of more than one .shp file found"); shpfilepath <- shpfilepath[1] }
      # note this will add  ejam_uniq_id =  row_number() 
      return(
        shapefile_clean(
          sf::read_sf(shpfilepath), # , crs = crs  should be left out here ?
          crs = crs
        )
      )
      
    } else {
      # for shiny, do cleaning/check in server so it can offer messages
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)] # one or more files that end in .shp 
      shp <- sf::read_sf(shpfilepath)  # , crs = crs  should be left out here ?
      return(
        dplyr::mutate(shp, ejam_uniq_id = dplyr::row_number()) # number them
      )
    }
  } else {
    return(NULL) # validation did the warning
  }
}

############################################################################################## #


#' shapefile_clean  -  drop invalid rows and warn if all invalid
#'
#' @param shp a shapefile object using sf::read_sf()
#' @param crs used in shp <- sf::st_transform(shp, crs = crs), default is crs = 4269 or Geodetic CRS NAD83 
#'
#' @return a shapefile object using sf::read_sf()
#' @export
#'
#' @seealso [shapefile_from_folder()]
shapefile_clean <- function(shp, crs = 4269) {
  if (nrow(shp) > 0) {
    shp <- dplyr::mutate(shp, ejam_uniq_id = dplyr::row_number()) # number them before dropping invalid ones, 
    #   so that original list can be mapped to results list more easily
    shp <- shp[sf::st_is_valid(shp),]          # determines valid shapes, to use those and drop the others
    shp <- sf::st_transform(shp, crs = crs)  # NEED TO DOCUMENT THE ASSUMPTION IT USES THIS CRS ***
    
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
#' @param crs used in st_as_sf() default is crs = 4269 or Geodetic CRS NAD83 
#' @import sf
#' @return A shapefile via [sf::st_as_sf()]
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @export
#'
shapefile_from_sitepoints <- function(sitepoints, crs = 4269) {
  #data.table::setDF(sitepoints)
  shpcoord <- sf::st_as_sf(sitepoints, coords = c('lon', 'lat'), crs = crs) #   want 4269
  return(shpcoord) #
}
############################################################################################## #


#' shape_buffered_from_shapefile - add buffer around shape
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shapefile spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius.miles width of buffer to add to shapefile
#'   (in case dist is a units object, it should be 
#'   convertible to arc_degree if x has geographic coordinates, 
#'   and to st_crs(x)$units otherwise)
#' @param crs used in st_transform()  default is crs = 4269 or Geodetic CRS NAD83 
#' @param ... passed to st_buffer()
#' @import sf
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @export
#' 
shape_buffered_from_shapefile <- function(shapefile, radius.miles, crs = 4269, ...) {
  
  return(sf::st_buffer(shapefile %>%  sf::st_transform(crs = crs), #  
                       dist = units::set_units(radius.miles, "mi"), ...))
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
#' @param crs used in st_transform()  default is crs = 4269 or Geodetic CRS NAD83 
#' @param ... passed to st_buffer()
#' @import sf
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @export
#' 
shape_buffered_from_shapefile_points <- function(shapefile_points, radius.miles, crs = 4269, ...) {
  
  return(sf::st_buffer(shapefile_points %>%  sf::st_transform(crs = crs), #  
                       dist = units::set_units(radius.miles, "mi"), ...))
}
############################################################################################## #

