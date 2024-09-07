
###################################################################### #
if (1 == 0) {
  
# to simplify calling this function from a NON-shiny context, to avoid saying shapefix(shp)[['shp']]
# 
# 1. if called from shiny it can be a diff name like shapefix_shinylist()
# or
# 2. it can check a param like shiny=true, 
# or
# 3. it can check for those params all being missing to see if it is called from shiny,
# or
# 4. it can check shiny::!is_running()  
  
  ## to add to ejamit.R or shapefile_from_any() or all the functions it uses
  shp <- shapefix[['shp']]
  if (!is.null(shinyvalidatemsg)) {stop(shinyvalidatemsg)} # but it already stops in shapefix() 
  
  ## to add to shiny app_server.R, replacing similar code that was being used there:
  shp <- shapefix[['shp']]
  if (!is.null(disable_buttons_SHP)) {disable_buttons[['SHP']] <- disable_buttons_SHP}
  if (!is.null(num_valid_pts_uploaded_SHP)) {num_valid_pts_uploaded[['SHP']] <- num_valid_pts_uploaded_SHP}
  if (!is.null(invalid_alert_SHP)) {invalid_alert[['SHP']] <- invalid_alert_SHP}
  if (!is.null(an_map_text_shp))  {an_map_text[['SHP']] <- an_map_text_shp}
  if (!is.null(shinyvalidatemsg)) {shiny::validate(shinyvalidatemsg)}
}
###################################################################### #


 
  
shapefix = function(shp,
                    disable_buttons_SHP = NULL, 
                    num_valid_pts_uploaded_SHP = NULL, 
                    invalid_alert_SHP = NULL, 
                    an_map_text_shp = NULL, 
                    shinyvalidatemsg = NULL) {
  
  ## THIS IS A WAY FOR app_server, and ejamit() via shapefile_from_any(),
  ##  to both use this one function
  ##  to do the same thing whether or not in a reactive context.
  
  return_now = function() {
    if (!shiny::isRunning() && !is.null(shinyvalidatemsg)) {
      stop(shinyvalidatemsg)
    }
    return(list(
      shp = shp, 
      disable_buttons_SHP = disable_buttons_SHP, 
      num_valid_pts_uploaded_SHP = num_valid_pts_uploaded_SHP, 
      invalid_alert_SHP = invalid_alert_SHP,
      shinyvalidatemsg = shinyvalidatemsg,
      an_map_text_shp = an_map_text_shp)
    )
  }
  
  if (is.null(shp)) {
    disable_buttons_SHP <- TRUE
    # disable_buttons[['SHP']] <- TRUE
    shinyvalidatemsg <- "file should contain the following file extensions: shp, shx, dbf, prj"
    # shiny::validate("Uploaded file should contain the following file extensions: shp, shx, dbf, prj")
    return_now()
  }
  
  if (any(sf::st_geometry_type(shp) == "POINT")) {
    disable_buttons_SHP <- TRUE
    # disable_buttons[['SHP']] <- TRUE
    shinyvalidatemsg <- "Shape file must be of polygon geometry."
    # shiny::validate("Shape file must be of polygon geometry.")
    return_now()
  }
  
  shp <- sf::st_zm(shp)
  
  if (any(grepl("sfc",lapply(shp,class)))) {
    colnames(shp)[grepl("sfc",lapply(shp,class))] <- "geometry"
    st_geometry(shp) <- "geometry"
  }
  
  if (nrow(shp) > 0) {
    
    shp_valid_check <- terra::is.valid(terra::vect(shp), messages = T)
    shp_is_valid <- shp_valid_check$valid
    numna <- sum(!shp_is_valid)
    num_valid_pts_uploaded_SHP  <- length(shp_is_valid) - sum(!shp_is_valid)
    # num_valid_pts_uploaded[['SHP']] <- length(shp_is_valid) - sum(!shp_is_valid)
    invalid_alert_SHP <- numna
    # invalid_alert[['SHP']] <- numna
    shp_valid <- dplyr::mutate(shp, siteid = dplyr::row_number())
    shp_proj <- sf::st_transform(shp_valid, crs = 4269)
    an_map_text_shp <- NA # will have to handle this as it gets returned
    
  } else {
    
    invalid_alert_SHP <- 0
    # invalid_alert[['SHP']] <- 0 # hides the invalid site warning
    an_map_text_shp <- HTML(NULL)
    # an_map_text_shp(HTML(NULL)) # hides the count of uploaded sites/shapes
    disable_buttons_SHP <- TRUE
    # disable_buttons[['SHP']] <- TRUE
    ## if not matched, return this message
    shinyvalidatemsg <- 'No shapes found in file uploaded.'
    # shiny::validate('No shapes found in file uploaded.')
    return_now()    
  }
  
  disable_buttons_SHP <- FALSE
  # disable_buttons[['SHP']] <- FALSE
  shp_proj$valid <- shp_is_valid
  if (!("ejam_uniq_id" %in% names(shp_proj))) {
    shp_proj <- cbind(ejam_uniq_id = 1:NROW(shp_proj), shp_proj)
  } else {
    if (!all.equal(1:NROW(shp_proj), shp_proj$ejam_uniq_id))
    {  
      warning("ejam_uniq_id already is a column in the shapefile, but is not 1 through N. However, it will NOT be overwritten.")
    }
  }
  shp_proj$invalid_msg <- NA
  shp_proj$invalid_msg[shp_proj$valid == F] <- shp_valid_check$reason[shp_proj$valid == F]
  shp_proj$invalid_msg[is.na(shp_proj$geometry)] <- 'bad geometry'
  # class(shp_proj) <- c(class(shp_proj), 'data.table')
  data.table::setDT(shp_proj)
  # shp_proj
  shp <- shp_proj
  
  return_now()
}
############################################################ # 
