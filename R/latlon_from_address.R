
####################################################################### #
  #  library(AOI) # needs tidygeocoder, fipio, and others not otherwise in EJAM:
  ### Imports: datasets, dplyr, fipio, htmlwidgets, jsonlite, leaflet,
  ### leaflet.extras, rnaturalearth, rvest, sf, shiny, terra,
  ### tidygeocoder, units
####################################################################### #

#                Functions here

# latlon_from_address_table()
# latlon_from_address()

# address_from_table()
# address_from_table_goodnames()

# fixcolnames_infer()

##################### #

##     test data:

# test_address_table_9
# test_addresses_9
# test_address_parts1
# test_addresses2
# test_address_table
# test_address_table_withfull
# test_address_table_goodnames

####################################################################### #


#' get lat,lon from table that contains USPS addresses
#'
#' @param x data.frame or can be missing if interactive
#'
#' @return same as output of [latlon_from_address()]
#'
#' @examples
#' address_from_table(test_address_table)
#'
#' ## fname <- system.file("testdata/address/street_address_9.xlsx", package = "EJAM")
#' ## pts <- address_from_table(fname)
#'
#' pts <- latlon_from_address(test_addresses_9[1:2])
#' ## out <- ejamit(pts, radius = 1)
#' ## ejam2report(out)
#'
#' latlon_from_address_table(test_address_table)
#' latlon_from_address_table(test_address_table_withfull)
#' ## *** NOTE IT FAILS IF A COLUMN WITH STREET NAME ONLY IS CALLED "address"
#' ##   instead of that storing the full address.
#'
#' fixcolnames_infer(currentnames = test_address_parts1)
#' fixcolnames_infer(currentnames = names(test_address_table))
#'
#' @export
#'
latlon_from_address_table <- function(x) {

  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
  }
  if (all(is.character(x)) && length(x) == 1) {
    if (file.exists(x)) {
      x <- EJAMejscreenapi::read_csv_or_xl(x)
    }
  }

  latlon_from_address(
    address_from_table(x)
  )
}
####################################################################### #


#' get USPS addresses from a table of that info
#'
#' @param x data.frame with address info in column(s)
#'
#' @return vector of USPS addresses, 1 per row of x
#'
#' @export
#'
address_from_table <- function(x) {

  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
  }
  if (all(is.character(x)) && length(x) == 1) {
    if (file.exists(x)) {
      x <- EJAMejscreenapi::read_csv_or_xl(x)
    }
  }

  names(x) <- fixcolnames_infer(names(x))
  addresses <- address_from_table_goodnames(x)
  return(addresses)
}
####################################################################### #


#' utility to get USPS addresses from a table that has correct colnames
#'
#' @param x a table with columns that overlap with colnames_allowed
#' @param colnames_allowed optional
#' @seealso [address_from_table()]
#' @return vector of USPS addresses
#'
#' @keywords internal
#' @export
#'
address_from_table_goodnames <- function(x, colnames_allowed = c('address', 'street', 'city', 'state', 'zip')) {

  # Get vector of addresses from a table.
  # Assumes "address" column has full address???
  # but that colname if not available,
  # seeks portions of address among columns defined by colnames_allowed
  # and pastes those together separated by a space.
  # i.e. columns called street, city, state, zip
  # or whichever of those are in the table as colnames.
  # Returns a vector of addresses.

  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
  }
  if (all(is.character(x)) && length(x) == 1) {
    if (file.exists(x)) {
      x <- EJAMejscreenapi::read_csv_or_xl(x)
    }
  }

  colnamesfound <- intersect(names(x), colnames_allowed)
  if ("address" %in% colnamesfound && !("street" %in% colnamesfound) && !("city" %in% colnamesfound)) {
    # seems like address column must be the entire address with street, city, state all in it ?
    cols2use <- "address"
    # return(x[, cols2use])
  } else {
    if ("address" %in% colnamesfound && !("street" %in% colnamesfound) && ("city" %in% colnamesfound) ) {
      # seems like address column is probably the street in this case - even if state is missing?
    }
    cols2use <- colnames_allowed[colnames_allowed %in% colnamesfound]
    # return(apply(x[, cols2use], 1, paste, collapse = " "))
  }
  return(
    if (length(cols2use) == 1) {
      return(x[, cols2use])
    } else {
      apply(x[, cols2use], 1, paste, collapse = " ")
    }
  )
}
####################################################################### #


#' convert colnames to standardized names based on aliases
#'
#' Used by address_from_table
#'
#' @param currentnames vector of colnames that may include aliases
#' @param alias_list optional named list where
#'   names are standard colnames like "street"
#'   and each named element in list is a vector of aliases
#'   for that standard name
#'
#' @return vector like currentnames but some renamed to a
#'   standard name if alias found, ignoring case.
#'
#' @export
#'
fixcolnames_infer <- function(currentnames, alias_list = NULL) {

  if (is.null(alias_list)) {
    alias_list <-   list(
      address = c("address"),
      lat = lat_alias, lon = lon_alias,
      street = c("street", "street address", "address1", "address 1"),
      city = c("city", "cityname", "city name"),
      state = c("state", "mystate", "statename", "ST"),
      zip = c("zip", "zipcode", "zip code")
    )
  }

  ################################################## #
  fixcolname1_infer <- function(currentnames, standardname, aliases = NULL) {

    lword <- standardname
    x <- currentnames

    if (!(lword %in% x)) {

      if (lword == 'lat') {
        # try to infer lat, using these in order of preferred to less
        # aliases <- tolower(c('lat', 'latitude83', 'latitude', 'latitudes', 'faclat', 'lats'))
        aliases <- lat_alias
      }
      if (lword == 'lon') {
        # try to infer lon, using these in order of preferred to less
        # aliases <- tolower(c('lon', 'longitude83', 'longitude', 'longitudes', 'faclong', 'long', 'longs', 'lons','lng'))
        aliases <- lon_alias
      }

      bestfound <- intersect(aliases, tolower(x))[1]
      # bestfound <- x[which.min( match(x, aliases ) )] # another way
      if (is.na(bestfound)) { # intersect()[1] returns NA if none
        # warning(paste0(lword, ' missing and no synonyms found')) # do not change x at all
      } else {
        # ignoring case, replace any exact match(es) to that one word. # should ideally confirm unique?
        x <- gsub(paste0('^', bestfound, '$'), lword, x, ignore.case = TRUE)
      }
    }
    if (sum(grepl(paste0('^', lword, '$'), x)) > 1) {warning(paste0('DUPLICATED ', lword))}
    x
  }
  ################################################## #

  newer <- currentnames

  for (i in seq_along(alias_list)) {
    standardname <- names(alias_list)[i]
    aliases <- alias_list[[i]]

    newer <- fixcolname1_infer(
      currentnames = newer,
      standardname = standardname,
      aliases = aliases
    )
  }
  return(newer)
}
####################################################################### #


#' geocode, but only if AOI package is installed and attached
#'   and what it imports like tidygeocoder etc.
#'
#' @param address vector of addresses, but tested only for 1
#' @param xy set it to TRUE if you want only x,y returned, see help for AOI pkg
#' @param pt  see help for AOI pkg, return geometry if set to TRUE, allowing map.
#'   param as provided is ignored and set to TRUE if aoimap=TRUE
#' @param aoimap  see help for AOI pkg, create map if set to TRUE
#' @param ...  passed to geocode() see help for AOI pkg
#'
#' @return returns tibble table of x,y or lat,lon values or geometries.
#'   see helpf for AOI pkg
#' @examples
#'   # only works if AOI package installed already and attached too
#'   # #test_addresses2b <- c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park")
#'   # #x <- geocode(test_addresses2b)
#'   # out <- ejamit(x, radius = 3)
#'   # fname = system.file("testdata/address/street_address_9.xlsx", package="EJAM")
#'
#' #x1 <- EJAMejscreenapi::read_csv_or_xl(fname)
#' #x2 <- latlon_any_format(fname)
#' #x3 <- latlon_from_anything(fname)
#' #names(x1)
#' #names(x2)
#'
#' @export
#'
latlon_from_address <- function(address, xy=FALSE, pt = FALSE, aoimap=FALSE, ...) {

  ############################################## #
  # make AOI package only optional ####
  ### all these are imported by AOI pkg that were not yet needed by EJAM:
  #
  #   c("datasets", "fipio", "htmlwidgets", "jsonlite", "leaflet.extras",
  #   "rnaturalearth", "rvest", "shiny", "terra", "tidygeocoder", "units")
  #
  # if (!require("AOI")) {
  #   remotes::install_github("mikejohnson51/AOI") #
  # }
  x <- try(find.package("AOI"))
  if (inherits(x, "try-error")) {
    warning("AOI package not available")
    x <- NULL
    ############################################## #
  } else {
    cat('for this to work you would need to use library(', 'AOI', ') first\n')
    # how to make it attached or used without triggering renv or packrat to think we want to import or depend on it?

    # x <- geocode(c("1200 Pennsylvania Ave, NW Washington DC", "Dupont Circle", "Research Triangle Park"))

    if (length(address) > 25) {stop("only 25 max supported in this function until decide if more ok")}

    if (aoimap) {
      x <- geocode(address, pt = TRUE, xy = xy, ...)   |> aoi_map()   # AOI:: # avoid making renv think we require it
    } else {
      x <- geocode(address, pt = pt, xy = xy, ...)
    }

    # geocode(xy=TRUE) does not work correctly for more than just 1 address, so fix output
    if (xy) {  # && length(address) > 1  ?? no
      x <- matrix(x, ncol = 2)
      x <- as.data.frame(x)
      colnames(x) <- c("x", "y")
    }
    x <- as.data.frame(x) # otherwise it is a tibble

    # convert x and y colnames to lon and lat
    names(x)[names(x) == "x"] <- "lon"
    names(x)[names(x) == "y"] <- "lat"
  }

  return(x)
}
####################################################################### #
