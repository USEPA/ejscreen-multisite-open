#' Simple map popup from a data.table or data.frame, one point per row
#' Creates popup vector that leaflet::addCircles or leaflet::addPopups can use.
#' Works similarly to EJAMejscreenapi::popup_from_df, but now extends to data.table
#' @details Each popup is made from one row of the data.frame. 
#'   Each popup has one row of text per column of the data.frame
#' @param x, a data table or data frame
#' @param n Show the first n columns of mypoints, in popup. "all" means all of them.
#' @param labels default is colnames(x) - vector used to label 
#'   the elements in the popup. Must be same length as column_names
#' @param column_names default is all, or a vector of column names from x to use 
#'
#' @return A vector of strings, one per row or map point, 
#'   with a line break separating column elements
#' @export
#'
#' @examples  
#'  dat <- data.table(
#'    RegistryId = c("110071102551", "110015787683"),
#'    FacilityName = c("USDOI FWS AK MARITIME NWR etc", "ADAK POWER PLANT"),
#'    LocationAddress = c("65 MI W. OF ADAK NAVAL FACILITY", "100 HILLSIDE BLVD"),
#'    CityName = c("ADAK", "ADAK"),
#'    CountyName = c("ALEUTIAN ISLANDS", "ALEUTIANS WEST"),
#'    StateAbbr = c("AK", "AK"),
#'    ZipCode = c("99546", "99546"),
#'    FIPSCode = c("02010", "02016"),
#'    lat = c(51.671389,51.8703), lon = c(-178.051111, -176.659),
#'    SupplementalLocation = c(NA_character_,NA_character_))
#'  
#'  ## add popups only
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addPopups(popup = popup_from_any(dat))
#'  
#'  ## add circles with clickable popups
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addCircles(popup = popup_from_any(dat))
#'  
#'  ## convert to data frame, works the same way 
#'  dat_df <- as.data.frame(dat_df)
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addCircles(popup = popup_from_any(dat))

popup_from_any <- function (x, column_names = names(x), labels = column_names, n = "all") {
  
  ## if input is a data.table object
  if(is.data.table(x)){
    if (n == "all" | n > NCOL(x)) {
    } else {
      x <- x[1:n]
    }
    
    if (any(!(column_names %in% names(x)))) {
      stop("some column_names not found in x for popup")
    }
    if (length(labels) != length(column_names)) {
      labels = column_names
      warning("column_names and labels must be same length. Using column_names as labels.")
    }
    
    ## subset columns with data.table syntax
    x <- x[, ..column_names]

    ## create vector of popups with column labels, length=# of rows of data.table
    popup_vec <- sapply(1:NROW(x), function(row_num) paste(labels, x[ row_num], sep=': ', collapse='<br>'))
    
  } else if(is.data.frame(x)){
    
    if (n == "all" | n > NCOL(x)) {
    } else {
      x <- x[, 1:n]
    }
    
    if (any(!(column_names %in% names(x)))) {
      stop("some column_names not found in x for popup")
    }
    if (length(labels) != length(column_names)) {
      labels = column_names
      warning("column_names and labels must be same length. Using column_names as labels.")
    }
    
    ## subset columns with data.frame syntax
    x <- x[, column_names]
    
    ## create vector of popups with column labels, length=# of rows of data.frame
    popup_vec <- sapply(1:NROW(x), function(row_num) paste(labels, x[row_num,], sep=': ', collapse='<br>'))
  } else {
    stop('Please pass in a data.table or data.frame object')
  }
 
 return(popup_vec)
}
