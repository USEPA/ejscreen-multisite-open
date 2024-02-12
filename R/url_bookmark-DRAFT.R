
#' utility - save bookmarked EJScreen session (map location and indicator)
#' 
#' @details  WORK IN PROGRESS - NOT USED AS OF EARLY 2023. 
#' You can use this function to create and save a json file that is a bookmark 
#' for a specific place/ map view/ data layer in EJScreen. 
#' You can later pull up that exact map in EJScreen by launching EJScreen, 
#' clicking Tools, Save Session, Load from File.
#' 
#' ***Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' 
#' Note: 
#' (1) The number of sessions that can be saved depends on the browser cache size. 
#' (2) Session files, if saved, are available from the default Downloads folder on your computer. 
#' (3) Users should exercise caution when saving sessions that may contain sensitive or confidential data.
#' 
#' @param ... passed to [url_bookmark_text()]
#' @param file path and name of .json file you want to save locally
#'
#' @return URL for 1 bookmarked EJScreen map location and variable displayed on map
#' 
#' @keywords internal
#'
url_bookmark_save <- function(..., file="ejscreenbookmark.json") {
  
  mytext <- url_bookmark_text(...)
  write(mytext, file = file)
  return(mytext)
  
  # example, at EJAM/inst/testdata/Sessions_Traffic in LA area.json
  # [{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":"ejindex_map","title":"Pollution and Sources","isDynamic":true,"layerType":"ejscreen","pctlevel":"nation","renderField":"B_PTRAF","renderIndex":4,"type":"map-image","url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer","visible":true,"opacity":0.5}],"graphics":[],"name":"Traffic in LA area"}]
  #
  # [{
  #   "extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},
  #   "basemap":"Streets",
  #   "layers":[
  #     {"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},
  #     {"id":"ejindex_map",
  #       "title":"Pollution and Sources",
  #       "isDynamic":true,
  #       "layerType":"ejscreen",
  #       "pctlevel":"nation",
  #       "renderField":"B_PTRAF",
  #       "renderIndex":4,
  #       "type":"map-image",
  #       "url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer",
  #       "visible":true,
  #       "opacity":0.5
  #     }
  #   ],
  #   "graphics":[],
  #   "name":"Traffic in LA area"
  # }]
  
}
################################################################## #


#' utility - URL for 1 bookmarked EJScreen session (map location and indicator)
#' 
#' @details 
#' WORK IN PROGRESS - NOT USED AS OF EARLY 2023. 
#' You can use this function to create and save a json file that is a bookmark 
#' for a specific place/ map view/ data layer in EJScreen. 
#' You can later pull up that exact map in EJScreen by launching EJScreen, 
#' clicking Tools, Save Session, Load from File.
#' 
#' Note: 
#' (1) The number of sessions that can be saved depends on the browser cache size. 
#' (2) Session files, if saved, are available from the default Downloads folder on your computer. 
#' (3) Users should exercise caution when saving sessions that may contain sensitive or confidential data.
#' 
#' @param x vector of approx topleft, bottomright longitudes in some units EJScreen uses? 
#'    Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' @param y vector of approx topleft, bottomright latitudes in some units EJScreen uses? 
#'    Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' @param name Your name for the map bookmark
#' @param title Your name for the map like Socioeconomic Indicators  or  Pollution and Sources
#' @param renderField name of variable shown on map, like B_UNEMPPCT for map color bins of percent unemployed
#'   or B_PTRAF for traffic indicator
#' @param pctlevel nation or state
#' @param xmin  calculated bounding box for map view
#' @param xmax  calculated bounding box for map view
#' @param ymin  calculated bounding box for map view
#' @param ymax  calculated bounding box for map view
#' @param urlrest Just use the default but it changes each year
#' @seealso [url_bookmark_save()]
#' @return URL for 1 bookmarked EJScreen map location and variable displayed on map
#' @examples \dontrun{
#'   url_bookmark_text()
#'   url_bookmark_save(
#'     x=c(-10173158.179197036, -10128824.702791695), 
#'     y=c(3548990.034736070,3579297.316451102), 
#'     file="./mysavedejscreensession1.json")
#'   }
#' 
#' @keywords internal
#'
url_bookmark_text <- function(
    x=c(-13232599.178424664, -13085305.024919074),
    y=c(3970069.245971938, 4067373.5829790044),
    # x=c(-172.305626, -59.454062),  # if longitude, zoomed way out to corners of USA plus some
    # y=c(63.774548, 16.955558), # if latitude, zoomed way out to corners of USA plus some
    name="BookmarkedEJScreenMap",
    title="Socioeconomic Indicators", # Pollution and Sources
    renderField="B_UNEMPPCT",   # B_PTRAF
    pctlevel="nation",
    xmin=1.1*min(x), # >1 because it is negative longitude in USA
    xmax=0.9*min(x), # <1 because it is negative longitude in USA
    ymin=0.9*min(y),
    ymax=1.1*min(y),
    urlrest=paste0("https://geopub.epa.gov/arcgis/rest/services", 
                   "/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer")
) {

  yrinurl <- gsub(".*v20(..).*", "20\\1", urlrest)
  yrnow <- substr(Sys.time(),1,4)
  if (yrnow > yrinurl + 1) {warning("Check that URL in url_bookmark_text() is updated to the latest dataset of EJScreen.")}
  # example, at EJAM/inst/testdata/Sessions_Traffic in LA area.json
  # [{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":"ejindex_map","title":"Pollution and Sources","isDynamic":true,"layerType":"ejscreen","pctlevel":"nation","renderField":"B_PTRAF","renderIndex":4,"type":"map-image","url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer","visible":true,"opacity":0.5}],"graphics":[],"name":"Traffic in LA area"}]
  #
  # [{
  #   "extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},
  #   "basemap":"Streets",
  #   "layers":[
  #     {"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},
  #     {"id":"ejindex_map",
  #       "title":"Pollution and Sources",
  #       "isDynamic":true,
  #       "layerType":"ejscreen",
  #       "pctlevel":"nation",
  #       "renderField":"B_PTRAF",
  #       "renderIndex":4,
  #       "type":"map-image",
  #       "url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer",
  #       "visible":true,
  #       "opacity":0.5
  #     }
  #   ],
  #   "graphics":[],
  #   "name":"Traffic in LA area"
  # }]
  
  # old urlrest was         "https://v18ovhrtay722.aa.ad.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2021/MapServer"  
  
  
  urltext <- paste0(
    '[{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},',
    
    '"xmin":',
    xmin,                   ###########   PARAMETER ################ #### #-10173158.179197036, ##################### #
    ',"ymin":',
    ymin,                   ###########   PARAMETER ################ ##### #3548990.0347360703, ##################### #
    ',"xmax":',
    xmax,                   ###########   PARAMETER ################ ##### #-10128824.702791695, ##################### #
    ',"ymax":',
    ymax,                   ###########   PARAMETER ################ ##### #3579297.316451102, ##################### #
    
    '},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":',
    '"', 
    'ejindex_map',  ###########  ???????????? ############ #
    '",',   
    '"title":"',
    title,                     ###########   PARAMETER ################ #
    '",',
    '"isDynamic":true,"layerType":',
    '"',
    'ejscreen',
    '",',
    '"pctlevel":"',
    pctlevel,                      ###########   PARAMETER ################ #
    '",',
    '"renderField":"',
    renderField,                  ###########   PARAMETER ################ #
    '",',
    '"renderIndex":4,"type":"map-image",',
    '"url":"',
    urlrest,                    ###########   PARAMETER ################ #
    '",',
    '"visible":true,"opacity":0.5}],"graphics":[],',
    '"name":"',
    name,
    '"',
    '}]'
  )
  return(urltext)
}
################################################################## #

