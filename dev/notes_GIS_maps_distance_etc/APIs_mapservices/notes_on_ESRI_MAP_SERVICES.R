

# Accessing ArcGIS REST services using R
"https://community.esri.com/t5/gis-life-blog/accessing-arcgis-rest-services-using-r/ba-p/898451"


############ Examples of pulling in WFS (for tribal layer) and WMS with 
#  sf, mapview, leaflet:

"https://mhweber.github.io/RGroupLighningTalk/WFS_Maps.nb.html"
# and 
"https://mhweber.github.io/RGroupLighningTalk/WMS_Tiles.html"

########################### # ########################### # ########################### # ########################### # 

###########  ways to add ESRI feature layers to a leaflet map in R:

# These examples will add the USA counties feature layer to your map.

{########################### # ########################### # ########################### # 
# * Use the leaflet.extras package: 

  devtools::install_github("bhaskarvk/leaflet.extras")
  
  library(leaflet)
  library(leaflet.extras)
  m <- leaflet() %>% addTiles() %>% addEsriFeatures(
    url = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties/FeatureServer/0"
    ) 
  m
}
{  ########################### # ########################### # ########################### # 
# * Use the esri leaflet plugin:   allows you to add ESRI feature layers to a leaflet map.
  
  devtools::install_github("Esri/esri-leaflet")

library(leaflet) 
  library(esri.leaflet) 
  m <- leaflet() %>% addTiles() %>% addEsriFeatureLayer(
    url = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties/FeatureServer/0"
    ) 
  m
}
{  ########################### # ########################### # ########################### #
# * Use the geojsonio package:    
  
  # If you have access to the ESRI feature layer data in GeoJSON format, you can 
  # use the geojsonio package to convert it to a format that can be used with leaflet. 
 
  library(leaflet)
  library(geojsonio)
  
  url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties/FeatureServer/0/query?where=1=1&outFields=*&f=geojson"
  
  geojson <- geojson_read(url)
  m <- leaflet() %>% addTiles() %>% addGeoJSON(geojson)
  m
} 

  ########################### # ########################### # ########################### # ########################### # 
  
  # MAP NEARBY BLOCK POINTS THEN OVERLAY THE BLOCK GROUP SHAPES

y <- plotblocksnearby(testpoints_10[5,], 
                      radius = 3.1,
                      returnmap = TRUE)

{# Extract block group fips of those block groups via the parent bgid numbers of those blocks
  # blockids <-      as.vector(sapply( y$x$calls[[2]]$args[[7]], function(z)   gsub(".*blockid: ([0-9]*)<.*", "\\1", z)))
}
  bgids <-  unique(as.vector(sapply(y$x$calls[[2]]$args[[7]], function(z) gsub(   ".*bgid: ([0-9]*)<.*", "\\1", z))))
  bgfips <- bgid2fips[bgid %in% bgids, bgfips]
  
  x <- map_blockgroups(bgfips, outFields = "*")
  
  # add those FIPS shapes to the leaflet htmlwidget map 
  
  mymap <-   y %>% 
    leaflet::addGeoJSON(geojsonio::geojson_json(x), color = "green", group = "Blockgroups", data = x) %>% 
    addLayersControl(overlayGroups = "Blockgroups")

  mymap
  
  
  ########################### # ########################### # ########################### # ########################### # 
  
