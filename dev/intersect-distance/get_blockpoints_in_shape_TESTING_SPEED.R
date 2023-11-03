

#  get_blockpoints_in_shape()  Was extremely slow as written as of 4/2023


##########################################
# load packages/ data

#    blockpoints_sf <- EJSCREENbatchdata   
# ::
# blockpoints_sf_make()

# which just does this:
  # blockpoints_sf <-  blockpoints |>
  #  sf::st_as_sf(coords = c("lon", "lat"), crs= 4269) # Geodetic CRS:  NAD83

# as .rda that took about 35 seconds to load, but only 5-10 seconds to create from  blockpoints like this.

# library(EJAM) # takes a long time to load if .onAttach() is forcing immediate loading of blockpoints and blockwts 

##########################################
# test points

n =  200   # how many facilities or sites?
rad =  1  # miles  or # rad5 <- units::set_units(5,"km")
pts=testpoints_1000[1:n,]

##########################################
# EJAM analysis

x = ejamit(pts, radius = rad)
out = x$results_bysite
mapfast(out,rad) # 
# mapfast(out,rad,column_names = "ej") # 


##########################################
# system.time({  

outfast <- get_blockpoints_in_shape(pts, addedbuffermiles = rad) 

# })


##########################################
# map blocks near each site

# library(
  
  # mapview

# )


polycircles <- shape_buffered_from_shapefile_points(shapefile_from_sitepoints(pts), rad) # several seconds
mapview::mapview(outfast, alpha.regions = 0.5, alpha=1, layer="blocks_nearby" ) + 
  mapview::mapview(shapefile_from_sitepoints(pts), layer="Sites", 
                   col.regions="black", color="red", alpha=1, alpha.regions = 0.8) + 
  mapview(polycircles, alpha.regions=0, color="black", col.regions="white")  



##########################################





################################## # 
# << 0.2 second for 3 sites 1 mile.  
# 1 second for 10 sites 3.1 miles.  
# 2.7 sec for 50 @3.1
# 5.2 sec for 100 @3.1 
################################## # 

# > system.time({  # even just 2 sites, only 1 mile, takes 2 minutes
#   +   outslow = get_blockpoints_in_shape(pts, addedbuffermiles =  rad)
#   + })
# user  system elapsed 
# 110.67    2.22  112.91   = 2 minutes

# > outslow

# Simple feature collection with 566 features and 3 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -77.04838 ymin: 38.88172 xmax: -73.77278 ymax: 41.07437
# Geodetic CRS:  NAD83
# First 10 features:

#         blockid siteid     sitename                   geometry   # this has blockid

# 1238865 1238865      1 Pulaski site POINT (-77.01352 38.90157)
# 1238866 1238866      1 Pulaski site POINT (-77.01261 38.90081)
# 1238870 1238870      1 Pulaski site POINT (-77.01309 38.89965)
# 1238871 1238871      1 Pulaski site  POINT (-77.01334 38.9007)
# 1238872 1238872      1 Pulaski site  POINT (-77.01303 38.8991)
# 1238873 1238873      1 Pulaski site POINT (-77.01394 38.90148)
# 1238874 1238874      1 Pulaski site POINT (-77.01426 38.90131)
# 1238875 1238875      1 Pulaski site POINT (-77.01475 38.90131)
# 1238876 1238876      1 Pulaski site POINT (-77.01568 38.90192)
# 1238877 1238877      1 Pulaski site POINT (-77.01569 38.90075)

# > outfast

# Simple feature collection with 566 features and 3 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -77.04838 ymin: 38.88172 xmax: -73.77278 ymax: 41.07437
# Geodetic CRS:  NAD83
# First 10 features:

#    siteid_fromgetblocksnearby siteid     sitename                   geometry  # this lacks blockid

# 4                           1      1 Pulaski site POINT (-77.01352 38.90157)
# 5                           1      1 Pulaski site POINT (-77.01261 38.90081)
# 9                           1      1 Pulaski site POINT (-77.01309 38.89965)
# 10                          1      1 Pulaski site  POINT (-77.01334 38.9007)
# 11                          1      1 Pulaski site  POINT (-77.01303 38.8991)
# 12                          1      1 Pulaski site POINT (-77.01394 38.90148)
# 13                          1      1 Pulaski site POINT (-77.01426 38.90131)
# 14                          1      1 Pulaski site POINT (-77.01475 38.90131)
# 15                          1      1 Pulaski site POINT (-77.01568 38.90192)
# 16                          1      1 Pulaski site POINT (-77.01569 38.90075)
# > 
#   
# setdiff(outfast$blockid, outslow$blockid) 
# NULL

