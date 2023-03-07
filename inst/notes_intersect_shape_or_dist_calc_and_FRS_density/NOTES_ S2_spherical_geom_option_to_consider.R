
# Points in S2 are modeled as lying on a perfect sphere -
#   much more efficient than doing the trig needed for lat lon data. 

# The sf R package can use the s2 R package that uses Google's s2Geometry library.

# you could use s2 directly but easier to just use it via sf::
# (unless you need real low level details from s2  )
library(sf)
library(s2)
cities <- st_as_sfc(s2_data_cities())
city_names <- s2_data_tbl_cities$name
canada <- st_as_sfc(s2_data_countries("Canada"))
city_names[as.matrix(st_intersects(cities, canada))]
# [1] "Ottawa"    "Vancouver" "Toronto"  

# st_is_within_distance()  is exact and extremely fast, one speaker said, but other said s2 is  SLOW in SOME situations,  including
#   "distance within x km" -- slow if a lot of points. They are working on speeding that up. https://www.youtube.com/watch?v=zqRhF2XM1CE 
# open issues https://github.com/r-spatial/sf/issues/1771   
# some people used st_buffer() to find nearby stuff and then st_intersects() but that is slow.
# st_is_within_distance()  is exact and extremely fast. 

# s2: Spherical Geometry Operators Using the S2 Geometry Library (FOR R)

# The sf package uses s2 for geographic coordinates if you say  sf::sf_use_s2(TRUE), 
# and will become the default after sf version 1.0.0.
# The sf package also supports creating s2 vectors using as_s2_geography() # and maybe also see sf::st_sfc())  and   
#  sf::st_as_s2(x, ...)  converts an sf POLYGON object into a form readable by s2.

 browseURL("https://r-spatial.github.io/sf/")  
 browseURL("https://r-spatial.github.io/s2/")  
 browseURL("https://cran.r-project.org/web/packages/s2/index.html")

  # The s2 R package provides bindings to Google's S2Geometry library. The package exposes an API similar to Google's BigQuery Geography API, whose functions also operate on spherical geometries.
 
# Provides R bindings for Google's s2 library for geometric calculations on the sphere. High-performance constructors and exporters provide high compatibility with existing spatial packages, transformers construct new geometries from existing geometries, predicates provide a means to select geometries based on spatial relationships, and accessors extract information about geometries.
# 

# https://s2geometry.io/about/overview
# Code for S2 is located in the following repositories:
#   Reference Implementations:
#   C++ S2 Library
# Ported Subsets:
#   Java S2 Library
#   Go S2 Library
#   Python S2 Library
# Running the S2 code requires the following:
#   A MacOSX or Linux platform. (Windows is not supported at this time.)
#    **** BUT THE R PACKAGE SEEMS TO PROVIDE THIS APPROACH FOR WINDOWS/R/ETC.



# Points in S2 are modeled as lying on a perfect sphere -
#   much more efficient than doing the trig needed for lat lon data. 

# 
# S2 offers: 
#  - Fast in-memory indexing of collections of points, polylines, and polygons.
# 
#  - Algorithms for measuring distances and finding nearby objects.
# 
#  - Robust algorithms for snapping and simplifying geometry (with accuracy and topology guarantees).
# 
#  - A collection of efficient yet exact mathematical predicates for testing relationships among geometric objects.
# 
#  - Support for spatial indexing, including the ability to approximate regions as collections of discrete “S2 cells”. This feature makes it easy to build large distributed spatial indexes.

 # Conversions to/from common GIS formats. (To read such formats, use an external library such as OGR.)
 

# Unit Vectors vs. Latitude/Longitude Pairs
# 
# In S2, points are represented internally as unit-length vectors (points on the surface of a three-dimensional unit sphere) as opposed to traditional (latitude, longitude) pairs. This is for two reasons:
#   
#   Unit vectors are much more efficient when working with geodesic edges. Using (latitude, longitude) pairs would require constantly evaluating trigonometric functions (sin, cos, etc), which is slow even on modern CPU architectures.
# 
# Unit vectors allow exact geometric predicates to be evaluated efficiently. To do this with (latitude, longitude) pairs, you would essentially need to convert them to the unit vector representation first.
# 
# For precision and efficiency, coordinates are represented as double-precision numbers. Robustness is achieved through the use of the techniques mentioned previously: conservative error bounds (which measure the error in certain calculations), exact geometric predicates (which determine the true mathematical relationship among geometric objects), and snap rounding (a technique that allows rounding results to finite precision without creating topological errors).
# 
# Classes vs. Functions
# Most algorithms in S2 are implemented as classes rather than functions. So for example, rather than having a GetDistance function, there is an S2ClosestEdgeQuery class with a GetDistance method. This design has two advantages:
#   
#   It allows caching and reuse of data structures. For example, if an algorithm needs to make thousands of distance queries, it can declare one S2ClosestEdgeQuery object and call its methods thousands of times. This provides the opportunity to avoid recomputing data that can be used from one query to the next.
# 
# It provides a convenient way to specify options (via a nested Options class), and to avoid respecifying those options many times when repeated operations need to be performed.
# 
# Geocentric vs. Geodetic Coordinates
# Geocentric and geodetic coordinates are two methods for defining a (latitude, longitude) coordinate system over the Earth’s surface. The question sometimes arises: which system does the S2 library use?
#   
#   The answer is: neither (or both). Points in S2 are modeled as lying on a perfect sphere (just as points in a traditional planar geometry library are modeled as lying on a perfect plane). How points are mapped onto the sphere is outside the scope of S2; it works equally well with geocentric or geodetic coordinates. (Actual geographic datasets use geodetic coordinates exclusively.)
