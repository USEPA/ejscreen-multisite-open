https://s2geometry.io/about/overview

Points in S2 are modeled as lying on a perfect sphere - much more efficient than doing the trig needed for lat lon data. 

Conversions to/from common GIS formats. (To read such formats, use an external library such as OGR.)

S2 offers: 
 - Fast in-memory indexing of collections of points, polylines, and polygons.

 - Algorithms for measuring distances and finding nearby objects.

 - Robust algorithms for snapping and simplifying geometry (with accuracy and topology guarantees).

 - A collection of efficient yet exact mathematical predicates for testing relationships among geometric objects.

 - Support for spatial indexing, including the ability to approximate regions as collections of discrete “S2 cells”. This feature makes it easy to build large distributed spatial indexes.



Unit Vectors vs. Latitude/Longitude Pairs

In S2, points are represented internally as unit-length vectors (points on the surface of a three-dimensional unit sphere) as opposed to traditional (latitude, longitude) pairs. This is for two reasons:
  
  Unit vectors are much more efficient when working with geodesic edges. Using (latitude, longitude) pairs would require constantly evaluating trigonometric functions (sin, cos, etc), which is slow even on modern CPU architectures.

Unit vectors allow exact geometric predicates to be evaluated efficiently. To do this with (latitude, longitude) pairs, you would essentially need to convert them to the unit vector representation first.

For precision and efficiency, coordinates are representated as double-precision numbers. Robustness is achieved through the use of the techniques mentioned previously: conservative error bounds (which measure the error in certain calculations), exact geometric predicates (which determine the true mathematical relationship among geometric objects), and snap rounding (a technique that allows rounding results to finite precision without creating topological errors).

Classes vs. Functions
Most algorithms in S2 are implemented as classes rather than functions. So for example, rather than having a GetDistance function, there is an S2ClosestEdgeQuery class with a GetDistance method. This design has two advantages:
  
  It allows caching and reuse of data structures. For example, if an algorithm needs to make thousands of distance queries, it can declare one S2ClosestEdgeQuery object and call its methods thousands of times. This provides the opportunity to avoid recomputing data that can be used from one query to the next.

It provides a convenient way to specify options (via a nested Options class), and to avoid respecifying those options many times when repeated operations need to be performed.

Geocentric vs. Geodetic Coordinates
Geocentric and geodetic coordinates are two methods for defining a (latitude, longitude) coordinate system over the Earth’s surface. The question sometimes arises: which system does the S2 library use?
  
  The answer is: neither (or both). Points in S2 are modeled as lying on a perfect sphere (just as points in a traditional planar geometry library are modeled as lying on a perfect plane). How points are mapped onto the sphere is outside the scope of S2; it works equally well with geocentric or geodetic coordinates. (Actual geographic datasets use geodetic coordinates exclusively.)
