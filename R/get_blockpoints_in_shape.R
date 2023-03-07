get_blockpoints_in_shape <- function(polys, addedbuffermiles=0) {
  if (!exists("blockpoints")) {stop("requires the blockpoints data.table called EJAMblockdata::blockpoints ")}
  
  # CHECK FORMAT OF polys - ensure it is spatial object (with data.frame/ attribute table? ) ? 
  
  
  
  # ensure it has unique IDs called siteid column, or else add that column  
  
  
  
  
  # MAYBE MORE EFFICIENT TO 1st intersect with blockgroups or even counties,
  # and then use those bg or counties to filter to a smaller table of blocks
  # and then find from among just those blocks which ones are in the polygons.
  
  # also could try is_within_distance()  to maybe find those blocks inside not just near a poly?
  
  
  # examples / testing ideas
  # testsites <- EJAMfrsdata::frs[sample(1:nrow(EJAMfrsdata::frs), 1e3),]
  # testsites$siteid <- 1:nrow(testsites)
  # testsites <- sf::st_as_sf(testsites, coords=c("lon","lat"), crs=4269)
  # myrad5 <- units::set_units(5,"km")
  # # myrad10 <- units::set_units(10,"km")
  # # myrad50 <- units::set_units(50,"km")
  # testpolycircles <- get_shape_buffered(testsites, radius = myrad5 ) # several seconds
  # testblocks <- EJAMblockdata::blockpoints # entire usa
  # # testblocks <- EJAMblockdata::blockpoints[sample(1:nrow(frs), 1e5),]
  # testblocks <- sf::st_as_sf(testblocks, coords=c("lon","lat"), crs=4269) # several seconds
  
  # results1  <- testblocks |> sf::st_join(testpolycircles)    # a couple minutes  ********************
  
  # results2  <- sf::st_join(testpolycircles, testblocks)     # THIS TAKES ESSENTIALLY FOREVER !!  ********************
  
  # sum(is.na(results1$siteid))/nrow(results1)
  # # [1] 0.87
  # # 87% of blocks in US   had none of these 1000 sites near them (within 5 km)
  # length(unique(results2$siteid))
  # length(unique(results2$blockid))
  # 
  
  if (addedbuffermiles > 0) {
    addedbuffermiles <- units::set_units(addedbuffermiles, "miles")
    polys <- get_shape_buffered(polys, radius = addedbuffermiles)
  }  
  
  # use   sf::st_intersects() or st_join(, join=intersects) 
  blockz  <- EJAMblockdata::blockpoints |>
    sf::st_as_sf(coords = c("lon", "lat"))     #  , crs = sf::st_crs(other_shapefile))
  polys <- sf::st_join(blockz, polys)
  
  return(polys)
}

