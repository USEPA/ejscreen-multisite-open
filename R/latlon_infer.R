latlon_infer <- function(mycolnames) {
  x <- mycolnames
  
  # Latitude, Lat, latitude, long, longitude, Longitude, Long, LONG, LAT, etc. 
  
  infer <- function(lword, x) {
    if (!(lword %in% x)) {
      if (lword == 'lat') {
        # try to infer lat, using these in order of preferred to less
        aliases <- c('lat', 'latitude83', 'latitude', 'latitudes', 'lats')
      }
      if (lword == 'lon') {
        # try to infer lon, using these in order of preferred to less
        aliases <- c('lon', 'longitude83', 'longitude', 'longitudes', 'long', 'longs', 'lons')
      }
      
      bestfound <- intersect(aliases, tolower(x))[1]
      if (is.na(bestfound)) { # intersect()[1] returns NA if none
        warning(paste0(lword, ' missing and no synonyms found')) # do not change x at all
      } else {
        # ignoring case, replace any exact match(es) to that one word. # should ideally confirm unique?
        x <- gsub(paste0('^', bestfound, '$'), lword, x, ignore.case = TRUE)
      }
    }
    if (sum(grepl(paste0('^', lword, '$'), x)) > 1) {warning(paste0('DUPLICATED ', lword))}
    x
  }
  
  x <- infer('lat', x)
  x <- infer('lon', x)
  x

}
             
## tests
if (1==9) {
  latlon_infer(c('trilat', 'belong', 'belong')) # warns if no alias found. Does not warn of dupes in other terms, just preferred term.
  latlon_infer(c('a', 'LONG', 'Longitude', 'lat')) # only the best alias is converted/used
  latlon_infer(c('a', 'LONGITUDE', 'Long', 'Lat')) # only the best alias is converted/used
  latlon_infer(c('a', 'longing', 'Lat', 'lat', 'LAT')) # case variants of preferred are left alone only if lowercase one is found
  latlon_infer(c('LONG', 'long', 'lat')) # case variants of a single alias are converted to preferred word (if pref not found), creating dupes!  warn!
  latlon_infer(c('LONG', 'LONG')) # dupes of an alias are renamed and still are dupes! warn!
  latlon_infer(c('lat', 'lat', 'Lon')) # dupes left as dupes but warn!
}
