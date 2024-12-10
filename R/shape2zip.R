

#############################################################################

### fix the draft of shape2zip() 

# ## ***  fix this and/or other functions so that this will work
# ## even if user provides a file name with .shp.zip in it
# 
# shptest = shapes_from_fips(fips = name2fips(c('tucson,az', 'tempe, AZ')))
# 
# testfilename1 = shape2zip(shp = shptest, file = './testjunk.shp.zip')
# zip::zip_list(testfilename1)
# shptest1 = shapefile_from_any(testfilename1)
# 
# testfilename2 = shape2zip(shp = shptest, file = './testjunk.zip')
# zip::zip_list(testfilename2)
# shptest2 = shapefile_from_any(testfilename2)

#############################################################################


shape2zip <- function(shp, file = file.path(".", "shapefile.zip")) {
  
  
  ## example/ test
  # shp = shapes_from_fips(fips = name2fips(c('tucson,az', 'tempe, AZ')))
  # file = '~/../Downloads/shapefile1.zip'
  
  folder = dirname(file)
  fname = basename(file)
  fname_noext <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", fname) 
  fname.zip = paste0(fname_noext, '.zip')
  fname.shp = paste0(fname_noext, '.shp')
  fnames.all = paste0(fname_noext, c(".shp", '.dbf', '.prj', '.shx'))
  
  tds = tempdir()
  for (fil in file.path(tds, fnames.all)) {
    if (file.exists(fil)) {file.remove(fil)}
  }
  # if (file.exists(file.path(tds, fname.shp))) {file.remove(file.path(tds, fname.shp))}
  sf::st_write(
    obj = shp,
    dsn = file.path(tds, fname = fname.shp),
    append = FALSE, delete_layer = TRUE
  )
  ## fname_noext_found <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", dir(tds, pattern = fname))  # this would be the version found
  ## fnames <- dir(tds, pattern = fname_noext_found)
  # fnames <- dir(tds, pattern = fname_noext)
  # fnames <- fnames[!grepl("zip$", fnames)]
  fnames <- fnames.all
  
  suppressWarnings({
    zipfullpath <- normalizePath(paste0(normalizePath(folder), "/", fname.zip))
    if (file.exists(zipfullpath)) {file.remove(zipfullpath)}
  })
  zip(zipfullpath, files = file.path(tds, fnames), extras = c('-j', '-D'))
  # unzip from tempdir to folder specified by parameter.
  # -D should prevent storing Directory info, 
  # -j is supposed to use no path info so files are all in root of .zip and there are not folders inside the .zip
  if (!file.exists(zipfullpath)) {warning('failed to create file at ', zipfullpath)} else {cat('saved', zipfullpath, '\n')}
  return(zipfullpath)
}
