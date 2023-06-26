
## copy of EJAMfrsdata pkg func called frs_clean but with SIC information
frs_clean <- function(frs, usefulcolumns=c('LATITUDE83', 'LONGITUDE83', 'REGISTRY_ID', 'PRIMARY_NAME', 'NAICS_CODES', 'SIC_CODES','PGM_SYS_ACRNMS')) {
  
  # REGISTRY_ID = 'c',
  # PRIMARY_NAME = 'c',   #   # frs$PRIMARY_NAME <- NULL # save space ?  825 MB VS 870 MB
  # PGM_SYS_ACRNMS = 'c',  # csv like  AIR:AK0000000201000026, AIRS/AFS:0201000026, NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
  # INTEREST_TYPES = 'c', # csv like "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR, UNSPECIFIED UNIVERSE"
  # NAICS_CODES = 'c',  # csv of NAICS
  # NAICS_CODE_DESCRIPTIONS = 'c', # csv?
  # SIC_CODES = 'c',  #csv
  # SIC_CODE_DESCRIPTIONS = 'c',  #csv
  # LATITUDE83 = 'd',   ###
  # LONGITUDE83 = 'd'
  
  frs <- frs[ , ..usefulcolumns]
  
  # Rename to column names used by this package ####
  setnames(frs, 'LATITUDE83', 'lat')
  setnames(frs, 'LONGITUDE83', 'lon')
  setnames(frs, 'NAICS_CODES', 'NAICS')
  setnames(frs, 'SIC_CODES', 'SIC')
  
  cat('Finished cleaning file.\n')
  # DROP ROWS LACKING lat/lon  ?####
  cat('Total rows: ', NROW(frs), '\n')  # >4 mill.
  # about 25% of all rows (870 MB vs 1160 MB)
  frs <- frs[!is.na(frs$lat), ]  
  cat('Rows with lat/lon: ', NROW(frs), '\n') # 3.4mill
  
  #   drop rows that are clearly INACTIVE sites 
  
  # done by frs_get()?
  
  data.table::setkey(frs, REGISTRY_ID)
  return(frs)
}

# stop('EJAMfrsdata package provides the function frs_read')
frs <-  frs_read(fullpath = 'NATIONAL_SINGLE.CSV' , only_essential_cols = T)

frs <- frs_clean(frs)

attr(frs, 'download_date') <- date
attr(frs, 'released') <- date
usethis::use_data(frs, overwrite=T)