
############### script ######### #

#  MUST HAVE THESE FUNCTIONS - THEY ARE IN EJAM BUT NOT EXPORTED. YOU CAN USE THEM VIA 
#    EJAM  :::  frs_read  ETC. 

# date <- Sys.Date()

frs <- frs_read(fullpath = 'NATIONAL_SINGLE.CSV' , only_essential_cols = T)

frs <- frs_clean(frs)

attr(frs, 'download_date') <- date
attr(frs, 'released')      <- date
usethis::use_data(frs, overwrite = TRUE)
