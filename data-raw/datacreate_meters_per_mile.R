
meters_per_mile <-  convert_units(1, from = 'miles', towhat = 'meters') # 1609.344  convert_units()

usethis::use_data(meters_per_mile, internal = FALSE, overwrite = TRUE)

