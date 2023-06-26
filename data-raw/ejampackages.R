ejampackages <- names(
  grep("EJAM.*", installed.packages()[,'Package'], value=TRUE)
  )
# ejampackages <- ejampackages[ejampackages != "EJAMfrsdata"]
usethis::use_data(ejampackages, overwrite = TRUE)

