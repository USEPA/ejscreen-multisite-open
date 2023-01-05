ejampackages <- names(
  grep("EJAM.*", installed.packages()[,'Package'], value=TRUE)
  )
usethis::use_data(ejampackages)

