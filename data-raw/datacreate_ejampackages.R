
## full list of possibly relevant packages

ejampackages <- names(
  grep("EJAM.*", installed.packages()[, 'Package'], value = TRUE)
  )
cat(paste0(" full list of possibly relevant packages: \n
  c('", paste0(ejampackages, collapse = "', '"), "')\n"))
cat('\n')
## the most essential ones now that EJAMejscreenapi will not be required by EJAM

ejampackages <- c("EJAM",
                  "EJAMbatch.summarizer")

cat(paste0(" critical EJAM-related packages being saved as 'ejampackages' are these: \n
  ejampackages <- c('", paste0(ejampackages, collapse = "', '"), "')\n"))
cat('\n')

metadata_add(ejampackages)
usethis::use_data(ejampackages, overwrite = TRUE)

