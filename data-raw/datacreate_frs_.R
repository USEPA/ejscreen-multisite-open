###################################################################################
# SCRIPT TO READ AND CLEAN LATEST FRS (and FRS BY SIC) AND SAVE FOR USE AS DATASETS
###################################################################################

# Note: As of 12/23, EJAM will no longer store some key frs files as package EJAM/data/
#  but as pins on connect server board.

################################################################################ # 
## DOWNLOAD FRS info AND UPDATE/CREATE & SAVE LOCAL FILES for frs-related datasets
################################################################################ # 
#
## library(## EJAMfrsdata)
mydir <- "~/../Downloads/arrow_frs"
if (!dir.exists(mydir)) {dir.create(mydir)}
if (!exists("alreadygot")) {alreadygot <- FALSE; mytemp <- tempdir()}
frs_update_datasets(folder = mytemp, # default would use a tempdir() but not return its name
                    downloaded_and_unzipped_already = alreadygot,
                    folder_save_as_arrow = mydir,
                    save_as_arrow_frs              = TRUE,
                    save_as_arrow_frs_by_programid = TRUE,
                    save_as_arrow_frs_by_mact      = TRUE,
                    save_as_arrow_frs_by_naics     = TRUE,
                    save_as_arrow_frs_by_sic       = TRUE,
                    save_as_data_frs              = FALSE,
                    save_as_data_frs_by_mact      = FALSE,
                    save_as_data_frs_by_naics     = FALSE,
                    save_as_data_frs_by_programid = FALSE,
                    save_as_data_frs_by_sic       = FALSE)
alreadygot <- TRUE
dir(mydir)
# rm(mydir)

##################################### # 
# frsprogramcodes.rda
#
# Manually also need to save updated frsprogramcodes.rda
#  see EJAM/data-raw/datacreate_frsprogramcodes.R
#
# and may need to update counts! ... see ???


################################################################################ # 
##  LOAD dataset FILES INTO MEMORY (If saved as .arrow locally but not kept in memory)
################################################################################ # 
#
fold <- mydir
frs_vars <- c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
for (varname in frs_vars) {
  fname <- paste0(varname, ".arrow")
  assign(varname, value = arrow::read_ipc_file(file = file.path(fold, fname)))
}

################################################################################ # 
## WRITE .arrow FILES TO pins BOARD on Posit Connect server, once loaded in memory
################################################################################ # 
# 
# THAT IS DONE BY SCRIPT  IN  
# EJAM/data-raw/datacreate_pins.R 
 
