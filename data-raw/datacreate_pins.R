#################################################################### #
# create pins board and save large data files there in .arrow format
#################################################################### #

# documentation on pins ####
# as a way to store data 
#
# https://docs.posit.co/connect/user/content-settings/
# https://pins.rstudio.com/reference/board_connect.html
# https://pins.rstudio.com/articles/posit-connect.html
# https://docs.posit.co/connect/how-to/pins/
# ~ ####
#################################################################### #

# UPDATE OR GET DATASETS ####

library(EJAM)
library(pins)

## 1st update/ create (or just read in) latest versions of datasets 
## using scripts as listed in  
#      EJAM/data-raw/datacreate_0_REGULAR_UPDATES_NOTES.R
## or 
## e.g. read from local data folder or wherever
# load('data/blockgroupstats.rda')  etc.
### or 
# dataload_from_local("all") 
### or
### assuming newest versions are on pins board ! 
# dataload_from_pins("all", ignorelocal = TRUE)

############################################################### #

# bgej ####
#
#  bgej was rda  here, but now is .arrow in pins board
# https://github.com/USEPA/EJAM/blob/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# https://github.com/USEPA/EJAM/raw/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# load("~/../Downloads/bgej.rda")
# > object.size(bgej)
# 123466744 bytes

##################################### # 
# frs etc. ####
#  frs, and frs_by_programid and 
# frs_by_sic and frs_by_naics and frs_by_mact  are much smaller files. 
# Updated/created using  EJAMfrsdata package functions
#  as used in script in  EJAM/data-raw/datacreate_frs_.R
#
# frs datasets once created and in memory, were written to pins board with script below.

##################################### # 
# (frsprogramcodes.rda is tiny and not a table so cannot use .arrow for it and not in pins board
#  - see EJAM/data-raw/datacreate_frsprogramcodes.R )

#################################################################### #

stop()
####################################################### #
# assuming newest versions are on pins board ! 
dataload_from_pins("all", ignorelocal = TRUE)

#         save_huge_data_files
# script to put all datasets from pins into a data folder in 
# EJAM-opensource repository/package

# To update a particular local folder, e.g.:

setwd("./../EJAM-opensource/data")

setwd("~/../Downloads/EJAMbigfiles")

arrow::write_ipc_file(blockwts,     "blockwts.arrow")
arrow::write_ipc_file(blockpoints,  "blockpoints.arrow")
arrow::write_ipc_file(blockid2fips, "blockid2fips.arrow")
arrow::write_ipc_file(quaddata,     "quaddata.arrow")

arrow::write_ipc_file(bgej,         "bgej.arrow")
arrow::write_ipc_file(bgid2fips,    "bgid2fips.arrow")

arrow::write_ipc_file(frs,              "frs.arrow")
arrow::write_ipc_file(frs_by_programid, "frs_by_programid.arrow")
arrow::write_ipc_file(frs_by_naics,     "frs_by_naics.arrow")
arrow::write_ipc_file(frs_by_sic,       "frs_by_sic.arrow")
arrow::write_ipc_file(frs_by_mact,      "frs_by_mact.arrow")

####################################################### #


#################################################################### #

# CREATE/SPECIFY BOARD ####

# instead of storing in aws dmap data commons
library(pins)
board <- pins::board_connect(auth = "auto")   # uses  "rsconnect"
# confirm you can see it:
board %>% pins::pin_browse("Mark")

## connect to board linked to posit connect

board <- board_connect(versioned = TRUE)

## other options for creating boards: 
## board_local() - link to local file folder
## board_folder() - link to dropbox or network drive
## board_s3() - link to S3 bucket, such as EPA Data commons
## board_url() - build board from data URLs, allow read-only access to datasets
############################################################### #

## list all datasets in a board
pin_list(board = board)

## print board or dataset metadata
board
# pin_meta(board = board, name = 'blockgroupstats_rds')

## show versions of a dataset
# pin_versions(board = board, name = 'blockgroupstats_rds')

## read datasets back in from the board
# system.time({bgstats <- board %>% pin_read('blockgroupstats_rds')})
# system.time({bgstats <- board %>% pin_read('blockgroupstats_arrow')})


#################################################################### #
# WRITE DATA TO BOARD ####
#################################################################### #

## FRS DATA   ####

board %>% 
  pins::pin_write(x = frs, 
            name = "frs", type = "arrow", 
            title = "frs data from EJScreen for EJAM", 
            description = "data.table -- See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = frs_by_programid, 
            name = "frs_by_programid", type = "arrow", 
            title = "frs_by_programid data from EJScreen for EJAM", 
            description = "data.table -- See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = frs_by_naics, 
            name = "frs_by_naics", type = "arrow", 
            title = "frs_by_naics data from EJScreen for EJAM", 
            description = "data.table -- See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = frs_by_sic, 
            name = "frs_by_sic", type = "arrow", 
            title = "frs_by_sic data from EJScreen for EJAM", 
            description = "data.table -- See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = frs_by_mact, 
            name = "frs_by_mact", type = "arrow", 
            title = "frs_by_mact data from EJScreen for EJAM", 
            description = "data.table -- See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
################### # 

# BLOCKGROUP DATA             note   bgej  was a tibble but replaced 12/5/23 with as.data.frame(bgej) version

board %>% 
  pins::pin_write(x = bgej, 
            name = "bgej", type = "arrow", 
            title = "bgej data from EJScreen for EJAM", 
            description = "data.frame -- approx 243k blockgroups, like blockgroupstats but for EJ Index raw scores, with bgfips, bgid, etc. - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )

board %>% 
  pins::pin_write(x = bgid2fips, 
            name = "bgid2fips", type = "arrow", 
            title = "bgid2fips data for EJAM", 
            description = "data.table of approx 242k blockgroups with Census FIPS for each blockgroup ID - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
################### # 

# BLOCKS DATA

board %>% 
  pins::pin_write(x = blockid2fips, 
            name = "blockid2fips", type = "arrow", 
            title = "blockid2fips data for EJAM", 
            description = "data.table of approx 8 million Census blocks with Census FIPS for each block ID - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = blockpoints, 
            name = "blockpoints", type = "arrow", 
            title = "blockpoints data for EJAM", 
            description = "data.table of approx 8 million Census blocks with blockid, lat, lon - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
board %>% 
  pins::pin_write(x = quaddata, 
            name = "quaddata", type = "arrow", 
            title = "quaddata data for EJAM", 
            description = "data.table of approx 8 million Census blocks with BLOCK_X, BLOCK_Z, BLOCK_Y, blockid, used to create index of all US block point locations - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  ) 
board %>% 
  pins::pin_write(x = blockwts, 
            name = "blockwts", type = "arrow", 
            title = "blockwts data from EJScreen for EJAM", 
            description = "data.table of approx 8 million Census blocks with blockid, bgid, blockwt, block_radius_miles - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = Sys.Date(), ejscreen_version = "2.2")
  )
################### # 


############################################################### # 

## Confirm it worked ####
board %>% pins::pin_browse("Mark/bgej")
board %>% pins::pin_browse("Mark/blockwts") 
board %>% pins::pin_browse("Mark/frs")


# THEN HAD TO MANUALLY SET ACCESS TO "EVERYONE" FOR EACH OF THE PINS !


############################################################### # 
# ~ ####

# TO READ IT LATER ####

# Note user needs own Posit Connect API key if not accessible by all on network, but should be accessible by all on network as configured as of 12/2023 

### via URL ####
# if logged in and have api key etc already, 
# this does work to download it: 
# https://rstudio-connect.dmap-stage.aws.epa.gov/content/343456d8-d580-47e1-87f2-1ec95ad7f792/_rev1116/bgej.arrow


### via R code  ####
#
# library(pins)
# board <- pins::board_connect(server = "rstudio-connect.dmap-stage.aws.epa.gov")
### board <- pins::board_connect(server = server = Sys.getenv("CONNECT_SERVER")) # ??? 
# bgej <- pins::pin_read(board, "Mark/bgej") 
# bgej[bgej$ST == "DE", ]  ### IT IS A TIBBLE NOT DT, NOT DF
#
# # or generally: 
#### dataload_from_pins()   
# # which mostly does something similar to this:
# b_vars <- c('blockwts', 'quaddata', 'blockpoints', 'blockid2fips', 'bgid2fips', 'bgej')
# frs_vars <- c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
# for (varname in c(frs_vars, b_vars)) {
#   assign(varname, value = pins::pin_read(board, paste0("Mark/", varname)))
# }

