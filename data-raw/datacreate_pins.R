#################################################################### #
# create pins board and save large data files there in .arrow format
#################################################################### #

# See documentation on pins ####
# as a way to store data 
#
# https://docs.posit.co/connect/user/content-settings/
# https://pins.rstudio.com/reference/board_connect.html
# https://pins.rstudio.com/articles/posit-connect.html
# https://docs.posit.co/connect/how-to/pins/
# ~ ####
#################################################################### #
# GET DATASETS ####
# 
# Get them manually from where created. 

##################################### # 
#  The block datasets were created via  EJAM/data-raw/datacreate_blockwts.R  and other code.
# and originally were in package called EJAMblockdata 
# and bgej happens to be here:
# https://github.com/USEPA/EJAM/blob/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# https://github.com/USEPA/EJAM/raw/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# load("~/../Downloads/bgej.rda")
# > object.size(bgej)
# 123466744 bytes

##################################### # 
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
# CREATE/SPECIFY BOARD ####

# instead of storing in aws dmap data commons
library(pins)
board <- pins::board_connect(auth = "auto")   # uses  "rsconnect"
# confirm you can see it:
board %>% pins::pin_browse("Mark")

#################################################################### #
# WRITE DATA TO BOARD ####

## frs and frs_by_programid ####
#   create frs-related ones here too:

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

#                               note   bgej  was a tibble but replaced 12/5/23 with as.data.frame(bgej) version
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
# if Mark is logged in and has api key etc already, 
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

