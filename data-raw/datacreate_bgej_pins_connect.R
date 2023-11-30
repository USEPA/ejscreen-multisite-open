# See documentation ####
#
# https://docs.posit.co/connect/user/content-settings/
# https://pins.rstudio.com/reference/board_connect.html
# https://pins.rstudio.com/articles/posit-connect.html
# https://docs.posit.co/connect/how-to/pins/
#################################################################### #
# Get the dataset ####
#
# https://github.com/USEPA/EJAM/blob/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# https://github.com/USEPA/EJAM/raw/8b156cc867b8d2f59aa81891113e61af8db2a7bb/data/bgej.rda
# load("~/../Downloads/bgej.rda")
# > object.size(bgej)
# 123466744 bytes


# CREATE BOARD ####
# instead of storing in aws dmap data commons

board <- board_connect(auth = "auto")   # uses  "rsconnect"

# WRITE data objects ####

board %>% 
  pin_write(x = bgej, 
            name = "bgej", type = "arrow", 
            title = "bgej data from EJScreen for EJAM", 
            description = "data.frame -- approx 243k blockgroups, like blockgroupstats but for EJ Index raw scores, with bgfips, bgid, etc. - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  )
board %>% 
  pin_write(x = bgid2fips, 
            name = "bgid2fips", type = "arrow", 
            title = "bgid2fips data for EJAM", 
            description = "data.table of approx 242k blockgroups with Census FIPS for each blockgroup ID - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  )
board %>% 
  pin_write(x = blockid2fips, 
            name = "blockid2fips", type = "arrow", 
            title = "blockid2fips data for EJAM", 
            description = "data.table of approx 8 million Census blocks with Census FIPS for each block ID - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  )
board %>% 
  pin_write(x = blockpoints, 
            name = "blockpoints", type = "arrow", 
            title = "blockpoints data for EJAM", 
            description = "data.table of approx 8 million Census blocks with blockid, lat, lon - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  )
board %>% 
  pin_write(x = quaddata, 
            name = "quaddata", type = "arrow", 
            title = "quaddata data for EJAM", 
            description = "data.table of approx 8 million Census blocks with BLOCK_X, BLOCK_Z, BLOCK_Y, blockid, used to create index of all US block point locations - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  ) 
board %>% 
  pin_write(x = blockwts, 
            name = "blockwts", type = "arrow", 
            title = "blockwts data from EJScreen for EJAM", 
            description = "data.table of approx 8 million Census blocks with blockid, bgid, blockwt, block_radius_miles - See documentation in EJAM package", 
            versioned = TRUE, metadata = list(upload_date = "11/17/2023", ejscreen_version = "2.2")
  )


############################################################### # 

## see that it worked ####
board %>% pin_browse("Mark/bgej")
board %>% pin_browse("Mark/blockwts") 


############################################################### # 


# READ - user needs own Posit Connect API key ####

### via URL ####
# if Mark is logged in and has api key etc already, 
# this does work to download it: 
# https://rstudio-connect.dmap-stage.aws.epa.gov/content/343456d8-d580-47e1-87f2-1ec95ad7f792/_rev1116/bgej.arrow


### via R code  ####
#
# library(pins)
# board <- board_connect(server = "https://rstudio-connect.dmap-stage.aws.epa.gov/connect")
### board <- board_connect(server = server = Sys.getenv("CONNECT_SERVER")) # ??? 
# bgej <- pin_read(board, "Mark/bgej") 
