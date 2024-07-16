

#' utility - write data objects to pins board in .arrow format
#' 
#' @param varnames vector of quoted names of datasets
#' @param boardfolder if needed to specify a different folder than default
#' @param auth See help documentation for [pins::board_connect()]
#' @param server if needed to specify a server other than default (which might be
#'   stored in envt variable CONNECT_SERVER or be registered via the rsconnect package).
#'   Note if auth = "envvar" then it looks for CONNECT_SERVER to get name of server which
#'   needs to be the full url starting with https:// - see help for board_connect
#' @param justchecking can set to TRUE to just see a list of what pins are stored in that board
#' @param silent set to TRUE to suppress cat() msgs to console
#' @param ignorelocal set it to TRUE to avoid 1st checking local disk for copies of files
#' @import pins
#' @return   varnames
#' 
datawrite_to_pins = function(
    
  varnames = c(
    'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
    'bgej', 'bgid2fips',
    'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"),
  
  # ext = c(".arrow", ".rda")[1],
  # fun = "arrow::write_ipc_file", 
  
  boardfolder = "Mark",
  auth = "auto",
  server = "https://rstudio-connect.dmap-stage.aws.epa.gov",
  
  justchecking = FALSE,
  ignorelocal = FALSE,
  silent = FALSE
) {
  
  # library(arrow)
  # library(data.table)
  # library(magrittr)
  
  #  WHAT METADATA TO USE FOR pins board info
  
  ### this is the metadata already inside each object (each dataset)
  meta = 0
  meta <- metadata_add(meta)
  
  ### this is the metadata to write to the pins board:
  ###  A list containing additional metadata to store with the pin. 
  ###  When retrieving the pin, this will be stored in the user key, to avoid potential clashes with the metadata that pins itself uses.
  
  meta <- list(
    date_pins_updated = c(pinsUploadDate = as.character(Sys.Date())), 
    ejscreen_version = attr(meta, "ejscreen_version")
  )
  ####################################################### #
  
  # Create or connect to existing pins board ####
  
  board <- board_connect(versioned = TRUE)
  
  # board <- pins::board_connect(auth = "auto")   # uses  "rsconnect"
  # confirm you can see it:
  # board %>% pins::pin_browse("Mark/frs") ## launches browser
  ## other options for types of boards: 
  ## board_local() - link to local file folder
  ## board_folder() - link to dropbox or network drive
  ## board_s3() - link to S3 bucket, such as EPA Data commons
  ## board_url() - build board from data URLs, allow read-only access to datasets
  ############################################################### #
  
  if (justchecking) {
    ## list all datasets in a board
print(
   pin_list(board = board)
)  
  
  ## see metadata about board or dataset
  # board
    for (i in 1:length(varnames)) {
    print(
     pin_meta(board = board, name = paste0(boardfolder, '/', varnames[i]))$user
    )
      }
  ## show versions of a dataset
  # pin_versions(board = board, name = 'blockgroupstats_rds')
  
    return()
  } 
 
  ## read datasets back in from the board
  # system.time({bgstats <- board %>% pin_read('blockgroupstats_rds')})
  # system.time({bgstats <- board %>% pin_read('blockgroupstats_arrow')})
  
  #################################################################### #
  # WRITE DATA TO BOARD ####
  #################################################################### #
  
  # Ask to confirm each of the defaults 

  if (interactive() && missing(varnames)) {
    confirmed <- rep(TRUE, length(varnames))
    for (i in seq_along(varnames)) {
      confirmed[i] <- askYesNo(paste0("Save ", varnames[i], "?"))
    }
    varnames <- varnames[!is.na(confirmed) & confirmed]
  }
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

## example of reading data from a pins board into a shiny app
## see more at https://pins.rstudio.com/articles/posit-connect.html
# if (FALSE) {
#   library(shiny)
#   ui <- fluidPage(
#     tableOutput("table")
#   )
#   
#   server <- function(input, output, session) {
#     #board <- board_local()
#     data <- pin_reactive_read(board, "blockgroupstats_arrow", interval = 1000)
#     output$table <- renderTable(data()[1:100,1:10])
#   }
#   shinyApp(ui, server)
# }
############################################################### #

  ## FRS DATA   ####
  
  if (exists("frs") & ("frs" %in% varnames)) {  
    # https://rstudio-connect.dmap-stage.aws.epa.gov/content/2c1f4770-adf1-4e4b-9e9b-f22ab597c858/frs.arrow
    board %>% 
      pins::pin_write(x = frs, 
                      name = "frs", type = "arrow", 
                      title = "frs data from EJScreen for EJAM", 
                      description = "data.table -- See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )}
  if (exists("frs_by_programid") & ("frs_by_programid" %in% varnames)) {
    board %>% 
      pins::pin_write(x = frs_by_programid, 
                      name = "frs_by_programid", type = "arrow", 
                      title = "frs_by_programid data from EJScreen for EJAM", 
                      description = "data.table -- See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )}
  if (exists("frs_by_naics") & ("frs_by_naics" %in% varnames)) {
    board %>% 
      pins::pin_write(x = frs_by_naics, 
                      name = "frs_by_naics", type = "arrow", 
                      title = "frs_by_naics data from EJScreen for EJAM", 
                      description = "data.table -- See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )}
  if (exists("frs_by_sic") & ("frs_by_sic" %in% varnames)) {
    board %>% 
      pins::pin_write(x = frs_by_sic, 
                      name = "frs_by_sic", type = "arrow", 
                      title = "frs_by_sic data from EJScreen for EJAM", 
                      description = "data.table -- See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )}
  if (exists("frs_by_mact") & ("frs_by_mact" %in% varnames)) {
    board %>% 
      pins::pin_write(x = frs_by_mact, 
                      name = "frs_by_mact", type = "arrow", 
                      title = "frs_by_mact data from EJScreen for EJAM", 
                      description = "data.table -- See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )}
  ################### # 
  
  # BLOCKGROUP DATA      
  
  if (exists("bgej") & ("bgej" %in% varnames)) {
    board %>% 
      pins::pin_write(x = bgej, 
                      name = "bgej", type = "arrow", 
                      title = "bgej data from EJScreen for EJAM", 
                      description = "data.frame -- approx 243k blockgroups, like blockgroupstats but for EJ Index raw scores, with bgfips, bgid, etc. - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )
  }
  if (exists("bgid2fips") & ("bgid2fips" %in% varnames)) {
    board %>% 
      pins::pin_write(x = bgid2fips, 
                      name = "bgid2fips", type = "arrow", 
                      title = "bgid2fips data for EJAM", 
                      description = "data.table of approx 242k blockgroups with Census FIPS for each blockgroup ID - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )
  }
  ################### # 
  
  # BLOCKS DATA
  
  if (exists("blockid2fips") & ("blockid2fips" %in% varnames)) {
    board %>% 
      pins::pin_write(x = blockid2fips, 
                      name = "blockid2fips", type = "arrow", 
                      title = "blockid2fips data for EJAM", 
                      description = "data.table of approx 8 million Census blocks with Census FIPS for each block ID - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )
  }
  if (exists("blockpoints") & ("blockpoints" %in% varnames)) {
    board %>% 
      pins::pin_write(x = blockpoints, 
                      name = "blockpoints", type = "arrow", 
                      title = "blockpoints data for EJAM", 
                      description = "data.table of approx 8 million Census blocks with blockid, lat, lon - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )
  }
  if (exists("quaddata") & ("quaddata" %in% varnames)) {
    board %>% 
      pins::pin_write(x = quaddata, 
                      name = "quaddata", type = "arrow", 
                      title = "quaddata data for EJAM", 
                      description = "data.table of approx 8 million Census blocks with BLOCK_X, BLOCK_Z, BLOCK_Y, blockid, used to create index of all US block point locations - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      ) 
  }
  if (exists("blockwts") & ("blockwts" %in% varnames)) {
    board %>% 
      pins::pin_write(x = blockwts, 
                      name = "blockwts", type = "arrow", 
                      title = "blockwts data from EJScreen for EJAM", 
                      description = "data.table of approx 8 million Census blocks with blockid, bgid, blockwt, block_radius_miles - See documentation in EJAM package", 
                      versioned = TRUE, metadata = attributes(meta)
      )
  }
  ################### # 
  
  
  ############################################################### # 
  
  ## Confirm it worked ####
  if (interactive()) {
    for (vn in varnames) {
      board %>% pins::pin_browse(paste0("Mark/", vn))
    }}
  
 cat( 'MIGHT HAVE TO MANUALLY SET ACCESS TO "EVERYONE" FOR EACH OF THE PINS !\n ')
 
 return(varnames)
 
}  
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
  
 ############################################################### # 
