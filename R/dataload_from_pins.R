#' dataload_from_pins - download / load datasets from pin board
#' @details 
#'   This does work if on VPN and if credentials already set up for the user doing this:
#'   
#'   board <- pins::board_connect(auth = "rsconnect") 
#'   
#'   This does work if that is true plus the two environment variables were created:
#'   
#'   board <- pins::board_connect(auth = 'manual', 
#'   
#'     server = Sys.getenv("CONNECT_SERVER"), 
#'   
#'     key = Sys.getenv("CONNECT_API_KEY")
#'     
#'   ) 
#'   
#'     after Sys.setenv(CONNECT_SERVER = "https://rstudio-connect.dmap-stage.aws.epa.gov")
#'   
#'     and   Sys.setenv(CONNECT_API_KEY =  correct-API-key-goes-here  )
#'   
#'   
#' @param varnames character vector of names of R objects to get from board, 
#'   or set this to "all" to load all of them
#' @param boardfolder if needed to specify a different folder than default
#' @param auth See help documentation for [pins::board_connect()]
#' @param server if needed to specify a server other than default (which might be 
#'   stored in envt variable CONNECT_SERVER or be registered via the rsconnect package).
#'   Note if auth = "envvar" then it looks for CONNECT_SERVER to get name of server which
#'   needs to be the full url starting with https:// - see help for board_connect 
#' @param envir if needed to specify environment other than default
#' @param folder_local_source path of local folder to 
#'   look in for locally saved copies in case pins board is not reachable by user. 
#' @param justchecking can set to TRUE to just see a list of what pins are stored in that board
#' @param getall set to TRUE to get all 
#' @import pins
#' 
#' @return a vector of names of objects downloaded if justchecking = FALSE, which excludes those 
#'   already in environment so not re-downloaded and excludes those not found in pin board. 
#'   If justchecking = TRUE, returns vector of names of ALL objects found in specified pin board,
#'   regardless of whether they are already in the environment, and 
#'   regardless of whether they were specified among varnames, or are related to EJAM at all. 
#' @export
#'
dataload_from_pins <- function(varnames = c(
  c('blockwts', 'blockpoints', 'blockid2fips', "quaddata"), 
  'bgej',      # load only if /when needed
  'bgid2fips', # load only if /when needed
  c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")  # load only if /when needed
)[1:4], 
boardfolder = "Mark", 
auth = "auto",
server = "https://rstudio-connect.dmap-stage.aws.epa.gov",
# server = "rstudio-connect.dmap-stage.aws.epa.gov", 
folder_local_source = NULL, # './data/', # or "~/../Downloads"
envir = globalenv(), 
justchecking = FALSE) {
  
  if ('all' %in% tolower(varnames)) {
    varnames <- c(
      c('blockwts', 'blockpoints', 'blockid2fips', "quaddata"), 
      'bgej',      
      'bgid2fips', 
      c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
    )
  }
  
  if (justchecking) {
    dataload_from_local(varnames = varnames, envir = envir, justchecking = TRUE, folder_local_source = folder_local_source) # this will display in console some info on where vars exist 
  }
  
  if (auth == "rsconnect") {
    board <- tryCatch(
      pins::board_connect(auth = "rsconnect") # ignore server default here. use server and key already configured for rsconnect.
      , error = function(e) e)
    
    # server <- gsub("https://", "", server)
  } else {
    board <- tryCatch(pins::board_connect(server = server, auth = auth),
                      error = function(e) e)
  }
  if (inherits(board, "error")) {
    board_available <- FALSE
    dataload_from_local(varnames, folder_local_source = folder_local_source, ext = 'arrow')
    
  } else {
    board_available <- TRUE
  }
  
  varnames_gotten <- NULL
  if (justchecking) {
    if (board_available) {
      message("Ignoring varnames, since justchecking = TRUE")
      cat("\nAvailable pins found at", server,":\n\n")
      varnames_info <- pins::pin_search(board, boardfolder)
      print(varnames_info) # view a table of info about the pins
      cat("\n")
      varnames_gotten <- gsub(paste0(boardfolder, "/"), "",  varnames_info$name)
    } else {   
      return( varnames_gotten) # get a vector of just the names of the objects
    }
    } else {
      for (varname_n in varnames) {
        if (exists(varname_n, envir = envir)) {
          cat(varname_n, " - an object with this name is already in specified environment, so not downloaded again.\n")
        } else {
          cat(varname_n, "- was loaded from local folder.\n")
          # cat(varname_n, "- not in local folder... ") # redundant with dataload_from_local() ?
          if (board_available) {
            pathpin <- paste0(boardfolder, "/", varname_n)
            if (pins::pin_exists(board, pathpin)) {
              assign(varname_n, pins::pin_read(board, pathpin), envir = envir)
              cat(varname_n, "- has been read from pin into specified environment.\n")
              varnames_gotten <- c(varnames_gotten, varname_n)
            } else {
              cat(varname_n, " - was not found at ", server, "/", pathpin, "\n", sep = "")
              warning(pathpin, "not found at ", server)
            }
          } else {
            cat(" and could not download ", varname_n, " - cannot connect to ", server, "/", "\n", sep = "")
          }
        }
      }
    }
    return(varnames_gotten)
  }
  # @param envir e.g., globalenv() or parent.frame()
  
  #  board <- pins::board_connect(server = "rstudio-connect.dmap-stage.aws.epa.gov")
  ## board <- pins::board_connect(server = server = Sys.getenv("CONNECT_SERVER"))  
  #  bgej  <- pins::pin_read(board, "Mark/bgej") ### IT IS A TIBBLE NOT DT, NOT DF
  
