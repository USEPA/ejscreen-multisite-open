#' dataload_from_pins - download / load datasets from pin board
#' @details 
#'   This does work:
#'   
#'   board <- pins::board_connect(auth = "rsconnect") 
#'   
#'   assuming that credentials are set up for the user doing this.
#'   
#'   This does work:
#'   
#'   board <- pins::board_connect(auth = 'manual', server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY")) 
#'   
#'   if   Sys.setenv(CONNECT_SERVER = "https://rstudio-connect.dmap-stage.aws.epa.gov")
#'   
#'   and if  CONNECT_API_KEY  was set to the API key created already.
#'   
#'   
#' @param varnames character vector of names of R objects to get from board
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
#'
#' @return a vector of names of objects downloaded if justchecking = FALSE, which excludes those 
#'   already in environment so not re-downloaded and excludes those not found in pin board. 
#'   If justchecking = TRUE, returns vector of names of ALL objects found in pin board,
#'   regardless of whether they are already in the environment, and 
#'   regardless of whether they were specified among varnames. 
#' @export
#'
dataload_from_pins <- function(varnames = c('blockwts', 'quaddata', 'blockpoints', 'blockid2fips', 'bgid2fips', 'bgej'), 
                               boardfolder = "Mark", 
                               auth = "auto",
                               server = "https://rstudio-connect.dmap-stage.aws.epa.gov",
                               # server = "rstudio-connect.dmap-stage.aws.epa.gov", 
                               envir = globalenv(), 
                               folder_local_source = './data/',
                               justchecking = FALSE) {
  
  if (auth == "rsconnect") {
    board <- tryCatch(
      pins::board_connect(auth = "rsconnect"), # ignore server default here. use server and key already configured for rsconnect.
      error = function(e){e})
    if(inherits(board, 'error')){
      dataload_from_local(varnames, folder_local_source = folder_local_source, ext='arrow')
    }
    # server <- gsub("https://", "", server)
  } else {
    board <- tryCatch(
      pins::board_connect(server = server, auth = auth),
      error = function(e){e})
    if(inherits(board, 'error')){
      dataload_from_local(varnames, folder_local_source = folder_local_source, ext='arrow')
    }
  }
  
  if(!inherits(board, 'error')){
    varnames_gotten <- NULL
    if (justchecking) {
      message("Ignoring varnames, since justchecking = TRUE")
      cat("\nAvailable pins found at ", server,":\n\n")
      varnames_info <- pins::pin_search(board, boardfolder)
      print(varnames_info) # view a table of info about the pins
      cat("\n")
      varnames_gotten <- gsub(paste0(boardfolder, "/"), "",  varnames_info$name)
      return( varnames_gotten) # get a vector of just the names of the objects 
    } else {
      for (varname_n in varnames) {
        if (exists(varname_n, envir = envir)) {
          cat(varname_n, " - an object with this name is already in specified environment, so not downloaded again.\n")
        } else {
          cat(varname_n, " - not in local folder... ")
          pathpin <- paste0(boardfolder, "/", varname_n)
          if (pins::pin_exists(board, pathpin)) {
            assign(varname_n, pins::pin_read(board, pathpin), envir = envir)
            cat(varname_n, " - has been read from pin into specified environment.\n")
            varnames_gotten <- c(varnames_gotten, varname_n)
          } else {
            cat(varname_n, " - was not found at ", server, "/", pathpin, "\n")
            warning(pathpin, " not found at ", server)
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
  
}
