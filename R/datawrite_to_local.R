#' datawrite_to_local
#' Write large object(s) like EJAM datasets to local disk for convenience during app/pkg development, formatted as .arrow or .rda 
#' @param varnames vector of object names
#' @param ext file .extension appropriate to the format, ".rda" or ".arrow"
#' @param fun function to use, but as a character string, like "arrow::write_ipc_file"
#'   but fun is ignored if ext=".rda" since it then just uses save()
#' @param justchecking set this to FALSE to actually save instead of 
#'   just seeing in console info or the commands to be used, to test/check this
#' @param folder_local_source path to local folder without slash at end
#' @param overwrite Set to TRUE to overwrite file if it exists already, with new copy.
#' @return the paths of the objects as requested to be saved whether or not actually done
#' @seealso [datawrite_to_aws()]  [datawrite_to_local()] [dataload_from_local()] [dataload_from_aws()] 
#' @examples 
#'   # datawrite_to_local(ext = ".arrow", folder_local_source = ".", justchecking = F, overwrite = T) 
#' @export
#'
datawrite_to_local <- function(varnames= c('bgid2fips',   'blockid2fips', 'blockpoints', 'blockwts' , 'quaddata' ), 
                               ext=c(".arrow", ".rda")[1],
                               folder_local_source = "~/../Downloads", 
                               fun=c("arrow::write_ipc_file", "save")[1], # not sure save would work here. 
                               justchecking = F, overwrite = FALSE) {
  if (!is.character(fun)) {warning('must specify function in fun parameter as a quoted character string')
    return(NULL)
  }
  if (length(ext) > 1)    {
    warning('must specify only one file extension for all the files')
    return(NULL)
  }
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::write_ipc_file"} 
  cat("\n\n")
  if (justchecking) {
    cat("Just checking, so nothing is being saved.\n\n")
  }
  if (!dir.exists(folder_local_source)) {
    cat(folder_local_source, ' does not exist.\n\n')
    if (!justchecking) {
      cat("Nothing could be saved.\n\n")
      warning( 'Nothing could be saved.')
      return()
    }
  }
  
  fnames <- paste0(varnames, ext)
  localpaths  <- paste0(folder_local_source, '/', fnames)
  # if (justchecking) {cat("The folder " , folder_local_source, "exists? ", file.exists(folder_local_source), '\n\n')}
  
  for (i in 1:length(varnames)) {
    
    this <- varnames[i] 
    
    if (justchecking) {
      cat(this, "is in memory? ", exists(x = this ), '... ') # looks in default environment!!
      if (dir.exists(folder_local_source)) {
        cat('The file is already saved in folder? ',  file.exists(localpaths[i]))
      }
       cat('\n')
    } else {
      if (file.exists(localpaths[i]) & !overwrite) {cat(localpaths[i], "already exists.\n  Set overwrite=TRUE if you want to replace it.\n")}
      if (!file.exists(localpaths[i]) | overwrite) {
        
        if (ext == ".rda") {
          
          cat("saving", localpaths[i] , "\n")
          save(list = this, file = localpaths[i])
          
        } else {
          
          #     .arrow files   ####
          cat("saving", localpaths[i], "\n");
          
          text_to_do <- paste0("x <- arrow::write_ipc_file(", 
                               varnames[i],", ",  
                               "sink = '", localpaths[i], "')"  
          )
          cat(" ", text_to_do, '\n')
          x <- eval(parse(text = text_to_do)) # executes the command
          
          # arrow::write_ipc_file( UNQUOTED OBJECT NAME!***, sink = localpaths[i])
        }
        if (file.exists(localpaths[i])) {cat("  saved ",  "\n\n")} else {cat("  Failed to save ",   '\n\n')}
      }
     
      
    }
  }   # end loop
  cat('\n\n')
  invisible(localpaths)
}
##############################################################
