#' datawrite_to_local -  NOT YET WORKING for .arrow
#' Write large object(s) like EJAM datasets to local disk for convenience during app/pkg development, formatted as .arrow or .rda 
#' @param varnames vector of object names
#' @param ext file .extension appropriate to the format, ".rda" or ".arrow"
#' @param fun function to use, but as a character string, like "arrow::write_ipc_file"
#'   but fun is ignored if ext=".rda" since it then just uses save()
#' @param justchecking set this to FALSE to actually save instead of 
#'   just seeing in console info or the commands to be used, to test/check this
#' @param folder_local_source path to local folder without slash at end
#' @return the paths of the objects as requested to be saved whether or not actually done
#' @seealso [datawrite_to_aws()]  [datawrite_to_local()] [dataload_from_local()] [dataload_from_aws()] 
#' @export
#'
datawrite_to_local <- function(varnames= c('bgid2fips',   'blockid2fips', 'blockpoints', 'blockwts' , 'quaddata' ), 
                               ext=c(".arrow", ".rda")[2],
                               folder_local_source = "~/../Downloads", 
                               fun=c("arrow::write_ipc_file", "save")[2], # not sure save would work here. 
                               justchecking = TRUE) {
  if (!is.character(fun)) {stop('must specify function in fun parameter as a quoted character string')}
  if (length(ext) > 1) {stop('must specify only one file extension for all the files')}
  if (ext=='.arrow' & missing(fun)) {fun <- "arrow::write_ipc_file"} 
  
  fnames <- paste0(varnames, ext)
  localpaths  <- paste0(folder_local_source, '/', fnames)
  
  for (i in 1:length(varnames)) {
    
    this <- varnames[i] 
    
    if (justchecking) {
      cat(this, "is in memory already? ", exists(x = this ), '\n') # looks in default environment!!
      cat(this, 'is at', localpaths[i], '? ', file.exists(localpaths[i]), '\n')
      cat("The folder" , folder_local_source, "exists? ", file.exists(folder_local_source), '\n')
    } else {
      
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
        cat(text_to_do, '\n')
        x <- eval(parse(text = text_to_do)) # executes the command
        
        # arrow::write_ipc_file( UNQUOTED OBJECT NAME!***, sink = localpaths[i])
        
      }
    }
  }   # end loop
  
  return(localpaths)
}
##############################################################
