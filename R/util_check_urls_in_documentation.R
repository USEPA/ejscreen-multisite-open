util_check_urls_in_documentation <- function() {
  cat(' search in each .R file for this: \n')
  cat(paste0( '"' ," grep(", "^#'", '.*[^<]http", x) ',  "\n "))
   
}


  
