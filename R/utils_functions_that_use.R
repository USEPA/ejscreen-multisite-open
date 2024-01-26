#' utility for developing package - searches for text in each function exported by pkg (or each .R source file in pkg/R)
#' @details Searches the body and parameter defaults of exported functions.  
#' @param text something like "EJAM::" or "stop\\(" or "library\\(" or "***"
#' @param pkg name of package or path to source package root folder - this 
#'   
#'   checks only the exported functions of an installed package, 
#'   if pkg = some installed package as character string like "EJAM"
#'   
#'   checks each .R source FILE NOT each actual function, 
#'   if pkg = root folder of source package with subfolder called R with .R source files
#'   
#' @param ignore_comments logical, 
#'   ignore_comments is ignored and treated as if it were TRUE when pkg = some installed package
#'   
#'   ignore_comments is used only if pkg = a folder that contains .R files
#'   
#'    Note it will fail to ignore comments in .R files that are at the end of the line of actual code like  print(1) # that prints 1
#'
#' @return vector of names of functions or paths to .R files
#' @export
#'
functions_that_use <- function(text = "stop\\(", pkg = "EJAM", ignore_comments = TRUE) {
  
  
  if (grepl("\\(", text) & !grepl("\\\\\\(", text)) {warning('to look for uses of stop(), for example, use two slashes before the open parens, etc. as when using grepl()')}
  
  stops <- NULL
  if (inherits(try(find.package(pkg), silent = TRUE), "try-error")) {
    # not an installed pkg. 
    if (dir.exists(file.path(pkg, "R"))) {
      # it should be a folder that is root of source package with subfolder called R with .R files so search in those
      
      for (this in list.files(file.path(pkg, 'R'), pattern = '.R', full.names = TRUE)) {
        text_lines_of_function_body <- readLines(this)
        # each row is an element of the vector here
        if (ignore_comments) {
          dropcommentedlines <- function(mytext) {gsub("^[ ]*#.*$", "", mytext)} # presumes each line is an element of mytext vector
          text_lines_of_function_body <- dropcommentedlines(text_lines_of_function_body)
        }
        text_of_function_body <- paste0(text_lines_of_function_body, collapse = '\n')
        if (grepl(text, text_of_function_body)) {
          stops <- c(stops, this)}
      }
      
    } else {
      if (shiny::isRunning()) {
        warning('pkg must be the name of an installed package or a path to root of source package with R subfolder that has .R files')
        return(NULL)
      } else {
      stop('pkg must be the name of an installed package or a path to root of source package with R subfolder that has .R files')
      }
    }
  } else {
    # it is an installed package
    if (ignore_comments == FALSE) {warning('always ignores commented lines when checking exported functions of an installed package')}
  for (this in getNamespaceExports(pkg)) {
    
    text_lines_of_function_body <- as.character(functionBody(get(this)))
    # or is that the same as just  as.character(body(this))  ??
    # each row is an element of the vector now
    
    # also check the function parameter default values
    text_lines_of_function_body <- c(text_lines_of_function_body, paste0(formals(this), collapse = " "))
    
    if (ignore_comments) {
      dropcommentedlines <- function(mytext) {gsub("^[ ]*#.*$", "", mytext)} # presumes each line is an element of mytext vector
      # however that will fail to ignore comments that are at the end of the line of actual code like  print(1) # that prints 1
      text_lines_of_function_body <- dropcommentedlines(text_lines_of_function_body)
    }
    text_of_function_body <- paste0(text_lines_of_function_body, collapse = '\n')
    if (grepl(text, text_of_function_body)) {
      stops <- c(stops, this)}
  }
  }
  return(sort(stops))
  }
