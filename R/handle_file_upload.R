
#' Used in app_server.R to read in files into a data.table
#' 
#' 
#' @description This function replaces some of the repetitive code in app_server for reading in input data files. It checks what type of file it is then properly reads it into a data.table
#' 
#' @param file_input actual input file that's being inputed through the app
#' 
#' @param file_type the extension derived from the file path
#' 
#' @return data.table derived from the input



handle_file_upload <- function(file_input, file_type) {
  result <- switch(file_type,
                   csv = {
                     tryCatch({
                       data.table::fread(file_input$datapath)
                     }, error = function(e) {
                       shiny::validate(paste("This csv file caused an error:", e$message))
                       NULL
                     })
                   },
                   
                   xls = {
                     tryCatch({
                       sheets <- readxl::excel_sheets(file_input$datapath)
                       if (length(sheets) > 1) {
                         shiny::validate('This excel file contains multiple sheets. EJAM only looks at the first sheet which may result in an error.')
                       }
                       readxl::read_excel(file_input$datapath) %>% data.table::as.data.table()
                     }, error = function(e) {
                       shiny::validate(paste("This excel file caused an error:", e$message))
                       NULL
                     })
                   },
                   
                   xlsx = {
                     tryCatch({
                       sheets <- readxl::excel_sheets(file_input$datapath)
                       if (length(sheets) > 1) {
                         shiny::validate('This excel file contains multiple sheets. EJAM only looks at the first sheet which may result in an error.')
                       }
                       readxl::read_excel(file_input$datapath) %>% data.table::as.data.table()
                     }, error = function(e) {
                       shiny::validate(paste("This excel file caused an error:", e$message))
                       NULL
                     })
                   },
                   
                   shiny::validate('Invalid file; Please upload a .csv, .xls, or .xlsx file')
  )
  
  return(result)
}
