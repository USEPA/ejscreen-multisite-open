#' ejam2excel
#' Format the results of ejamit() for excel and optionally save .xlsx file
#' @param ejamitout output of [ejamit()] 
#' @param fname optional name or full path and name of file to save locally, like "out.xlsx" 
#' @param save_now optional logical, whether to save as a .xlsx file locally or just return workbook object
#'   that can later be written to .xlsx file using [openxlsx::saveWorkbook()]
#' @param overwrite optional logical, passed to [openxlsx::saveWorkbook()]
#' @param launchexcel optional logical, passed to [xls_formatting2()], whether to launch browser to see spreadsheet immediately
#' @param interactive_console optional - should set to FALSE when used in code or server. If TRUE,
#'   prompts RStudio user interactively asking where to save the downloaded file
#' @param in.testing optional logical
#' @param radius_or_buffer_in_miles optional radius in miles
#' @param in.analysis_title optional title as character string
#' @param react.v1_summary_plot optional - a plot object
#' @param radius_or_buffer_description optional text phrase describing places analyzed
#' @param hyperlink_colnames optional names of columns with URLs
#' @param ... optional additional parameters passed to [xls_formatting2()], such as 
#'   heatmap_colnames, heatmap_cuts, heatmap_colors, etc.
#' @examples \dontrun{
#'   ejam2excel(testoutput_ejamit_10pts_1miles)
#'   }
#' @return returns a workbook object for use by openxlsx::saveWorkbook(wb_out, pathname)
#'   or returns just the full path/file name of where it was saved if save_now = TRUE
#' @export
#'
ejam2excel <- function(ejamitout, 
                       fname = NULL, # full path and name, or just name of .xlsx file 
                       save_now = TRUE, overwrite = TRUE, launchexcel = FALSE,
                       interactive_console = TRUE, 
                       
                       in.testing = FALSE,
                       in.analysis_title =  "EJAM analysis",
                       react.v1_summary_plot = NULL,
                       radius_or_buffer_in_miles = NULL,  #  input$bt_rad_buff
                       buffer_desc = "Selected Locations", 
                       radius_or_buffer_description = 'Miles radius of circular buffer (or distance used if buffering around polygons)', 
                       # radius_or_buffer_description =   "Distance from each site (radius of each circular buffer around a point)",
                       hyperlink_colnames = c("EJScreen Report", "EJScreen Map", "ECHO report"),
                       ...
) {
  
  npts <- NROW(ejamitout$results_bysite)
  if (missing(radius_or_buffer_in_miles)) {
    radius_or_buffer_in_miles  <- ejamitout$results_overall$radius.miles
  } 
  
  if (is.null(fname)) {
    fname_was_provided <- FALSE
    default_pathname <- paste0(
      gsub(" ", ".", gsub(":", "", Sys.time())),
      " ", "EJAM output ", 
      npts, "pts ", radius_or_buffer_in_miles, "miles", 
      ".xlsx") 
    # e.g.,   "2023-10-10.220725 EJAM output 10pts 3miles.xlsx"
    pathname <- default_pathname
  } else {
    fname_was_provided <- TRUE
    pathname <- fname
  } 
  
  # server does something like this:
  ## note analysis type or overview to 'notes' tab
  # if (submitted_upload_method() == "SHP") {
  #   radius_or_buffer_description <- 'Distance from each shape (buffering around each polygon)'
  # } else {
  # radius_or_buffer_description <- 'Distance from each site (radius of each circular buffer around a point)'
  # }
  
  # keepcols <- rep(TRUE, NCOL(ejamitout$results_overall))
  
  # defaults in xls_formatting2 : 
  #
  # overall, eachsite, longnames=NULL, bybg=NULL, formatted=NULL,
  # summary_plot = NULL, 
  # plotlatest = TRUE, 
  # plotfilename = NULL, 
  # 
  # analysis_title = "EJAM analysis",
  # buffer_desc = "Selected Locations", 
  # radius_or_buffer_in_miles = NULL,
  # radius_or_buffer_description='Miles radius of circular buffer (or distance used if buffering around polygons)',
  # notes=NULL,
  # 
  # heatmap_colnames=NULL, heatmap_cuts = c(80, 90, 95), heatmap_colors = c("yellow", "orange", "red"),
  # hyperlink_colnames = c("EJScreen Report","EJScreen Map","ECHO report"), 
  # graycolnames=NULL, narrowcolnames=NULL, graycolor='gray', narrow6=6,
  # 
  # testing=FALSE, launchexcel = FALSE, saveas = NULL,
  
  # in server code:
  #hyperlink_colnames = c("EJScreen Report", "EJScreen Map" ),
  # summary_plot   = v1_summary_plot(),
  # analysis_title = input$analysis_title,
  # buffer_desc    = "Selected Locations", 
  # radius_or_buffer_in_miles = input$bt_rad_buff,
  # radius_or_buffer_description = radius_or_buffer_description,
  # # saveas = fname,
  # testing = input$testing
  
  # these should be data.tables or at least they used to be when coming from ejamit() but not within server code...
  # so does that cause a problem for xls_formatting2() if they are data.table format???
  
  wb_out <- xls_formatting2(
    
    # ### if we must provide data.frame only, not data.table, here, then we may need to convert them:
    # overall   = as.data.frame(ejamitout$results_overall), # 1 row with overall results aggregated across sites
    # eachsite  = as.data.frame(ejamitout$results_bysite), # 1 row per site
    # longnames = as.data.frame(ejamitout$longnames), # 1 row, but full plain English column names
    
    overall   = ejamitout$results_overall, #  1 row with overall results aggregated across sites
    eachsite  = ejamitout$results_bysite,  #  1 row per site
    longnames = ejamitout$longnames,       #  1 row, but full plain English column names
    bybg      = ejamitout$results_bybg_people, # not entirely sure should provide bybg tab? it is huge and only for expert users but enables a plot
    formatted = ejamitout$formatted,  
    hyperlink_colnames = hyperlink_colnames,  # need to ensure these get formatted right to work as links in Excel
    # heatmap_colnames=names(table_as_displayed)[pctile_colnums], # can use defaults
    # heatmap_cuts=c(80, 90, 95), # can use defaults
    # heatmap_colors=c('yellow', 'orange', 'red') # can use defaults
    ## optional, shiny-specific arguments to go in 'Plot' and 'Notes' sheets
    summary_plot   = react.v1_summary_plot, # NULL is fine
    analysis_title = in.analysis_title,
    
    buffer_desc = buffer_desc,
    radius_or_buffer_in_miles    = radius_or_buffer_in_miles,
    radius_or_buffer_description = radius_or_buffer_description,
    # saveas = pathname, # could do it this way but then need to condition it on save_now and cannot offer interactive picking of pathname in RStudio
    testing = in.testing,
    launchexcel = launchexcel,
    ...
  )
  
  if (save_now) {
    if (interactive_console & interactive()) {
      # *** replace this awkward interface- should allow normal save where you can alter or confirm both the filename and the folder
      # and  should we parse what they provided and if only filename given, prompt them for folder?
      if (fname_was_provided) {
        # ok, pathname exists already. 
      } else {
        pathname <- rstudioapi::showPrompt(
          "Save spreadsheet file", 
          "Confirm folder and name of file to save",
          default = pathname
        )
        
      }
      
      # do error checking of pathname here and if invalid, should let you try again
      # ***
      
      
      
    }
    if (is.null(pathname)) {
      cat('Invalid path/file, so using default: ', default_pathname, '\n')
      pathname <- default_pathname
    }
    # do error checking of pathname here - in case not interactively set and want to warn/ exit more gracefully
    
    cat("Saving as ", pathname, "\n")
    ## save file and return for downloading - or do this within xls_formatting2( , saveas=fname) ?
    openxlsx::saveWorkbook(wb_out, pathname, overwrite = overwrite)
    return(pathname)
  } else {
    return(wb_out)
  }
}








