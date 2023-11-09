#' ejam2excel - alias for table_xls_from_ejam()
#' @inheritParams table_xls_from_ejam
#' @export
ejam2excel <- function(ejamitout, 
                       fname = NULL, # full path and name, or just name of .xlsx file 
                       save_now = TRUE, overwrite = TRUE, launchexcel = FALSE,
                       interactive_console = TRUE, 
                       ok2plot = TRUE,
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
  table_xls_from_ejam(
    ejamitout, 
    fname,
    save_now,
    overwrite,
    launchexcel,
    interactive_console,
    ok2plot,
    in.testing,
    in.analysis_title,
    react.v1_summary_plot,
    radius_or_buffer_in_miles,
    buffer_desc,
    radius_or_buffer_description,
    hyperlink_colnames,
    ...
  ) 
}


