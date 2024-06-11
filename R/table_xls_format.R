#' Format EJAM tabular outputs for saving as Excel spreadsheet
#' 
#' Used by table_xls_from_ejam()
#' 
#' @details  Already took and put here most or all of code from table_xls_format() or table_xls_format_api() 
#' @param overall  table to save in one tab, from ejamit()$overall, EJAM analysis of indicators overall (one row),
#'   but if entire output of ejamit() is passed as if it were overall, function figures out eachsite, etc.
#' @param eachsite table to save in one tab, from ejamit()$overall, EJAM analysis site by site (one row per site)
#' @param longnames vector of indicator names to display in Excel table
#' @param formatted optional table to save in one tab, from ejamit()$overall, EJAM analysis overall in different format
#' @param bybg Optional large table of details of each block group that is only needed to analyze distances by group. 
#' @param plot_distance_by_group logical, whether to try to add a plot of mean distance by group. 
#'   This requires that bybg be provided as a parameter input to this function. 
#' @param summary_plot optional plot object passed from EJAM shiny app to save in 'Plot' sheet of Excel table
#' @param plotlatest optional logical. If TRUE, the most recently displayed plot (prior to this function being called) will be inserted into a tab called plot2
#' @param plotfilename the full path including name of .png file to insert
#' @param mapadd logical optional - try to include a map of the points 
#' @param report_map leaflet map object passed from Shiny app to display in 'Map' sheet
#' @param ok2plot can set to FALSE to prevent plots from being attempted, while debugging
#' @param analysis_title optional title passed from Shiny app to 'Notes' sheet
#' @param buffer_desc optional description of buffer used in analysis, passed to 'Notes' sheet
#' @param radius_miles If provided, miles buffer distance (from polygon or from point if circular buffers)
#' @param radius_or_buffer_description optional text saying if distance is radius or polygon buffer, passed to 'Notes' sheet  
#' @param notes Text of additional notes to put in the notes tab, optional vector of character elements pasted in as one line each.
#' 
#' @param heatmap_colnames optional vector of colnames to apply heatmap colors
#' @param heatmap_cuts vector of values to separate heatmap colors, between 0-100
#' @param heatmap_colors vector of color names for heatmap bins, same length as 
#'   heatmap_cuts, where first color is for those >= 1st cutpoint, but <2d,
#'   second color is for those >=2d cutpoint but <3d, etc.
#' @param hyperlink_colnames names of which to treat as URLs that should be hyperlinks
#' @param graycolnames which columns to deemphasize
#' @param narrowcolnames which column numbers to make narrow
#' @param graycolor color used to deemphasize some columns
#' @param narrow6 how narrow
#' @param launchexcel Set to TRUE to have this function launch Excel immediately, showing the final workbook created here.
#' @param saveas If not NULL, and a valid path with filename.xlsx is provided,
#'    the workbook will be saved locally at that path and name. Warning: it will overwrite an existing file.
#' 
#' @param testing optional for testing only
#' @param updateProgress optional Shiny progress bar to update during formatting
#' @param ... other params passed along to [openxlsx::writeData()]
#' 
#' @import graphics
#' @import openxlsx
#' @import webshot webshot
#' @seealso [table_xls_from_ejam()] 
#' @return a workbook, ready to be saved in spreadsheet format, with tabs like "Overall" and "Each Site"
#' @examples \dontrun{
#'   table_xls_format(
#'     testoutput_ejamit_100pts_1miles$results_overall, 
#'     testoutput_ejamit_100pts_1miles$results_bysite,
#'     saveas =  "out1.xlsx")
#'  # can just pass the whole results of ejamit(), for convenience
#'  wb <- table_xls_format(testoutput_ejamit_100pts_1miles)
#'  openxlsx::saveWorkbook(wb, file = "out2.xlsx")
#' }
#' 
#' @keywords internal
#'
table_xls_format <- function(overall, eachsite, longnames=NULL, formatted=NULL, bybg=NULL, 
                            plot_distance_by_group = FALSE, 
                            summary_plot = NULL, 
                            plotlatest = FALSE, 
                            plotfilename = NULL, 
                            mapadd = FALSE,
                            report_map=NULL,
                            ok2plot = TRUE,
                            analysis_title = "EJAM analysis",
                            buffer_desc = "Selected Locations", 
                            radius_or_buffer_in_miles = NULL,
                            radius_or_buffer_description='Miles radius of circular buffer (or distance used if buffering around polygons)',
                            notes=NULL,
                            
                            heatmap_colnames = NULL,   heatmap_cuts = c(80, 90, 95),  heatmap_colors  = c("yellow", "orange", "red"),
                            heatmap2_colnames = NULL, heatmap2_cuts = c(1.009, 2, 3), heatmap2_colors = c("yellow", "orange", "red"),
                            
                            hyperlink_colnames = c("EJScreen Report", "EJScreen Map", "ECHO report"), 
                            graycolnames=NULL, narrowcolnames=NULL, graycolor='gray', narrow6=6,
                            
                            testing=FALSE, updateProgress = NULL,
                            launchexcel = FALSE, saveas = NULL,
                            
                            ...) {
  
  ###################  #   ###################  #   ###################  #   ###################  # 
  # if user passed the entire output of ejamit() or doaggregate() as the first parameter, 
  # rather than splitting it up and passing overall, eachsite, longnames, formatted, bybg separately,
  # notice that and try to use it and split it up for them - it makes the interface much more convenient for use in console:
  #
  if (class(overall)[1] == "list" && missing(eachsite)) {
    # assume user may have passed entire output of ejamit() or doaggregate() as first parameter, so extract the elements of that list
    if ("longnames"           %in% names(overall) && is.null(longnames)) {longnames <- overall$longnames} # else stays as NULL
    if ("formatted"           %in% names(overall) && is.null(formatted)) {formatted <- overall$formatted} # else stays as NULL
    if ("results_bybg_people" %in% names(overall) && is.null(bybg))      {bybg      <- overall$results_bybg_people} # else stays as NULL

    if (!("results_bysite" %in% names(overall))) {
      eachsite <- NULL # unusual situation we will try to accommodate
    } else {
      eachsite <- overall$results_bysite
    }
    if (!("results_overall" %in% names(overall))) {
      overall <- NULL # unusual situation we will try to accommodate
    }  else {
      overall <- overall$results_overall
    }
    if (is.null(eachsite)[1] && is.null(overall)[1]) {
      # no data available at all
      warning("table_xls_format() requires either overall or eachsite data as from results of ejamit() or doaggregate() for example") 
      return(NULL)
    }
    if (is.null(eachsite)[1] && !is.null(overall)[1]) {
      eachsite <- overall # only 1 of them is available, so use the overall data as the site by site data, enabling the function to still do something useful
    }
    if (!is.null(eachsite)[1] && is.null(overall)[1]) {
      overall <- eachsite[1, ] # only 1 of them is available, so use the eachsite structure but fill in NA values in the data row, hopefully enabling the function to still do something useful 
      overall[ , ] <- NA
    }
  }
  ###################  #   ###################  #   ###################  #   ###################  # 
  if (missing(eachsite)[1] && missing(overall)[1]) {
    # no data available at all
    warning("table_xls_format() requires either overall or eachsite data as from results of ejamit() or doaggregate() for example") 
    return(NULL)
  }
  
  if (is.null(heatmap2_colnames)) {
    heatmap2_colnames <- map_headernames$rname[grepl('ratio', map_headernames$varlist)] # from EJAMejscreenapi ::
  }
  
  if (testing) {
    cat('in table_xls_format, names of each site: \n\n'); print(names(eachsite))
    
  }
  # SHORT REPORT COMBINES THESE reactives:
  #   
  #   summary_title() for top of 1pager (it includes input$analysis_title) 
  # 
  # v1_demog_table() from table_gt_format(df)
  # 
  # v1_envt_table()  same
  # 
  # v1_summary_plot()  from  ggplot() 
  # 
  # report_map()  from leaflet() BUT THAT IS HTML WIDGET NOT A PNG OR GGPLOT OBJECT !
  
  
  # column names check/ cleanup?? #### 
  
  ## if no longnames provided, use existing column names ####
  if (is.null(longnames)) {
    longnames <- fixcolnames( names(overall), 'r', 'long')
  }
  
  longnames_overall <- longnames[!(longnames %in% c("valid","invalid_msg"))]
  if (is.null(formatted)) {
    # formatted <- "" # or could do this here if we assume they omitted it but did not intend to prevent it from appearing on the spreadsheet as a tab:
    formatted <- table_tall_from_overall(results_overall = overall, longnames = longnames_overall)
  }
  
  if (length(heatmap_cuts) != length(heatmap_colors)) {
    warning("heatmap_cuts and heatmap_colors should be same length")
    if (length(heatmap_colors) == 2) {
      heatmap_colors <- colorRampPalette(c(heatmap_colors[1], heatmap_colors[2]) )(length(heatmap_cuts)) } else {
        heatmap_colors <-   heat.colors(length(heatmap_cuts), alpha = 0.5) # problem if length is 0? 1?
        # heatmap_colors <- colorRampPalette(c("white", "red" ))(length(heatmap_cuts))
      }
  }
  if (!is.null(heatmap_colnames))  { 
    if ( setdiff(heatmap_colnames, names(eachsite)) > 0 ) {
      warning(paste0("Some of heatmap_colnames not found among column headers: ", setdiff(heatmap_colnames, names(eachsite)), collapse = ", "), " ")
      heatmap_colnames <- intersect(heatmap_colnames, names(eachsite))
      if (length(heatmap_colnames) == 0) {heatmap_colnames <- NULL}
    }
  }
  if (!is.null(hyperlink_colnames) & !all(hyperlink_colnames %in% names(eachsite)))   {
    warning('all column names in hyperlink_colnames should be found in eachsite table')
    hyperlink_colnames <- intersect(hyperlink_colnames, names(eachsite))
    if (length(hyperlink_colnames) == 0) {hyperlink_colnames <- NULL}
  }
  if (!is.null(narrowcolnames) & !all(narrowcolnames %in% names(eachsite)))   {
    warning('all column names in narrowcolnames should be found in eachsite table')
    narrowcolnames <- intersect(narrowcolnames, names(eachsite))
    if (length(narrowcolnames) == 0) {narrowcolnames <- NULL}
  }
  
  # error checking graycolnames is later, below.
  ## replace missing column headers with friendly names? doubt they are ever missing some
  
  headers_overall  <- names(overall)
  headers_eachsite <- names(eachsite)
  
  # replace Inf with NA to remove #NUM! errors in Excel
  overall   <- overall |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  eachsite <- eachsite |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  
  if (testing) {
    cat('\n   names of hyperlink_colnames \n\n')
    print(hyperlink_colnames)
  }
  ############################################## # 
  # NAME EACH WORKBOOK SHEET ####
  
  ##
  if(is.function(updateProgress)){
    boldtext <- 'Creating workbook sheets'
    updateProgress(message_main = boldtext, value = 0.1)
  }
  
  wb <- openxlsx::createWorkbook()
  
  ## Overall sheet and Each Site sheet ####
  
  openxlsx::addWorksheet(wb, sheetName = 'Each Site')
  openxlsx::addWorksheet(wb, sheetName = 'Overall'  )
  if (!is.null(formatted)) {openxlsx::addWorksheet(wb, sheetName = 'Overall 2') }
  openxlsx::addWorksheet(wb, sheetName = 'longnames')
  openxlsx::addWorksheet(wb, sheetName = 'map') 
  
  # openxlsx::addWorksheet(wb, sheetName = 'bybg') # a lot of rows and not essential except to calculate distance vs demog group stats/plots
  # the plot sheets and notes sheet are created below
  
  ######################################################################## #
  
  if(is.function(updateProgress)){
    boldtext <- 'Adding plots'
    updateProgress(message_main = boldtext, value = 0.2)
  }
  
  ## write to PLOT sheets ####
  if (ok2plot)  {
    if (plotlatest) {
      # inserts the last plot that was drawn before this function was called
      openxlsx::addWorksheet(wb, sheetName = "plot2",  gridLines = FALSE)
      openxlsx::insertPlot(wb, sheet = 'plot2', 
                           fileType = 'png',
                           width = 9, height = 7)
    }
    
    if (is.null(summary_plot)) {
      
      # None provided so try to create one anyway? 
      # example: plot_barplot_ratios( unlist( testoutput_ejamit_1000pts_1miles$results_overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]))
      cat('plotting ratios to avg by group\n')
      if (data.table::is.data.table(overall)) {
        summary_plot <- try( plot_barplot_ratios(unlist( overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]),
                                                 names2plot_friendly = fixcolnames(c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), oldtype = 'r', newtype = 'shortlabel')) )
      } else {
        summary_plot <- try( plot_barplot_ratios(unlist(as.data.frame(overall[ , c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg) ])),
                                                 names2plot_friendly = fixcolnames(c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), oldtype = 'r', newtype = 'shortlabel')) )
      }
      if (inherits(summary_plot, "try-error")) {
        warning('cannot create plot_barplot_ratios() output')
        cat('cannot create plot_barplot_ratios() output\n')
      } else {
        print(summary_plot)
        openxlsx::addWorksheet(wb, sheetName = "plot_ratios",  gridLines = FALSE)
        openxlsx::insertPlot(wb, "plot_ratios", width = 9, height = 7) # The current plot is saved to a temporary image file using dev.copy. This file is then written to the workbook using insertImage.
      }
      
    } else {
      # add summary_plot ggplot object  to 'plot' sheet of Excel download 
      cat('adding summary_plot (ggplot output) that was provided\n')
      mytempdir <- tempdir() # did not work on server?
      openxlsx::addWorksheet(wb, sheetName = "plot",  gridLines = FALSE)
      ggplot2::ggsave(filename = paste0(mytempdir, '/', 'summary_plot.png'), plot = summary_plot,
                      width = 9, height = 7, units = 'in')
      openxlsx::insertImage(wb, sheet = 'plot', 
                            file = paste0(mytempdir, '/', 'summary_plot.png'),
                            width = 9, height = 7)
    }
    
    if (!is.null(bybg) & plot_distance_by_group) {
      cat('plotting mean distance by group\n')
      fname  <- try(
        suppressWarnings(
          plot_distance_mean_by_group(bybg, 
                                      demogvarname = c(names_d, names_d_subgroups), 
                                      demoglabel = fixcolnames(c(names_d, names_d_subgroups), 
                                                               oldtype = 'r', newtype = 'shortlabel'),
                                      returnwhat = "plotfilename", graph = TRUE)
        )
      )
      if (inherits(fname, "try-error")) {
        fname <- NULL; warning('cannot create distance table')
      } else {
        openxlsx::addWorksheet(wb, sheetName = "plot_distances",  gridLines = FALSE)
        #plot_distances
        openxlsx::insertImage(wb, sheet = "plot_distances", file = fname, width = 11, height = 7) #  The current plot gets inserted
        # openxlsx::insertPlot(wb, "plot_distances", width = 9, height = 7) #  The current plot gets inserted
      }
      # plotfilename <- try(plot_distance_mean_by_group(bybg, returnwhat = "plotfilename"))
      # if (inherits(plotfilename, "try-error")) {plotfilename <- NULL; warning('cannot create plot using plot_distance_mean_by_group')}
    }
  }
  
  # if (!is.null(plotfilename)) {
  #   if (file.exists(plotfilename)) {
  #     openxlsx::addWorksheet(wb, sheetName = "plotfile",  gridLines = FALSE)
  #     openxlsx::insertImage( wb, sheet =     'plotfile', 
  #                            file = plotfilename,
  #                            width = 14, height = 7)
  #   } else {warning(plotfilename, " not found")}
  # }
  
  ######################################################################## #
  ## write to map sheet ####
  
  if(is.function(updateProgress)){
    boldtext <- 'Adding map'
    updateProgress(message_main = boldtext, value = 0.3)
  }
  
  ## save html to png   -    THIS IS VERY SLOW HOWEVER. THERE ARE FASTER WAYS THAN CREATING A WIDGET AND THEN TURNING IT INTO A SIMPLE PNG
  if (mapadd) {
    mytempdir <- tempdir()
    mypath <- file.path(mytempdir, "temp.html")
    cat("drawing map\n")
    ## add map from Shiny app if applicable
    if(!is.null(report_map)){
      htmlwidgets::saveWidget(report_map, mypath, selfcontained = FALSE)
    } else {
      z <- mapfast(eachsite, radius = radius_or_buffer_in_miles, column_names = 'ej')
      htmlwidgets::saveWidget(z, mypath, selfcontained = FALSE)
    }
    webshot::webshot(mypath, file = file.path(mytempdir, "map1.png"), cliprect = "viewport")
    if (testing) cat(file.path(mytempdir, "map1.png"), '\n')
    openxlsx::insertImage(wb, sheet = 'map', file = file.path(mytempdir, 'map1.png'),
                          width = 9, height = 7)
  }
  
  ######################################################################## #
  ## write to NOTES sheet  ####
  
  openxlsx::addWorksheet(wb, sheetName = "notes", gridLines = FALSE)
  
  # confirm what is radius or buffer length
  if (is.null(radius_or_buffer_in_miles)) {
    # try to find in eachsite or set to "Not Specified"
    if ('radius.miles' %in% names(eachsite)) {
      radius_or_buffer_in_miles <- eachsite$radius.miles[1]
    } else {
      radius_or_buffer_in_miles <- "Not Specified"
    }
  }
  notes_df <- data.frame(
    'Analysis Title' = analysis_title,
    'Number of Locations Analyzed' = NROW(eachsite),
    'Locations analyzed' = buffer_desc,
    "Distance in miles" = radius_or_buffer_in_miles, 
    "Distance type" = radius_or_buffer_description,
    "Population at x% of sites" =  popshare_p_lives_at_what_pct(eachsite$pop, p = 0.50, astext = TRUE),
    "Population at N sites" = popshare_at_top_n(eachsite$pop, c(1, 5, 10), astext = TRUE),
    check.names = FALSE
  )
  
  notes_df <- as.data.frame(  t(notes_df) )
  
  
  openxlsx::writeData(    wb, sheet = 'notes', x = notes_df,       rowNames = TRUE,  colNames = FALSE)
  
  if (!is.null(notes)) {
    notes_usertext <- cbind(notes)
    usernoterows <- NROW(notes_usertext); if (!(usernoterows > 0)) {usernoterows <- 1}
    openxlsx::writeData(    wb, sheet = 'notes', x = notes_usertext, rowNames = FALSE, colNames = FALSE, startRow = NROW(notes_df) + 3)
    openxlsx::addStyle(     wb, sheet = 'notes', rows = 1:(usernoterows + NROW(notes_df)), cols = 1,  style = openxlsx::createStyle(wrapText = TRUE), stack = TRUE)
  } else {usernoterows <- 0}
  openxlsx::setRowHeights(wb, sheet = 'notes', rows = 1:(usernoterows + NROW(notes_df)), heights = 50)
  openxlsx::setColWidths( wb, sheet = 'notes', cols = 1:4,            widths = "auto")
  
  
  ######################################################################## #
  # Write Tables to Data sheets ####
  ######################################################################## #
  
  if(is.function(updateProgress)){
    boldtext <- 'Writing data to sheets'
    updateProgress(message_main = boldtext, value = 0.4)
  }
  
  openxlsx::writeData(wb, 
                      sheet = 'Overall', x = overall, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 
                      keepNA = FALSE, # NA converted to blank or to #N/A
                      ...
  )
  # CHANGE SO IT IS NOT data.table, 
  if (data.table::is.data.table(eachsite)) {
    data.table::setDF(eachsite) # to make syntax below work since it was written assuming data.frame only not data.table
  }
  # REPLACE THE URLS WITH GOOD ONES
  #eachsite[ , hyperlink_colnames] <-  (url_4table(eachsite$lat, eachsite$lon, radius = radius_or_buffer_in_miles, as_html = FALSE))$results_bysite
  eachsite[eachsite$valid==T , hyperlink_colnames] <-  (url_4table(eachsite$lat[eachsite$valid==T], eachsite$lon[eachsite$valid==T], radius = radius_or_buffer_in_miles, as_html = FALSE))$results_bysite
  eachsite[eachsite$valid==F , hyperlink_colnames] <-  NA# (url_4table(lat=NULL, lon=NULL, radius = radius_or_buffer_in_miles, as_html = FALSE))$results_bysite
  
  openxlsx::writeData(wb, 
                      sheet = 'Each Site', x = eachsite, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = TRUE, # not sure if this gets undone when we later write more data to worksheet
                      keepNA = FALSE,   # NA converted to blank or to #N/A
                      ...
  )
  
  openxlsx::writeData(wb, 
                      sheet = 'longnames', x = cbind(longnames = longnames, rname = headers_eachsite ), 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 
                      keepNA = FALSE,
                      ...
  )
  if (!is.null(formatted)) {
    openxlsx::writeData(wb, 
                        sheet = 'Overall 2', x = formatted, 
                        xy = c(1,1), colNames = TRUE, 
                        withFilter = FALSE, 
                        keepNA = FALSE
    )
    openxlsx::setColWidths(wb, "Overall 2", cols = 2, widths = 90)
  }
  
  ######################################################################## #
  # HYPERLINKS ####
  # special names for the pdf and map links #
  ######################################################################## #
  
  # ## External Hyperlink -- HOW TO DO THIS:
  # 
  # x <- c("https://www.google.com", "https://www.google.com.au")
  # names(x) <- c("google", "google Aus")
  # class(x) <- "hyperlink"
  # writeData(wb, sheet = 1, x = x, startCol = 10)
  
  # recently how it ends up from e.g., doagg/ejamlite
  # <a href="https://ejscreen.epa.gov/mapper/index.html?wherestr=40.70103,-75.12058", target="_blank" rel="noreferrer noopener">EJScreen Map</a>
  
  # output of ejamit()$results_bysite have a  EJScreen Report  column that has values like this:   
  # <a href="https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-122.64108,%22y%22:45.55001%7D&distance=1&unit=9035&areatype=&areaid=&f=report", target="_blank" rel="noreferrer noopener">EJScreen Report</a>
  
  # output from app_server code, ready to get sent to table_xls_format(), is like this:
  #   url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE)
  # "<a href=\"https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-103,%22y%22:42%7D&distance=3&unit=9035&areatype=&areaid=&f=report\", target=\"_blank\" rel=\"noreferrer noopener\">EJScreen Report</a>"
  
  # ### code from ejscreenapi that was to make these columns work, somewhat generic naming possible
  
  if(is.function(updateProgress)){
    boldtext <- 'Formatting hyperlinks'
    updateProgress(message_main = boldtext, value = 0.6)
  }
  
  if (!is.null(hyperlink_colnames)) {
    
    hypercolnums <- match(hyperlink_colnames, names(eachsite)) # returns the position of each, or NA if not found. is that what we want here??
    if (testing) {cat("trying to apply hyperlinks to column numbers ", paste0(hypercolnums, collapse = ", "), "\n")}    
    hyperlink_text <- hyperlink_colnames
    
    if (data.table::is.data.table(eachsite)) {
      data.table::setDF(eachsite) # to make syntax below work since it was written assuming data.frame only not data.table
    }
    # failed:
    # # convert from html back to simple URL
    # eachsite[, hyperlink_colnames ] <- gsub('.*(http.*)\", target=.*', '\\1',  eachsite[ , hyperlink_colnames ] )
    # 
    
    # NOW CONVERT SIMPLE URLS INTO EXCEL HYPERLINKS
    
    for (i in 1:length(hyperlink_colnames)) {
      # not sure it has to be in a loop actually but only 2 or 3 columns to loop over
      namedvector <- as.vector(eachsite[ , hyperlink_colnames[i]])
      namedvector[namedvector <- 'N/A'] <- NA
      class(namedvector) <- "hyperlink"
      names(namedvector) <- paste(hyperlink_text[i], 1:(NROW(eachsite))) # NOT NROW + 1 HERE !  # to use e.g., "EJScreen Report 1" 
      ## write to the worksheet the revised URL
      openxlsx::writeData(wb, sheet = 'Each Site',
                          x = namedvector,   
                          startRow = 2, startCol = hypercolnums[i])
    }
  }
  
  ######################################################################## #
  # end of hyperlink code
  ######################################################################## #
  
  
  ## check variable type of each column ####
  #
  # The vartypes are these: 
  # c("raw", 
  #   "usratio",  "stateratio",
  #   "uspctile", "statepctile", 
  #   "usavg",    "stateavg",   
  #   "usbin",    "statebin", 
  #   "ustext",   "statetext", 
  #   "usraw",    "stateraw", 
  #        "geo")
  vartypes_overall  <- varname2vartype_ejam(headers_overall,  map_headernames)
  vartypes_eachsite <- varname2vartype_ejam(headers_eachsite, map_headernames)
  # but note varname2color_ejam() can return a color appropriate to each variable name
  
  ## ratio colnums
  ratio_colnums_overall  <- which(vartypes_overall  %in% c('usratio', 'stateratio')) 
  ratio_colnums_eachsite <- which(vartypes_eachsite %in% c('usratio', 'stateratio')) 
  
  
  ## define percentile columns
  
  pctile_colnums_overall  <- which(vartypes_overall  %in% c('uspctile', 'statepctile')) #'percentile')
  pctile_colnums_eachsite <- which(vartypes_eachsite %in% c('uspctile', 'statepctile')) #'percentile')
  
  if (is.null(heatmap_colnames)) {
    heatmap_colnums <- pctile_colnums_eachsite
    heatmap_colnames <- headers_eachsite[heatmap_colnums]
  }
  
  ## define PERCENTAGE columns
  
  is.percentage_overall  <- 1 == fixcolnames(headers_overall, oldtype = "r", newtype = "percentage")
  percentage_colnums_overall <- which(is.percentage_overall)
  is.percentage_eachsite  <- 1 == fixcolnames(headers_eachsite, oldtype = "r", newtype = "percentage")
  percentage_colnums_eachsite <- which(is.percentage_eachsite)
  # vlist          <-      fixcolnames(headers_eachsite, oldtype = "r", newtype = "varlist")
  # percentage_colnums_check <-  which(headers_eachsite %in% c("pctpre1960", "avg.pctpre1960", "state.avg.pctpre1960")  |  vlist %in% c(
  #   "names_d", "names_d_avg", "names_d_state_avg", 
  #   "names_d_subgroups", "names_d_subgroups_avg", "names_d_subgroups_state_avg",
  #   "names_d_subgroups_nh", "names_d_subgroups_nh_avg", "names_d_subgroups_nh_state_avg",
  #   "names_d_subgroups_alone", "names_d_subgroups_alone_avg", "names_d_subgroups_alone_state_avg"))
  #   print(setdiff2(percentage_colnums_check, percentage_colnums))
  ######################################################################## #
  
  if(is.function(updateProgress)){
    boldtext <- 'Applying formatting'
    updateProgress(message_main = boldtext, value = 0.7)
  }
  
  # ROW 1 STYLE ####
  
  headstyle_basic <- openxlsx::createStyle(
    wrapText = TRUE, halign = "CENTER", valign = 'center',
    #fgFill = "#4F81BD",  
    textDecoration = "Bold"  , border = "bottom", borderStyle = "medium"
  )
  openxlsx::addStyle(wb, 'Overall',   style = headstyle_basic, stack = TRUE, rows = 1, cols = 1:NCOL(eachsite), gridExpand = TRUE)
  openxlsx::addStyle(wb, 'Each Site', style = headstyle_basic, stack = TRUE, rows = 1, cols = 1:NCOL(eachsite), gridExpand = TRUE)
  
  # FREEZE PANES: ROW 1 AND LEFTMOST COLUMNS ####
  
  # openxlsx::freezePane(wb, sheet = 'Each Site', firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
  openxlsx::freezePane(wb, sheet = 'Each Site', firstActiveCol = 5, firstActiveRow = 2)
  # openxlsx::freezePane(wb, sheet = 'Overall',   firstActiveCol = 1) 
  openxlsx::freezePane(wb, sheet = 'Overall',   firstActiveCol = 1, firstActiveRow = 2)
  openxlsx::freezePane(wb, sheet = 'Overall 2', firstActiveCol = 1, firstActiveRow = 2)
  
  # COLUMN WIDTHS   ####
  
  if (!is.null(narrowcolnames)) {
    narrowcolnums <- match(narrowcolnames, names(eachsite))
    openxlsx::setColWidths(wb, 'Overall',   cols = narrowcolnums, widths = narrow6)
    openxlsx::setColWidths(wb, 'Each Site', cols = narrowcolnums, widths = narrow6)
  }
  openxlsx::setColWidths(wb, "longnames", cols = 1:2, widths = 90)
  
  # HEADER ROW HEIGHT   ####
  
  openxlsx::setRowHeights(wb, sheet = 'Each Site', rows = 1, heights = 175)
  openxlsx::setRowHeights(wb, sheet = 'Overall',   rows = 1, heights = 175)
  
  # HEADER ROW COLOR ####
  
  header_colors_overall  <- varname2color_ejam(headers_overall,  map_headernames)
  header_colors_eachsite <- varname2color_ejam(headers_eachsite, map_headernames)
  
  force_to_be_plain <- c("ejam_uniq_id", "pop", "lon", "lat", "ST", "statename")
  header_colors_overall[ headers_overall  %in% force_to_be_plain] <- "white"
    header_colors_eachsite[headers_eachsite %in% force_to_be_plain] <- "white"  
      
    header_colors_overall[ is.na(header_colors_overall )] <- ("gray")
    header_colors_eachsite[is.na(header_colors_eachsite)] <- ("gray")
    #new_colors <- c(unique(header_colors_overall), unique(header_colors_eachsite))
    new_colors <- unique(c(header_colors_overall, header_colors_eachsite))
    
    for (i in new_colors) {
      style_cur <- openxlsx::createStyle(fgFill = i)
      openxlsx::addStyle(wb, 'Overall',   cols = which(header_colors_overall  == i), rows = 1, style = style_cur, stack = TRUE)
      openxlsx::addStyle(wb, 'Each Site', cols = which(header_colors_eachsite == i), rows = 1, style = style_cur, stack = TRUE) 
    }
    
    # GRAY/shade ENTIRE COLUMNS ####
    if (!is.null(graycolnames)) {
      if (length(graycolor) > 1) {
        warning("Must specify only one value for graycolor. Using default gray.")
        graycolor <- "gray"
      }
      if (length(setdiff(graycolnames, names(eachsite))) > 0) {
        warning(paste0("Some of graycolnames not found among column headers: ", setdiff(graycolnames, names(eachsite)), collapse = ", "), " ")
        graycolnames <- intersect(graycolnames, names(eachsite))
      }
      if (length(graycolnames) > 0) {
        openxlsx::addStyle(wb, 'Overall',   cols = which(names(overall)  %in% graycolnames), rows = 2:(1 + NROW(overall)), style = openxlsx::createStyle(fgFill = graycolor) , stack = TRUE)
        openxlsx::addStyle(wb, 'Each Site', cols = which(names(eachsite) %in% graycolnames), rows = 2:(1 + NROW(overall)), style = openxlsx::createStyle(fgFill = graycolor) , stack = TRUE)
      } else {
        warning(paste0("graycolnames not found among column headers: ", graycolnames, collapse = ", "), " ")}
    }
    ###########################################  ###########################################  ########################################## #
    
    # Numbers HEATMAP / CONDITIONAL FORMATTING  ####
    # to highlight large percentiles in Excel
    
    heatmap_colnames <- intersect(heatmap_colnames, names(eachsite))
    heatmap_colnums <- match(heatmap_colnames, names(eachsite))
    heatmap_cuts <- c(heatmap_cuts, Inf)
    if (length(heatmap_colnames) > 0) {
      
      ## split heatmap columns into sequences of consecutive columns
      hc_split <- split(heatmap_colnums, cumsum(c(1, diff(heatmap_colnums) != 1)))
      
      
      for (i in 1:length(heatmap_colors)) {
        if (testing) {
          cat('heatmap_colnames ', paste0(heatmap_colnames, collapse = ", ") , '\n ... at heatmap_colnums =   \n', paste0(heatmap_colnums, collapse = ", "),' --- for color ', heatmap_colors[i], '\n')
          cat("\n\n")}
        style_cur <- openxlsx::createStyle( bgFill = heatmap_colors[i] )
        
        
        ## need to loop over heatmap columns so it skips columns in between
        #for(j in 1:length(heatmap_colnums)){
        for(k in 1:length(hc_split)){  
        openxlsx::conditionalFormatting(wb, "Overall",    rows = 2 , 
                                        cols = hc_split[[k]],#heatmap_colnums[j],  
                                        style = style_cur,  stack = TRUE,
                                        rule = paste0(">=", heatmap_cuts[i]))
        
        openxlsx::conditionalFormatting(wb, "Each Site",  rows = 2:(1 + NROW(eachsite)), #gridExpand = TRUE,
                                        cols = hc_split[[k]],#heatmap_colnums[j],
                                        style = style_cur, #stack = TRUE,
                                        rule = paste0(">=", heatmap_cuts[i]))
        }
      }
    }
    
    # HEATMAP COLORING FOR SECOND SET OF COLUMNS - THESE ARE RATIOS WITH CUTS 1.0, 2.0, 3.0 x average
    
    heatmap2_colnames <- intersect(heatmap2_colnames, names(eachsite))
    heatmap2_colnums <- match(heatmap2_colnames, names(eachsite))
    heatmap2_cuts <- c(heatmap2_cuts, Inf)
    
    ## split heatmap columns into sequences of consecutive columns
    hc2_split <- split(heatmap2_colnums, cumsum(c(1, diff(heatmap2_colnums) != 1)))
    
    for (i in 1:length(heatmap2_colors)) {
      if (testing) {
        cat('heatmap2_colnames ', paste0(heatmap2_colnames, collapse = ", ") , '\n ... at heatmap2_colnums =   \n', paste0(heatmap2_colnums, collapse = ", "),' --- for color ', heatmap2_colors[i], '\n')
        cat("\n\n")}
      style_cur <- openxlsx::createStyle( bgFill = heatmap2_colors[i] )
      
      ## need to loop over heatmap columns so it skips columns in between
      #for(j in 1:length(heatmap2_colnums)){
      for(k in 1:length(hc2_split)){
      openxlsx::conditionalFormatting(wb, "Overall",    rows = 2 ,
                                      cols = hc2_split[[k]],#heatmap2_colnums[j],
                                      style = style_cur, stack = TRUE,
                                      rule = paste0(">=", heatmap2_cuts[i]))
      
      openxlsx::conditionalFormatting(wb, "Each Site",  rows = 2:(1 + NROW(eachsite)), #gridExpand = TRUE,
                                      cols = hc2_split[[k]],#heatmap2_colnums[j],
                                      style = style_cur, #stack = TRUE,
                                      rule = paste0(">=", heatmap2_cuts[i]))
      }
    }
    
    
    # conditionalFormatting(wb, "colourScale",
    #                       cols = 1:ncol(df), rows = 1:(1 + NROW(df)),
    #                       style = createStyle(bgFill = "yellow"),
    #                       rule = c(80,89.999),
    #                       type = "between"
    # ) 
    # conditionalFormatting(wb, "colourScale",
    #                       cols = 1:ncol(df), rows = 1:(1 + NROW(df)),
    #                       style = createStyle(bgFill = "orange"),
    #                       rule = c(90,94.999),
    #                       type = "between"
    # )
    # conditionalFormatting(wb, "colourScale",
    #                       cols = 1:ncol(df), rows = 1:(1 + NROW(df)),
    #                       style = createStyle(bgFill = "red"),
    #                       rule = c(95,100),
    #                       type = "between"
    # )
    ###########################################  ###########################################  ########################################## #
    
    
    # NUMBER FORMATS ####
    
    ###   decimal places / rounding  ####
    
    ### Number format default for raw indicator columns - should get replaced though by table_round() or table_rounding_info() in most or all cases  ####
    
    raw_colnums_overall   <- which(vartypes_overall  == 'raw data for indicator')
    raw_colnums_eachsite  <- which(vartypes_eachsite == 'raw data for indicator')
    raw_var_style <- openxlsx::createStyle(numFmt = '#,##0.00')
    # openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = raw_colnums_overall,  style=raw_var_style, stack = TRUE)
    # openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:((1 + NROW(eachsite)), cols = raw_colnums_eachsite, style=raw_var_style, stack = TRUE, gridExpand = TRUE)
    
    # GET INFO FROM map_headernames THAT SPECIFIES NUMBER OF DECIMAL PLACES FOR MOST OR ALL INDICATORS !!
    #
    # How many decimals (or ideally significant digits but not possible here) to report?  This could be done via rounding values before put in excel,
    #  OR probably better, sending exact values to Excel but using Excel formatting to display them correctly.
    # 
    # sigfigs_table <-  map_headernames[ "" != (map_headernames$sigfigs), c("sigfigs", "decimals", "rname", "acsbgname",	"csvname2.2")]
    digitstable <- map_headernames[ "" != (map_headernames$decimals) | "" != (map_headernames$sigfigs), c("sigfigs", "decimals", "rname", "acsbgname",	"csvname2.2")]
    decimals_cols <- names(eachsite)[names(eachsite) %in% digitstable$rname[digitstable$decimals != ""]]
    decimals_colnum <- match(decimals_cols, names(eachsite)) # and overall has same exact names and sort order of names
    decimals_tosee <- digitstable$decimals[match(decimals_cols, digitstable$rname)]
    dec2format <- function(decimalscount) ifelse(decimalscount == 0, "0", paste0("0.", paste0(rep("0", decimalscount), collapse = '')))
    # dec2formats <- Vectorize(dec2format)
    ## only loop over unique values
    for (i in unique(decimals_tosee)) {
      style_cur <- openxlsx::createStyle(numFmt = dec2format(i))
      openxlsx::addStyle(wb, 'Overall',   cols = decimals_colnum[decimals_tosee == 'i'], rows = 2                    ,  style = style_cur, stack = TRUE)
      openxlsx::addStyle(wb, 'Each Site', cols = decimals_colnum[decimals_tosee == 'i'], rows = 2:(1 + NROW(eachsite)), style = style_cur, stack = TRUE, gridExpand = TRUE)
    }
    # for (i in 1:length(decimals_cols)) {
    #   style_cur <- openxlsx::createStyle(numFmt = dec2format(decimals_tosee[i]))
    #   openxlsx::addStyle(wb, 'Overall',   cols = decimals_colnum[i], rows = 2                    ,  style = style_cur, stack = TRUE)
    #   openxlsx::addStyle(wb, 'Each Site', cols = decimals_colnum[i], rows = 2:(1 + NROW(eachsite)), style = style_cur, stack = TRUE, gridExpand = TRUE)
    # }
    
    ### distances should only have about 2 decimal places ####
    
    distance_colnums <- which(grepl("distance_", names(eachsite)))
    openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = distance_colnums, style = openxlsx::createStyle(numFmt = '#,##0.00'), stack = TRUE)
    openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = distance_colnums, style = openxlsx::createStyle(numFmt = '#,##0.00'), stack = TRUE, gridExpand = TRUE)
    
    ### RATIO - rounded to one decimal place    ####
    ratio_var_style <- openxlsx::createStyle(numFmt = '#,##0.0')
    openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = ratio_colnums_overall,  style = ratio_var_style, stack = TRUE)
    openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = ratio_colnums_eachsite, style = ratio_var_style, stack = TRUE, gridExpand = TRUE)
    
    
    ### PERCENTILE - rounded, integer 0-100 format    ####
    
    pctile_var_style <- openxlsx::createStyle(numFmt = '#0')
    openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = pctile_colnums_overall,  style = pctile_var_style, stack = TRUE)
    openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = pctile_colnums_eachsite, style = pctile_var_style, stack = TRUE, gridExpand = TRUE)
    
    ### PERCENTAGE format for Demog percentages columns####
    
    percentage_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")   # specify 0 decimal places plus percentage style
    percentage_style <- openxlsx::createStyle(numFmt = "0%")   # specify 0 decimal places plus percentage style
    openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = percentage_colnums_overall, style = percentage_style, stack = TRUE)
    openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = percentage_colnums_eachsite, style = percentage_style, stack = TRUE, gridExpand = TRUE)
    # openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = percentage_colnums, style=openxlsx::createStyle(numFmt = '#0'), stack = TRUE)
    # openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:((1 + NROW(eachsite)), cols = percentage_colnums, style=openxlsx::createStyle(numFmt = '#0'), stack = TRUE, gridExpand = TRUE)  
    
    ### Number format total count columns  ####
    
    count_colnums_overall  <- c(which(headers_overall == 'pop'), which(vartypes_overall  == 'count demog'))
    count_colnums_eachsite <- c(which(headers_overall == 'pop'), which(vartypes_eachsite == 'count demog'))
    count_var_style <- openxlsx::createStyle(numFmt = "#,###,###" )
    openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = count_colnums_overall,  style = count_var_style, stack = TRUE)
    openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = count_colnums_eachsite, style = count_var_style, stack = TRUE, gridExpand = TRUE)
    
    ### apply a default general number format to all OTHER columns ? or just assume all were already covered above  ***  ####
    # 
    # other_colnums_overall  <- setdiff(1:length(vartypes_overall),  c(decimals_colnum, distance_colnums, heatmap_colnums, pctile_colnums_overall,  raw_colnums_overall,  percentage_colnums, count_colnums_overall))
    # other_colnums_eachsite <- setdiff(1:length(vartypes_eachsite), c(decimals_colnum, distance_colnums, heatmap_colnums, pctile_colnums_eachsite, raw_colnums_eachsite, percentage_colnums, count_colnums_eachsite))
    # other_var_style <- openxlsx::createStyle(numFmt = '##,##0.00')
    # openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = other_colnums_overall,  style = other_var_style, stack = TRUE)
    # openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(1 + NROW(eachsite)), cols = count_colnums_eachsite, style = other_var_style, stack = TRUE, gridExpand = TRUE)
    
    
    # can group columns too, to help user hide some of them
    average_colnums_eachsite <- which(vartypes_eachsite %in% c('usavg', 'stateavg'))
    openxlsx::groupColumns(wb, "Each Site", cols = average_colnums_eachsite, hidden = TRUE)
    
    
    ###########################################  ###########################################  ########################################## #
    
    # RENAME TOP ROW TO long names ####
    # how to replace the header row but without replacing any formatting! ***
    openxlsx::writeData(wb, sheet = 'Overall',   x = as.data.frame(rbind(longnames_overall), row.names = NULL), xy = c(1,1), colNames = FALSE)
    openxlsx::writeData(wb, sheet = 'Each Site', x = as.data.frame(rbind(longnames), row.names = NULL), xy = c(1,1), colNames = FALSE)
    
    # add FILTER ROW 1 in case it did not remain in place
    openxlsx::addFilter(wb, "Each Site", rows = 1, cols = 1:ncol(eachsite))
    
    # openxlsx::addWorksheet(wb, "Overall tall")
    # openxlsx::writeData(wb, 
    #                     sheet = 'Overall tall', x = t(overall), 
    #                     xy = c(1,1), colNames = TRUE, 
    #                     withFilter = FALSE, 
    #                     keepNA = FALSE, # NA converted to blank or to #N/A
    #                     ...
    # )
    
    # Rename longnames tab or remove it
    
    # put overall 2 as 2d tab
    
    if(is.function(updateProgress)){
      boldtext <- 'Saving file'
      updateProgress(message_main = boldtext, value = 1)
    }
    
    # Launch in Excel before saving file ####
    if (launchexcel) {
      openxlsx::openXL(wb)
    }
    # SAVEAS local file ####
    if (!is.null(saveas)) {
      thatfolder = dirname(saveas)
      
      if (file.exists(thatfolder)) {
        fname = basename(saveas)
        xext = gsub(".*\\.(x.*)","\\1", fname)
        if (xext %in% c("xls", "xlsx")) {
          attempt = try(openxlsx::saveWorkbook(wb, file = saveas, overwrite = TRUE))
          cat('Saving as ', saveas, '\n')
        } else {
          warning(saveas, ' does not appear to be a path and filename ending in .xls or .xlsx')
        }
      } else {
        warning(thatfolder, ' folder does not appear to exist')
      }
    }
    # done ###################### 
    
    return(wb)
}
################################################################################# # 







#' helper function - assign fill color to shade excel cells by indicator type and category
#' 
#' Use color shading to make spreadsheet easier to use, grouping the indicators
#'
#' @param vartype must be one found in dput(unique(map_headernames$vartype)) 
#'   like "usratio", "stateratio", "usraw", "stateraw", 
#'   "uspctile", "statepctile", "usavg", "stateavg", etc.
#'   NA if not found.
#' @param varcategory must be one of "Demographic"   "Environmental" "EJ Index" "other" 
#'   as from dput(unique(map_headernames$varcategory))
#' @return vector of colors like c('lightblue', 'gray') matching length of vartype
#' @seealso [varinfo()] [varname2vartype_ejam()]  [varname2varcategory_ejam()] [varname2color_ejam()]
#' @export
#'
vartype_cat2color_ejam <- function(vartype=raw, varcategory="other") {
  
  vartype_cat <- paste0(vartype, "_", varcategory) # e.g., 
  
  # > unique(map_headernames$varcategory)
  # [1] "Demographic"   "Environmental" "EJ Index"      "other"  
  
  # The vartypes are these (but not they might get modified by varname2vartype_ejam() 
  # dput(unique(map_headernames$vartype)) 
  # c("raw",
  #   "usraw",    "stateraw", 
  #   "uspctile", "statepctile", 
  #   "usavg",    "stateavg",   
  #   "usratio",  "stateratio",
  # 
  #   "usbin",    "statebin", 
  #   "ustext",   "statetext", 
  #        "geo")  
  
  # TO SEE THE RESULTS OF THE COLOR ASSIGNMENTS:   
  # cbind(rname = names(EJAM :: testoutput_ejamit_10pts_1miles$results_bysite), vartype = map_headernames$vartype[match(names(EJAM :: testoutput_ejamit_10pts_1miles$results_bysite), map_headernames$rname)], vartype_cat2color_ejam = vartype_cat2color_ejam(map_headernames$vartype[match(names(testoutput_ejamit_10pts_1miles$results_bysite), map_headernames$rname)]), varname2color_ejam = varname2color_ejam(names(EJAM :: testoutput_ejamit_10pts_1miles$results_bysite)))
  # cbind(rname = names(        testoutput_ejamit_10pts_1miles$results_bysite), vartype = map_headernames$vartype[match(names(    testoutput_ejamit_10pts_1miles$results_bysite), map_headernames$rname)], vartype_cat2color_ejam = vartype_cat2color_ejam(map_headernames$vartype[match(names(testoutput_ejamit_10pts_1miles$results_bysite), map_headernames$rname)]), varname2color_ejam = varname2color_ejam(names(    testoutput_ejamit_10pts_1miles$results_bysite))) 
  
  # for shading headers in Excel of results
  coloring <- matrix(
    c(
      'ratio_other',                "lightblue",  #  '#FFD700', # "gold"  
      "usratio_other",                "lightblue",  #   '#FFD700', # "gold"
      "stateratio_other",             "lightblue2",  #  '#FFA500', # "orange" 
      
      'ratio_Environmental',                "green2",  #  '#FFD700', # "gold"  
      "usratio_Environmental",                "green2",  #   '#FFD700', # "gold"
      "stateratio_Environmental",             "green1",  #  '#FFA500', # "orange"
      
      'ratio_Demographic',                "turquoise2",  #  '#FFD700', # "gold"  
      "usratio_Demographic",                "turquoise2",  #   '#FFD700', # "gold"
      "stateratio_Demographic",             "turquoise1",  #  '#FFA500', # "orange" 
      
      'ratio_EJ Index',                "mistyrose3",  #  '#FFD700', # "gold"  
      "usratio_EJ Index",                "mistyrose3",  #   '#FFD700', # "gold"
      "stateratio_EJ Index",             "mistyrose2",  #  '#FFA500', # "orange" 
      
      
      'percentile_other',              'gray95', #  
      "uspctile_other",                'gray95', # 
      "statepctile_other",            "gray98"   , #  
      
      'percentile_Environmental',              'palegreen2', #  "lightblue"
      "uspctile_Environmental",                'palegreen2', #  "lightblue"
      "statepctile_Environmental",            "palegreen1"   , #  
      
      'percentile_Demographic',              'skyblue2', #  "lightblue"
      "uspctile_Demographic",               'skyblue2', #  "lightblue"
      "statepctile_Demographic",            "skyblue1"   , #  
      
      ' percentile_EJ Index',              'mistyrose3', #  "lightblue"
      "uspctile_EJ Index",                'mistyrose3', #  "lightblue"
      "statepctile_EJ Index",            "mistyrose2"   , #  
      
      
      'raw data for indicator_other' , "gray95", # "gray90", # '#FFD700', # "orange"
      "raw_other",                    "gray95", # "gray90", # # '#FFD700', # "orange"
      "usraw_other",                   "gray95", # "gray90", #  '#FFD700', # "orange"
      "stateraw_other",                "gray98", # '#FFA500', # "orange"
      
      'raw data for indicator_Environmental' , "palegreen3", # "gray90", # '#FFD700', # "orange"
      "raw_Environmental",                    "palegreen3", # "gray90", # # '#FFD700', # "orange"
      "usraw_Environmental",                   "palegreen3", # "gray90", #  '#FFD700', # "orange"
      "stateraw_Environmental",                "palegreen2", # '#FFA500', # "orange"
      
      'raw data for indicator_Demographic' , "lightcyan2", # "gray90", # '#FFD700', # "orange"
      "raw_Demographic",                    "lightcyan2", # "gray90", # # '#FFD700', # "orange"
      "usraw_Demographic",                   "lightcyan2", # "gray90", #  '#FFD700', # "orange"
      "stateraw_Demographic",                "lightcyan", # '#FFA500', # "orange"
      
      'raw data for indicator_EJ Index' , "mistyrose4", # "gray90", # '#FFD700', # "orange"
      "raw_EJ Index",                    "mistyrose4", # "gray90", # # '#FFD700', # "orange"
      "usraw_EJ Index",                   "mistyrose4", # "gray90", #  '#FFD700', # "orange"
      "stateraw_EJ Index",                "mistyrose3", # '#FFA500', # "orange"
      
      
      'average_other',                 "grey70" , # '#90EE90', # "lightgreen"
      "usavg_other",                  "grey70" ,  #  '#90EE90', # "lightgreen"
      "stateavg_other",              "gray80" ,  
      
      'average_Environmental',                 "palegreen4" , # '#90EE90', # "lightgreen"
      "usavg_Environmental",                  "palegreen4" ,  #  '#90EE90', # "lightgreen"
      "stateavg_Environmental",              "palegreen4" ,  
      
      'average_Demographic',                 "lightcyan3" , # '#90EE90', # "lightgreen"
      "usavg_Demographic",                  "lightcyan3" ,  #  '#90EE90', # "lightgreen"
      "stateavg_Demographic",              "lightcyan2" ,  
      
      'average_EJ Index',                 "mistyrose4" , # '#90EE90', # "lightgreen"
      "usavg_EJ Index",                  "mistyrose4" ,  #  '#90EE90', # "lightgreen"
      "stateavg_EJ Index",              "mistyrose3" ,  
      
      
      'count demog_other',               "white",
      'misc_other',                "white",   # '#BEBEBE'  # "gray"
      
      'count demog_Environmental',         'darkgreen',
      'misc_Environmental',                "darkgreen",   # '#BEBEBE'  # "gray"
      
      'count demog_Demographic',             "white",
      'misc_Demographic',                "white",   # '#BEBEBE'  # "gray"
      
      'count demog_EJ Index',             'lightcyan4', #
      'misc_EJ Index',                "lightcyan4"      
      
    ), 
    ncol = 2, byrow = TRUE
  )
  colnames(coloring) <- c('vartype_cat', 'color')
  coloring[match(vartype_cat, coloring[, 'vartype_cat'], nomatch = NA) , 'color']
}
################################################################################# # 


#' helper function - for color coding excel sheet columns
#' 
#' Convert R variable name of indicator to appropriate color for header row in Excel
#' @param varname things like us.avg.pctlowinc 
#'
#' @return vector of colors
#' @seealso [varinfo()] [varname2vartype_ejam()] [varname2varcategory_ejam()] [vartype_cat2color_ejam()]
#' @export
#'
varname2color_ejam <- function(varname, varnameinfo) {
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep('black',length(varname)))
    }
  }
  hues_vector <- vartype_cat2color_ejam(
    vartype =     varname2vartype_ejam(    varname = varname, varnameinfo = varnameinfo),
    varcategory = varname2varcategory_ejam(varname = varname, varnameinfo = varnameinfo)
  )
  return(hues_vector)
}
################################################################################# # 


#' helper function - given indicator names, look up what type each is
#' 
#' @details  
#'   The types are things like raw data count for indicator, average, percentile, etc.
#' @param varname vector of 1 or more names
#' @param varnameinfo data.frame with info on type of each variable
#'
#' @return vector same size as varname
#' @seealso [varinfo()] [vartype_cat2color_ejam()] [varname2color_ejam()]
#' @export
#'
varname2vartype_ejam <- function(varname, varnameinfo) {
  
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep(NA, length(varname)))
    }
  }
  # > unique(map_headernames$vartype)
  #     "raw" "geo"      
  # "usratio"     "stateratio" 
  # "uspctile"    "statepctile" 
  # "usavg"       "stateavg"   
  # "usbin"       "statebin"    # not returned by EJAM
  # "ustext"      "statetext"    # not returned by EJAM   
  # "usraw"       "stateraw"    
  
  cur_matches <- varnameinfo[match(varname, varnameinfo[ , 'rname'], nomatch = NA) , 'vartype']
  # but then this would have overridden many of those types specified in map_headernames :
  cur_matches[grep('pop',        varname)] <- 'count demog'   # it is "raw" in map_headernames
  # cur_matches[grep('^avg',       varname)] <- 'average' # these were all "usavg" in map_headernames; vartype is more usefully returning  "usratio" vs "stateratio"
  # cur_matches[grep('^ratio.to.', varname)] <- 'ratio'   # these are "usratio" or "stateratio" which is a useful distinction for colorcoding
  cur_matches[cur_matches == 'n'] <- NA
  cur_matches[cur_matches ==  ""] <- NA
  return(cur_matches)
}
################################################################################# # 


#' helper function - given indicator names, look up what category each is
#' 
#' @details  
#' tells if variable is "Demographic" "Environmental" "EJ Index" or "other"
#'   as from dput(unique(map_headernames$varcategory))
#' @param varname vector of 1 or more names like "pctlowinc" as in unique(map_headernames$rname)
#' @param varnameinfo data.frame with info on type of each variable
#'
#' @return vector same size as varname
#' @seealso [varinfo()] [vartype_cat2color_ejam()] [varname2color_ejam()]
#' @export
#'
varname2varcategory_ejam <- function(varname, varnameinfo) {
  
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep(NA, length(varname)))
    }
  }
  # > unique(map_headernames$varcategory)
  # "Demographic" "Environmental" "EJ Index" or "other"
  
  cur_matches <- varnameinfo[match(varname, varnameinfo[ , 'rname'], nomatch = NA) , 'varcategory']
  cur_matches[cur_matches == 'n'] <- NA
  cur_matches[cur_matches ==  ""] <- NA
  return(cur_matches)
}
################################################################################# # 
