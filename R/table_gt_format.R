#' table_gt_format - Format a table of demog or envt scores, percentiles, etc. to look similar to EJScreen report tables
#' 
#' @param df A data frame like the output of 
#'   
#'   table_gt_from_overall(testoutput_ejamit_100pts_1miles$results_overall) 
#'   
#'   which is just a specific format of key results overall.
#' 
#'   It has these columns (but it still works if the first two are omitted 
#'   and user-provided indicators are used - it just names them indicator 1, indicator 2, etc.): 
#'   
#'   varnames_r, varnames_shown, value, state_avg, state_pctile, usa_avg, usa_pctile 
#'   
#'    and one row per indicator, where varnames_shown are longer indicator names for use in report.
#'    
#'    The sort order in this df is ignored!  Instead, the variables are shown in the same order as
#'    shown in EJScreen reports, as recorded in map_headernames and checked here via varinfo(varnames_r, "reportsort"), etc.
#'    
#'    Uses gt R package for formatting.
#'    
#' @param type, string - must be demog or envt 
#' @param my_cell_color,   color for table cell fill backgrounds,  can be given as string ('blue') or hex code ('#0070c0')
#' @param my_border_color, color for table borders and boundaries, can be given as string ('blue') or hex code ('#0070c0')
#' @seealso [table_gt_from_overall()]
#' @return a gt-style table with formatting to closely match EJScreen standard report formatting
#' 
#' @examples 
#'   x <- table_gt_format(
#'          table_gt_from_overall(
#'            testoutput_ejamit_100pts_1miles$results_overall))
#' 
#' @export
#'
table_gt_format <- function(df, type="demog", my_cell_color =  '#dce6f0', my_border_color = '#aaaaaa') {
  
  # nearRcolor('#dce6f0') is "aliceblue"
  # nearRcolor('#aaaaaa') is "darkgray"
  
  # handle missing info, and make it possible to use this function even without providing all the columns:
  # *** might want to suppress even displaying columns with missing data? e.g., ratios? but be careful is so, as a given location might truly have NA values as result and you want to see that is the case not hide the column.
  
  if (missing(df)) { # show shell
    if (type == "demog") {rnames <- c(names_d, names_d_subgroups)} else {rnames <- names_e} 
    df <- data.frame(varnames_r = rnames, value = NA, stringsAsFactors = FALSE)
  }
  if (!(type[1] %in% c('demog', 'envt'))) {warning("report type must be demog or envt - using demog by default")}
  
  
  if (!("varnames_r"     %in% names(df))) {
    if ("varnames_shown" %in% names(df)) {
      df$varnames_r <- fixcolnames(df$varnames_shown, 'long', 'r') # try infer name
    } else {
      warning("Cannot show standard report without variable names - using placeholders") # maybe the user supplied their own indicators
      df$varnames_shown <- paste0("Indicator ", 1:NROW(df))
      df$varnames_r <- df$varnames_shown
    }
  }
  if (!("varnames_shown" %in% names(df))) {df$varnames_shown <- fixcolnames(df$varnames_r, 'r', 'long')}
  if (!("state_avg"      %in% names(df))) {df$state_avg    <- NA}
  if (!("state_pctile"   %in% names(df))) {df$state_pctile <- NA}
  if (!("us_avg"         %in% names(df))) {
    df$us_avg <- NA
    df$us_avg[df$varnames_r %in% names(usastats)] <- usastats[usastats$PCTILE == "mean", match( df$varnames_r, names(usastats), nomatch = 0)] # fill in any that we can
  }  
  if (!("us_pctile"      %in% names(df))) {df$us_pctile <- NA}
  if (!("state_ratio"      %in% names(df))) {df$state_ratio <- NA}
  if (!("us_ratio"      %in% names(df))) {df$us_ratio <- NA}
  
  
  # SORT TABLE to show in the same sort order as EJScreen reports use, acc to metadata in map_headernames
  df <- df[ order(as.numeric(varinfo(df$varnames_r, "reportsort"))), ] # items not found are just "" which is ok
  
  # ROUNDING ####
  # Should get number of decimal places to use as in EJScreen reports, acc to metadata in map_headernames
  # although that is for each indicator not for each grouping like percentcols in general.
  df$digits <- table_rounding_info(df$varnames_r) # returns NA for nonmatches
  df$digits[is.na(df$digits)] <- 2 # default if no match
  
  # rounding for groupings of variables  
  digits_percentcols <- 1
  digits_ratiocols   <- 2
  digits_percentilecols <- 0
  # digits_rawcols are row-specific, in df$digits for df$value with df$varnames_r
  
  # df$varnames_r
  if (type == "demog") {
    percentcols <- c("value", "state_avg",    "usa_avg")
  }
  if (type == "envt") {
    rawcols    <- c("value", "state_avg",    "usa_avg")
  }
  ratiocols  <- c(         'state_ratio',  'usa_ratio')
  pctilecols <- c(         'state_pctile', 'usa_pctile')
  
  ###################################################################################################### # 
  ###################################################################################################### # 
  
  if (type == 'demog') {
    
    # subgroup_rows <- which(df$varnames_r %in% names_d_subgroups )
    # print(subgroup_rows)
    
    nice_table <- gt::gt(df[ , c('varnames_shown', 'value',
                                 'state_avg', 'state_pctile',
                                 'usa_avg', 'usa_pctile', 
                                 'state_ratio', 'usa_ratio')]) |> 
      
      ## format column labels
      gt::cols_label(
        
        varnames_shown = gt::md('**Socioeconomic Indicators**'),
        value          = gt::md('**Value**'),
        
        state_avg    = gt::md('**Average<br> in State**'),
        state_pctile = gt::md('**Percentile<br> in State**'),
        
        usa_avg    = gt::md('**Average<br> in USA**'),
        usa_pctile = gt::md('**Percentile<br> in USA**'),
        
        state_ratio = gt::md('**Ratio to<br> State Avg.**'),
        usa_ratio =   gt::md('**Ratio to<br> USA Avg.**')
        
      )  |>
      
      ## add subgroup header
      # if (length(subgroup_rows) > 0) {   # awkward to make this conditional when using the pipe here...
      # 
      #   nice_table <-  nice_table |> 
      #   gt::tab_row_group(label = gt::md('**Race/ethnic subgroups**'),
      #                     rows = subgroup_rows  # (2+nrow(df) - length(names_d_subgroups)):(nrow(df))
      #   ) |>
      #   # add all/main group header
      #   gt::tab_row_group(label = gt::md('**Socioeconomic Indicators**'),
      #                     rows = 1:nrow(df)   # 1:(1+nrow(df) - length(names_d_subgroups))
    #   )
    #   }
    #   ############################################### # 
    #   
    # nice_table <- nice_table |> 
    
    # RAW DEMOG PERCENT SCORE COLUMNS
    
    gt::fmt_percent(columns = percentcols, rows = dplyr::everything(),  decimals = digits_percentcols) |>
      
      # RATIO COLUMNS
      
      gt::fmt_number(columns = ratiocols, rows = dplyr::everything(),  decimals = digits_ratiocols) |>
      
      # PERCENTILE COLUMNS
      
      gt::fmt_number(columns = percentilecols, rows = dplyr::everything(),  decimals = digits_percentilecols) 
    ############################################### #
    #  |>
    nice_table <- nice_table |> 
      
      ## replace NAs with --
      gt::sub_missing(missing_text = '--') |> 
      
      ## add footnote
      gt::tab_footnote(
        footnote = "Avg. in state means the average indicator value, among all the residents at these sites, using the statewide value in each resident's state. Percentile in state means the same, but using the site-specific value (expressed as a percentile) where each resident lives.", 
        locations = gt::cells_column_labels(
          columns = c(state_avg, state_pctile)
        )
      )  |> 
      ## center values for %ile columns
      gt::cols_align(
        align = 'center', columns = pctilecols
      )
    ###################################################################################################### # 
    ###################################################################################################### # 
    
  } else if (type == 'envt') {
    
    nice_table <- gt::gt(df) |> 
      
      ## format column labels
      gt::cols_label(
        
        var_names = gt::md('**Pollution and Sources**'),
        value     = gt::md('**Value**'),
        
        state_avg    = gt::md('**Average<br> in State**'),
        state_pctile = gt::md('**Percentile<br> in State**'),
        
        usa_avg    = gt::md('**Average<br> in USA**'),
        usa_pctile = gt::md('**Percentile<br> in USA**'),
        
        state_ratio = gt::md('**Ratio to<br> State Avg.**'),
        usa_ratio =   gt::md('**Ratio to<br> USA Avg.**')
        
      )
    
    ############################################### # 
    
    nice_table <-   nice_table |> 
      
      ## RATIO COLUMNS - envt
      
      gt::fmt_number(columns = ratiocols, rows = dplyr::everything(),  decimals = digits_ratiocols) |>
      
      # PERCENTILE COLUMNS - envt
      
      gt::fmt_number(columns = percentilecols, rows = dplyr::everything(),  decimals = digits_percentilecols) |>
      
      # RAW ENVT SCORE COLUMNS
      
      # *** Need a loop here or some other way to handle a flexible number of rows instead of hard coding it for 13 ! ***
      # 
      gt::fmt_number(columns = rawcols, rows = 1,  decimals = df$digits[1]) |> 
      gt::fmt_number(columns = rawcols, rows = 2,  decimals = df$digits[2]) |> 
      gt::fmt_number(columns = rawcols, rows = 3,  decimals = df$digits[3]) |> 
      gt::fmt_number(columns = rawcols, rows = 4,  decimals = df$digits[4]) |> 
      gt::fmt_number(columns = rawcols, rows = 5,  decimals = df$digits[5]) |> 
      gt::fmt_number(columns = rawcols, rows = 6,  decimals = df$digits[6]) |> 
      gt::fmt_number(columns = rawcols, rows = 7,  decimals = df$digits[7]) |> 
      gt::fmt_number(columns = rawcols, rows = 8,  decimals = df$digits[8]) |> 
      gt::fmt_number(columns = rawcols, rows = 9,  decimals = df$digits[9]) |> 
      gt::fmt_number(columns = rawcols, rows = 10, decimals = df$digits[10]) |> 
      gt::fmt_number(columns = rawcols, rows = 11, decimals = df$digits[11]) |> 
      gt::fmt_number(columns = rawcols, rows = 12, decimals = df$digits[12]) |> 
      gt::fmt_number(columns = rawcols, rows = 13, decimals = df$digits[13])   
    
    # ## pm 2.5
    # gt::fmt_number(columns = rawcols, rows = 1,  decimals = 2) |> 
    # ## ozone
    # gt::fmt_number(columns = rawcols, rows = 2,  decimals = 1) |> 
    # ## cancer risk
    # gt::fmt_number(columns = rawcols, rows = 3,  decimals = 0) |> 
    # ## respiratory
    # gt::fmt_number(columns = rawcols, rows = 4,  decimals = 1) |> 
    # ## diesel pm
    # gt::fmt_number(columns = rawcols, rows = 5,  decimals = 3) |> 
    # ## lead paint
    # gt::fmt_number(columns = rawcols, rows = 6,  decimals = 2) |> 
    # ## traffic proximity
    # gt::fmt_number(columns = rawcols, rows = 7,  decimals = 0) |> 
    # ## NPL proximity
    # gt::fmt_number(columns = rawcols, rows = 8,  decimals = 3) |>
    # ## RMP proximity
    # gt::fmt_number(columns = rawcols, rows = 9,  decimals = 2) |> 
    # ## TSDF proximity
    # gt::fmt_number(columns = rawcols, rows = 10, decimals = 1) |>
    # ## NPDES proximity
    # gt::fmt_number(columns = rawcols, rows = 11, decimals = 4) |> 
    # ## Underground storage tanks
    # gt::fmt_number(columns = rawcols, rows = 12, decimals = 0) |> 
    # # and it was missing RSEI indicator
    
    
    ############################################### # 
    
    ############################################### # 
    
    nice_table <-   nice_table |> 
      
      gt::sub_missing(missing_text = '--') |> 
      ## add group header
      # gt::tab_row_group(label = gt::md('**Pollution and Sources**'), rows = 1:(nrow(df)) ) |> 
      # 
      ## add footnote
      gt::tab_footnote(
        footnote = "Avg. in state means the average indicator value, among all the residents at these sites, using the statewide value in each resident's state. Percentile in state means the same, but using the site-specific value (expressed as a percentile) where each resident lives.", 
        locations = gt::cells_column_labels(
          columns = c(state_avg, state_pctile)
        )
      )  |>
      ## center values for %ile columns
      gt::cols_align(
        align = 'center', columns = pctilecols
      )
    
  } else {
    stop('Argument "type" should be one of "demog" or "envt"')
  }
  ###################################################################################################### # 
  ###################################################################################################### # 
  
  nice_table |> 
    
    ## add outer lines
    gt::opt_table_outline(color = my_border_color) |> 
    gt::opt_table_lines() |> 
    ## add stripes for alternating rows
    gt::opt_row_striping() |>
    gt::tab_options(
      ## set font, font size, font color
      table.font.names = 'Arial',
      table.font.size = '13.33px', 
      table.font.color = '#333333',
      ## change spacing around text
      data_row.padding = '1px',
      data_row.padding.horizontal = '10px',
      row_group.padding = '1px',
      row_group.padding.horizontal = '0px',
      footnotes.padding = '20px',
      
      ## change colors of cells
      row.striping.background_color = my_cell_color,
      row_group.background.color    = my_cell_color,
      
      ## change colors of lines/borders
      column_labels.vlines.color     = my_border_color,
      row_group.border.top.color     = my_border_color,
      row_group.border.bottom.color  = my_border_color,
      table_body.hlines.color        = my_border_color,
      table_body.vlines.color        = my_border_color,
      table_body.border.bottom.color = my_border_color
    ) |> 
    ## center column headers
    gt::tab_style(
      style = gt::cell_text(align = 'center', v_align = 'middle'),
      locations = gt::cells_column_labels(columns = dplyr::everything())
    ) # |> 
  
  # gtExtras::gt_color_rows( # https://jthomasmock.github.io/gtExtras/reference/gt_color_rows.html
  # 
  #   palette = "ggthemes::colorblind",
  #   # note that you can manually define range like c(4, 6, 8)
  #   domain = c(80,90,95),
  #   pal_type = "discrete"
  # )
  
  # gt::data_color(  # 
  #   columns = c(state_pctile, usa_pctile),
  #   method = "bin",
  #   palette = c("white", "yellow", "orange", "red"),
  #   domain = c(0, 80, 90, 95)
  # )
  
}
