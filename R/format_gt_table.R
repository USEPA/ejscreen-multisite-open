#' format_gt_table
#'
#' @param df, a data frame with 6 columns (var_names, value, state_avg, state_pctile, usa_avg, usa_pctile), and one row per indicator
#' @param type, string - one of 'demog', 'envt' 
#' @param my_cell_color, color for filling in background of table cells, can be given as string ('blue') or hex code ('#0070c0')
#' @param my_border_color, color for table borders and boundaries, can be given as string ('blue') or hex code ('#0070c0')
#' 
#' @return a 'gt'-style table with formatting to closely match EJScreen standard report formatting
#' @export
#'
format_gt_table <- function(df, type, my_cell_color =  '#dce6f0', my_border_color = '#0070c0'){
   if(type == 'demog'){
      subgroup_rows <- which(df$var_names %in% names_d_subgroups_friendly)
      print(subgroup_rows)
      
     nice_table <- gt::gt(df) %>% 
      ## format column labels
      gt::cols_label(

        var_names = gt::md('**Selected Variables**'),
        value     = gt::md('**Value**'),
        
        state_avg    = gt::md('**Average<br> in State**'),
        state_pctile = gt::md('**Percentile<br> in State**'),
        
        usa_avg    = gt::md('**Average<br> in USA**'),
        usa_pctile = gt::md('**Percentile<br> in USA**'),
        
        state_ratio = gt::md('**Ratio to<br> State Avg.**'),
        usa_ratio =   gt::md('**Ratio to<br> USA Avg.**')
        
      )  %>% 
       ## add subgroup header
       gt::tab_row_group(label = gt::md('**Race/ethnic subgroups**'), 
                         rows = subgroup_rows  # (2+nrow(df) - length(names_d_subgroups)):(nrow(df)) 
       ) %>% 
       ## add all/main group header 
       gt::tab_row_group(label = gt::md('**Socioeconomic Indicators**'), 
                         rows = 1:nrow(df)   # 1:(1+nrow(df) - length(names_d_subgroups))  
       ) %>%   
       
      ## format decimal places for all indicators
      gt::fmt_percent(columns = c(2,3,5), rows = everything(),  decimals = 0) %>%
      ## replace NAs with --
      gt::sub_missing(missing_text = '--') %>% 
      
      ## add footnote
      gt::tab_footnote(
        footnote = "Avg. in State is the average over all these residents of their State's overall (avg.) value. \nPercentile in State is the average person's State percentile.",
        locations = gt::cells_column_labels(
          columns = c(state_avg, state_pctile)
        )
      )  %>% 
      ## center values for %ile columns
      gt::cols_align(
        align = 'center', columns = c('state_pctile', 'usa_pctile')
      ) 
    
  } else if(type == 'envt') {
    
    nice_table <- gt::gt(df) %>% 
      ## format column labels
      gt::cols_label(
        
        var_names = gt::md('**Selected Variables**'),
        value     = gt::md('**Value**'),
        
        state_avg    = gt::md('**Average<br> in State**'),
        state_pctile = gt::md('**Percentile<br> in State**'),
        
        usa_avg    = gt::md('**Average<br> in USA**'),
        usa_pctile = gt::md('**Percentile<br> in USA**'),
        
        state_ratio = gt::md('**Ratio to<br> State Avg.**'),
        usa_ratio =   gt::md('**Ratio to<br> USA Avg.**')
        
      ) %>% 
      ## format different decimal places for each indicator # should get pulled from 
      ## pm 2.5
      gt::fmt_number(columns = c(2,3,5), rows = 1,  decimals = 2) %>% 
      ## ozone
      gt::fmt_number(columns = c(2,3,5), rows = 2,  decimals = 1) %>% 
      ## cancer risk
      gt::fmt_number(columns = c(2,3,5), rows = 3,  decimals = 0) %>% 
      ## respiratory
      gt::fmt_number(columns = c(2,3,5), rows = 4,  decimals = 1) %>% 
      ## diesel pm
      gt::fmt_number(columns = c(2,3,5), rows = 5,  decimals = 3) %>% 
      ## lead paint
      gt::fmt_number(columns = c(2,3,5), rows = 6,  decimals = 2) %>% 
      ## traffic proximity
      gt::fmt_number(columns = c(2,3,5), rows = 7,  decimals = 0) %>% 
      ## NPL proximity
      gt::fmt_number(columns = c(2,3,5), rows = 8,  decimals = 3) %>%
      ## RMP proximity
      gt::fmt_number(columns = c(2,3,5), rows = 9,  decimals = 2) %>% 
      ## TSDF proximity
      gt::fmt_number(columns = c(2,3,5), rows = 10, decimals = 1) %>%
      ## NPDES proximity
      gt::fmt_number(columns = c(2,3,5), rows = 11, decimals = 4) %>% 
      ## Underground storage tanks - 0 decimal places
      gt::fmt_number(columns = c(2,3,5), rows = 12, decimals = 0) %>% 
      gt::sub_missing(missing_text = '--') %>% 
      ## add group header
      gt::tab_row_group(label = gt::md('**Pollution and Sources**'), rows = 1:(nrow(df)) ) %>% 
    
      ## add footnote
      gt::tab_footnote(
        footnote = "Avg. in State is the average over all these residents of their State's overall (avg.) value. \nPercentile in State is the average person's State percentile.",
        locations = gt::cells_column_labels(
          columns = c(state_avg, state_pctile)
        )
      )  %>%
      ## center values for %ile columns
      gt::cols_align(
        align = 'center', columns = c('state_pctile', 'usa_pctile')
      ) 
  } else {
    stop('Argument "type" should be one of "demog" or "envt"')
  }
  
  nice_table %>% 
    ## add outer lines
    gt::opt_table_outline(color = my_border_color) %>% 
    gt::opt_table_lines() %>% 
    ## add stripes for alternating rows
    gt::opt_row_striping() %>%
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
      row_group.background.color = my_cell_color,
      
      ## change colors of lines/borders
      column_labels.vlines.color = my_border_color,
      row_group.border.top.color = my_border_color,
      row_group.border.bottom.color = my_border_color,
      table_body.hlines.color = my_border_color,
      table_body.vlines.color = my_border_color,
      table_body.border.bottom.color = my_border_color
    ) %>% 
    ## center column headers
    gt::tab_style(
      style = gt::cell_text(align = 'center', v_align = 'middle'),
      locations = gt::cells_column_labels(columns = everything())
    ) # %>% 
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
