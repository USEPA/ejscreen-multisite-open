#' table_gt_from_overall - prep results_overall for formatting as nice gt table/report
#' table_gt_format() input is the output of this function
#' @param results_overall data.table like testoutput_ejamit_100pts_1miles$results_overall
#'    from something like ejamit(testpoints_100, radius = 1)$results_overall
#' @param type demog or envt to specify which type of table
#' @seealso  [table_gt_format()]
#' @return A table ready for formatting via gt package in the function table_gt_format
#' @export
#'
#' @examples
#'    x = table_gt_from_overall(testoutput_ejamit_100pts_1miles$results_overall)
#'    y = table_gt_format(x)
#'    
table_gt_from_overall <- function(results_overall, type = c("demog", "envt")[1]) {
  
  # results_overall <-  testoutput_ejamit_100pts_1miles$results_overall 
  
  if (!data.table::is.data.table(results_overall)) results_overall <- data.table::as.data.table(results_overall)
  
  
  if (type == "envt") {
    
    table_4gt <- data.frame(
      
      varnames_r     =                   c(names_e         ),  # sort order for display will be changed by the gt function!
      varnames_shown =                   c(names_e_friendly),
      
      value          = results_overall[, ..names_e] %>% t, 
      
      'state_avg'    = results_overall[, ..names_e_state_avg] %>% t,
      'state_pctile' = results_overall[, ..names_e_state_pctile] %>% t,   
      
      'usa_avg' = data.frame(usa_avg = unlist(avg.in.us[ , names_e])),    # is available as avg.in.us
      
      'usa_pctile'   = results_overall[, ..names_e_pctile] %>% t,  # xxx
      
      'state_ratio'  = results_overall[, ..names_e_ratio_to_state_avg] %>% t,
      'usa_ratio'    = results_overall[, ..names_e_ratio_to_avg] %>% t
    )
    
  } else {
    
    table4gt   <- data.frame(
      
      # ignore input$calculate_ratios ? ***
      
      varnames_r     =                                       c(names_d         ,       names_d_subgroups           ), # sort order for display will be changed by the function!
      varnames_shown =                                       c(names_d_friendly,       names_d_subgroups_friendly  ),
      
      value           = data_processed()$results_overall[, c(..names_d,              ..names_d_subgroups)] %>% t, 
      
      'state_avg'    = (data_processed()$results_overall[, c(..names_d_state_avg,    ..names_d_subgroups_state_avg)]    %>% t), 
      'state_pctile' = (data_processed()$results_overall[, c(..names_d_state_pctile, ..names_d_subgroups_state_pctile)] %>% t), 
      
      'usa_avg'  = data.frame(usa_avg = unlist(avg.in.us[, c(  names_d,                names_d_subgroups)])) , # avg.in.us is a data.frame not data.table
      
      'usa_pctile'   = (data_processed()$results_overall[, c(..names_d_pctile,       ..names_d_subgroups_pctile)]  %>% t),
      
      # note these have subgroups too already in them:
      # "state_ratio" = unlist(ratio.to.state.d()),  
      "state_ratio"  = (data_processed()$results_overall[, c(..names_d_ratio_to_state_avg, ..names_d_subgroups_ratio_to_state_avg)] %>% t),     # xxx
      # "usa_ratio"   = unlist(ratio.to.us.d() )     
      "usa_ratio"   =  (data_processed()$results_overall[, c(..names_d_ratio_to_avg,       ..names_d_subgroups_ratio_to_avg)] %>% t )     # xxx
    )
    
    # need to verify percentile should be rounded here or use ceiling() maybe? 
    # try to replicate EJScreen percentiles as they report them...
    # *** maybe update to use sigfigs or decimal rounding info metadata stored in EJAMejscreenapi::map_headernames table
    # tab_data_d$usa_pctile   <- round(tab_data_d$usa_pctile   ,0) 
    # tab_data_d$state_pctile <- round(tab_data_d$state_pctile ,0)
    ## switch to this once it works:
    # tab_data_d$usa_pctile    <- table_round(tab_data_d$usa_pctile)     # # round( , table_rounding_info(names(tab_data_d$usa_pctile)))
    # tab_data_d$state_pctile  <- table_round(tab_data_d$state_pctile) 
    
    # ## set colors for table or just use defaults from that function
    # my_cell_color   <- '#dce6f0';
    # my_border_color <- '#aaaaaa';
    
    
    
  }
  return(table_4gt)
  
}
