
#' Generate EJAM Summary Report in HTML within shiny app
#' 
#' Creates a 2 page report on overall results or for one site, with 
#' demographic and environmental indicators, and EJ Indexes if needed.
#' For the same function for use in RStudio, 
#' see [ejam2report()] which relies on `build_community_report()`
#' 
#' @param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#' @param analysis_title, title to use in header of report
#' @param totalpop, total population included in location(s) analyzed
#' @param locationstr, description of the location(s) analyzed
#' @param include_ejindexes, whether to build tables for EJ and EJ supp. indexes
#' @param in_shiny, whether the function is being called in or outside of shiny - affects location of header
#' @param filename, path to file to save HTML content to; if null, returns as string (used in Shiny app)
#' 
#' @keywords internal
#' 
build_community_report <- function(output_df, analysis_title, totalpop, locationstr, 
                                   include_ejindexes=FALSE, in_shiny = FALSE, filename = NULL){
  
  ## check that analysis was run with EJ columns; if not, don't add them
  if (include_ejindexes) {
    ejcols <- c(names_ej,names_ej_state, names_ej_supp,names_ej_supp_state)
    if (!(all(ejcols %in% names(output_df)))) {
      include_ejindexes <- FALSE
    } 
  }
  
  output_df_rounded <-   as.data.frame( output_df) 
  
  #  basic rounding and units
  r100x <- colnames(output_df_rounded) %in% c(names_d, names_d_avg, names_d_state_avg) # units were 0-1 not 0-100
  output_df_rounded[, r100x] <- 100 * (output_df_rounded[, r100x])
  if (include_ejindexes) {
    r2 <- colnames(output_df_rounded) %in% c(names_e, names_e_avg, names_e_state_avg, 
                                             names_ej, names_ej_supp)
  } else {
    r2 <- colnames(output_df_rounded) %in% c(names_e, names_e_avg, names_e_state_avg)
  }
  output_df_rounded[, r2] <- round(output_df_rounded[, r2], 2)
  if (include_ejindexes) {
    r0 <-  colnames(output_df_rounded) %in%  c(names_e_pctile, names_d_pctile, names_e_state_pctile, names_d_state_pctile, c(names_d, names_d_avg, names_d_state_avg),
                                               names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  } else {
    r0 <-  colnames(output_df_rounded) %in%  c(names_e_pctile, names_d_pctile, names_e_state_pctile, names_d_state_pctile, c(names_d, names_d_avg, names_d_state_avg) )
  }
  output_df_rounded[, r0] <- round(output_df_rounded[, r0], 0)
  pctsign <- colnames(output_df_rounded) %in% c(names_d, names_d_avg, names_d_state_avg)
  output_df_rounded[, pctsign] <- paste0(output_df_rounded[, pctsign], "%")
  
  
  full_page <- paste0(
    generate_html_header(analysis_title, totalpop, locationstr, in_shiny = in_shiny),
    generate_demog_header(),
    fill_tbl_full(output_df_rounded),
    collapse = ''
  )
  ## add EJ index and Supp EJ index tables 
  ## only if those columns are available
  if (include_ejindexes) {
    full_page <- paste0(full_page,
                        generate_ej_header(),
                        fill_tbl_full_ej(output_df_rounded), 
                        #generate_ej_supp_header(),
                        #fill_tbl_full_ej_supp(output_df_rounded),
                        collapse = '') 
  }
  
  full_page <- paste0(
    full_page,
    fill_tbl_full_subgroups(output_df_rounded),
    collapse = ''
  )
  
  if (is.null(filename)) {
    return(HTML(full_page))
  } else {
    sink(file = filename)
    cat(HTML(full_page))
    sink()
  }
}
