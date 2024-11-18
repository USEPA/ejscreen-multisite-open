#' @name epa_programs
#' @docType data
#' @title epa_programs (DATA) named vector with program counts
#' @description
#'    This is a named vector of EPA programs that lists the programs and has names with programs and program counts. Used by EJAM shiny app for dropdown menu.
#' @details
#'
#'  Also see
#'  - [EPA documentation describing each program code](https://www.epa.gov/frs/frs-data-sources) aka data source.
#'  - [2021-05/frs_program_abbreviations_and_names.xlsx](https://www.epa.gov/sites/default/files/2021-05/frs_program_abbreviations_and_names.xlsx)
#' 
#'   ## For a table sorted by how many facilities from FRS are listed for each program:
#'
#'   `epa_programs_counts <- frs_by_programid[, .N, by="program"][order(N), ]`
#'
#'   ## Top 25 Federal ones (by trying to exclude State databases from the list)
#'
#'   `epa_programs_counts$fed = !grepl("^[A-Z]{2}[-]", as.vector(epa_programs_counts$program),1,3)`
#'   
#'   `table(epa_programs_counts$fed)`
#'
#'   `epa_programs_counts[fed == TRUE, ][order(-N), ][1:25, ]`
#'
#'   Also see [frs_by_programid]
'epa_programs'
