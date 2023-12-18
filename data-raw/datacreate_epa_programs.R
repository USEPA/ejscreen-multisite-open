
# ***  check if this is redundant with  frsprogramcodes()



if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")

epa_program_counts <- dplyr::count(frs_by_programid, program, name = 'count') # EJAM :: frs_by_programid
epa_program_counts$pgm_text_dropdown <- paste0(epa_program_counts$program, ' (',prettyNum(epa_program_counts$count, big.mark = ','), ')')
epa_programs <- setNames(epa_program_counts$program, epa_program_counts$pgm_text_dropdown)

usethis::use_data(epa_programs)

# this had been in global.R

# it is used in ui to offer a list of programs with a count of sites for each

# this must be redone/ rebuilt whenever frs and frs_by_programid  are updated !!! ***
