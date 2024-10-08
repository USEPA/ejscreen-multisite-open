
# This must be redone/ rebuilt whenever frs and frs_by_programid  are updated to get counts updated !! ***
# This data object `epa_programs` is used in ui to offer a list of programs with a count of sites for each

# 1st must update frs_by_programid, via script in EJAM/datacreate_frs_.R  


# get the frs_by_programid dataset (which should have been just updated via script in EJAM/datacreate_frs_.R ! )

if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")


# get counts by program type, like AIRS/AQS program has more than 8,000 facilities in FRS dataset, etc.

epa_program_counts <- dplyr::count(frs_by_programid, program, name = 'count') # EJAM :: frs_by_programid
epa_program_counts$pgm_text_dropdown <- paste0(epa_program_counts$program, ' (',prettyNum(epa_program_counts$count, big.mark = ','), ')')
# create a named vector where $program is the vector elements and $pgm_text_dropdown is the names:
epa_programs <- setNames(epa_program_counts$program, epa_program_counts$pgm_text_dropdown)

# Finally, save it in the EJAM/data/ folder for use as a dataset loaded with the EJAM package:

# Finally, save it in the EJAM/data/ folder for use as a dataset loaded with the EJAM package:
epa_programs <- metadata_add(epa_programs)
usethis::use_data(epa_programs, overwrite = TRUE)

cat("UPDATED DOCUMENTATION OF THIS DATA SET MANUALLY - HELP DOC IS A BIT COMPLICATED\n")
if (rstudioapi::isAvailable()) {
    rstudioapi::documentOpen('./R/data_epa_programs.R')
}
############################################################################################ #

# ***  note `epa_programs` is a complete list and has counts, while 
#  `frsprogramcodes` has the full titles but only about a dozen key programs and no counts.

#   frsprogramcodes
#                                                               description     code
# 1    National Pollutant Discharge Elimination System (NPDES) (ICIS-NPDES)    NPDES
# 2  The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)      AIR
# 3    The Resource Conservation and Recovery Act (RCRA) Information System RCRAINFO
# 4                                   Risk Management Plan (RMP) facilities      RMP
# 5                      The Safe Drinking Water Information System (SDWIS)     SFDW
# 6                              The Superfund Enterprise Management System     SEMS
# 7                              Clean Air Markets Division Business System   CAMDBS
# 8                                        Toxics Release Inventory Program     TRIS
# 9                                        Greenhouse Gas Reporting Program   E-GGRT
# 10                                             Emissions Inventory System      EIS
# 11                                           Toxic Substances Control Act     TSCA
# 12   Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields    ACRES
# 13 Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities       BR

# (frsprogramcodes.rda is tiny and not a table so cannot use .arrow for it and not in pins board
#  - see EJAM/data-raw/datacreate_frsprogramcodes.R )
############################################################################################ #
