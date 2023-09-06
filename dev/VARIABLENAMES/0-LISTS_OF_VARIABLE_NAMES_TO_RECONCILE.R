
# THIS IS OBSOLETE MORE OR LESS but may want to check that all these were addressed

# other places the variable names are used include eg
# 
# see also: "NOTES_fix-update_E_and_D_variablenames_updates.R"
#  
# 
# --------VARIABLES NAMES TABLES/LISTS TO MERGE:---------
# 
# 	OW EJSCREENbatch :: inputs as needed for OW tool? (ACS, ejscreen, Census?)
# **	OW EJSCREENbatch :: outputs (just the buffering part that should output counts?; not summary/ graphics/tables?)
# 
# **	EJAM::blockgroupstats dataset	
# 	EJAM::doaggregate() source code has lists of formulas and variables, including which are summed, wtd mean, or calculated via formulas.
# 	EJAM::  outputs of EJAM::doaggregate (the app outputs like ejscreenapi outputs)
# 	EJAM::usastats
# ****	EJAM::  xls? doaggregate_output.xls, reconciliation tab (xwalk doag -API -batchsum)
# 
# ****   /batch.summarizer/ still? doaggregate_output12032021.csv
#   	    & xlsx tab had crosswalk doag vs API vs old live working batchsum 
# ****	batch.summarizer::  csv of map_batch_to_friendly  fieldnames v1 - works for old live batchsum
# 	batch.summarizer::  inputs needed by batch.summarizer 
# 	batch.summarizer::  outputs of downloaded table (has more summary stats than the input table)
# 
#    /batch.summarizer/  map_batch_to_friendly_fieldnames_2022_EJAM.xlsx (may later delete  csv version) 
#    /batch.summarizer/  map_batch_to_friendly_fieldnames_2022_EJAM.csv
#    /batch.summarizer/data/  map_batch_to_friendly_fieldnames_2021_EJAM.rda
#    /batch.summarizer/inst/ejscreenapi_variable_names_2022-04.xlsx
# 
# **	ejscreenapi variable names / map_headernames.csv and was in .rda too
# 	ejscreenapi's  EJAMejscreenapi::popup_from_ejscreen() 
# 	ejscreenapi  outputs  -NOT ESSENTIAL AS CANT USE TO SUMMARIZE ACROSS SITES
# 
# ****	ejscreen package file ejscreenformulas etc. (name lists)
# 	ejscreen package file names.e, names.d, etc etc etc (data holding name lists)
# 	ejscreen/data/  nicenames, names.e.nice, names.evars, etc. etc. etc. etc. etc. 
# 	ejscreen package file bg22plus, and similar data. (data table's column names) 
# 	output of ejscreen package file ejscreen.create (output table's column names)
# 	ACS or Envt inputs as needed for ejscreen package file ejscreen_create() ?? not priority /ACS &Census raw?
#    /ejscreen/inst/  ejscreenformulas.xlsx    (deleted csv version) then save as the .rdata version
#    /ejscreen/data/  ejscreenformulas.rda  (and ejscreenformulasnoej.rda  )
#    /ejscreen/inst/  EJSCREEN_columns_explained.xlsx   (deleted csv version) 
#    /ejscreen/inst/  EJSCREEN_columns_explained.csv  from FTP site  
#   /ejscreen/inst/variables_needed_template.csv  [csv that just has ACS variables needed by default.]
# 	change.fieldnames.ejscreen.r_to_gdb(), etc.
# 	
# **	epav/EJScreen FTP site geodatabase field names
# 	epa/EJScreen FTP site csv dataset (and map services versions/etc.)
# **	epa/EJScreen' API for standard reports (and used by ejscreenapi)
# 	epa's other map services field names
# 	epa's glossary pages, webpages, etc. (long names of variables)
# 
# ----------------------------------------------------------------------------------------------------------
