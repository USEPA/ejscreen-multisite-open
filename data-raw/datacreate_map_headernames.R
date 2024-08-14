if (!exists("askquestions")) {askquestions <- FALSE}
if (!exists("rawdir")) {rawdir <- './data-raw'}

#createorupdatethetablethatmapsfromoneversionof
#variablenames(e.g.,long,clearerones)
#toanother(e.g.,shortereasierforanalysisorprogramminginR,etc.)

datacreate_map_headernames <- function(rawdir = "./data-raw", fname = 'map_headernames_2.3.xlsx') {
  
  fpath <- file.path(rawdir, fname)
  if (!file.exists(fpath)) {stop("did not find (but this requires) ", fpath)}
  
  map_headernames <- as.data.frame(readxl::read_xlsx(fpath))
  
  map_headernames[is.na(map_headernames)] <- ''  #changeNAvaluestoemptycell,soitiseasiertosubsetetc.
  
  cat('must redo sample dataset outputs in EJAM/inst/testdata/  via
  EJAM/data-raw/datacreate_testpoints_testoutputs.R
  and possibly also EJAMejscreen/ files via
  datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R
      \n')
  
  # cbind(names(map_headernames))
  invisible(map_headernames)
}
################################################################################# #

#  UPDATE map_headernames_2.3.xlsx MANUALLY, 
#  then read .xlsx and save as dataset for package
if (askquestions && interactive()) {
  y <- askYesNo("Want to open .xlsx to edit it now?")
  if (!is.na(y) && y) {
    fpath = rstudioapi::selectFile(path = rawdir, filter = "xlsx")
    browseURL(fpath)
    y <- askYesNo("Y if done editing and ready to go on, N to abort/stop")
    if (is.na(y) || !y) {stop("stopping script")} 
  }
}
if (!exists("fpath")) {
  map_headernames <- datacreate_map_headernames()
} else {
  map_headernames <- datacreate_map_headernames(fpath)
}

# patch to fill in some missing info, used by batch.summarize()
# but should make names_r_all more complete
map_headernames$calculation_type[map_headernames$rname %in% setdiff(map_headernames$rname[calctype(map_headernames$rname) == "sum of counts"], names_all_r[calctype(names_all_r) == "sum of counts"])] <- "sum of counts"


map_headernames <- metadata_add(map_headernames)
usethis::use_data(map_headernames, overwrite = TRUE)

rm(datacreate_map_headernames)
rm(y)
cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())


if (1 == 0) {
############################################################# #
#
#'    Names of API output fields and their definitions: 
#'    
#'    2023_07         ver 2.2 <https://ejscreen.epa.gov/mapper/ejsoefielddesc1.html>
#'    2022 to 2023_06 ver 2.1 <https://ejscreen.epa.gov/mapper/ejsoefielddesc.html>
#'    
#'    Simple web interface to API:
#'    
#'    2023_07         ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenapi1.html>
#'    2022 to 2023_06 ver 2.1 <https://www.epa.gov/ejscreen/ejscreen-api>
#'    
#'    REST endpoint:
#'    
#'    2023_07         ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=>
#'    2022 to 2023_06 ver 2.1 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=>
#'    
#'    Web tool user guide: 
#'    
#'    2023_07         ver 2.2 <https://ejscreen.epa.gov/mapper/help/ejscreen_help.pdf>
#'    
#'    
############################################################# #
## see variables names as provided by EJScreen API before they get renamed
# 
# names(jsonlite::fromJSON(rawToChar((ejscreenRESTbroker(lon = -80, lat = 42))$content)))

############################################################# #
## see names as used in EJAM
# 
# names(ejamit(testpoints_50[1:2,])$results_bysite)
# 
# library(EJAM)
# x = names(ejamit(testpoints_50)$results_bysite)
# y = map_headernames$rname
# setdiff(x,y) ###  "EJScreen Report" "EJScreen Map"    "ACS Report"      "ECHO report"    
# 
# names_all
# as.vector(unlist(namez)) 
# names(namez)

 

############################################################# #
## see variable names as provided in EJScreen dataset downloaded from FTP site
## and stored in EJAMejscreendata package ? no longer stored there as of v 2.2
# 
# names(EJAMejscreendata
#   EJSCREEN_StatePct_with_AS_CNMI_GU_VI)
# names(EJAMejscreendata
#   EJSCREEN_Full_with_AS_CNMI_GU_VI)


###################################################################### #
#  COMPARE EJAM friendly names stored in namez or lists like names_d_friendly 
#  versus  the friendly names stored in  map_headernames  and xlsx that creates it ####

stop('this is a script')
## requires EJAM package to refer to namez
# nnn = paste0("names_", names(namez))
# nnn = namez[grepl('friendly', nnn)]
# 
# reconcile_friendly_names <- list()
# 
# for (i in 1:length(nnn)) {
#   
#   checked =  paste0("names_", gsub("_friendly","", names(nnn)[i])  )
#   # cat("checking for varlist = ")
#   # cat(checked,'\n')
#   # cat("\n")
#   
#   
#   zz <- as.vector(unlist(nnn[i]))
#   if (grepl("white", zz[1], ignore.case = TRUE)) { zz = zz[c(2:8,1)]}
#   
#   thosevars <- map_headernames$names_friendly[map_headernames$varlist == checked]  # and note shortlabel column also
#   
#   if (i == 1) {
#     df = data.frame(
#       varlist = checked, 
#       in.ejscreenapi.map_headernames  = map_headernames$rname[map_headernames$varlist == checked],
#       in.ejam.namez = get(checked),
#       friendly.name.in.ejscreenapi.map_headernames = thosevars, 
#       friendly.name.in.ejam.namez = zz
#     )
#   } else {
#     dfmore = data.frame(
#       varlist = checked, 
#       in.ejscreenapi.map_headernames  =  map_headernames$rname[map_headernames$varlist == checked],
#       in.ejam.namez = get(checked),
#       friendly.name.in.ejscreenapi.map_headernames = thosevars, 
#       friendly.name.in.ejam.namez = zz
#     )
#     df = rbind(df, dfmore)
#   }
#   
#   # print(  cbind(
#   #   friendly.name.in.ejam.namez = zz, 
#   #   friendly.name.in.ejscreenapi.map_headernames = thosevars)
#   #   )
#   #  cat("\n\n")
# }
# reconcile_friendly_names <- df 
# 
# vcheck = cbind(df[,2:3], identicalnaming = df[,2] == df[,3])
# vcheck[!vcheck$identicalnaming,]
# 
# save(reconcile_friendly_names, file = "reconcile_friendly_names.rda")
# write.csv(reconcile_friendly_names, file = "reconcile_friendly_names.csv", row.names = F)

}

# # which sources provide which variables or indicators? 
# #
# some = unique(map_headernames$rname[map_headernames$varlist != "" & map_headernames$varlist != "x_anyother"])
# info = varinfo(some, info = c('api', 'csv', 'acs', 'varlist'))
# info[nchar(paste0(info$api, info$csv, info$acs)) > 0, ]
# 
