#scripttocreateorupdatethetablethatmapsfromoneversionof
#variablenames(e.g.,long,clearerones)toanother(e.g.,shortereasierforanalysisorprogramminginR,etc.)
#a<-read.csv('./data-raw/map_headernames.csv')

# if it is being created from the spreadsheet:

getwd()
setwd('~/../../R/mysource/EJAM')
dir("./data-raw", pattern = "map_headernames")
dir("./data", pattern = "map_headernames")

# file.exists()

map_headernames <- as.data.frame(readxl::read_xlsx(
  './data-raw/map_headernames_2.3.xlsx'
  ))

map_headernames[is.na(map_headernames)] <- ''  #changeNAvaluestoemptycell,soitiseasiertosubsetetc.
# map_headernames$rname[is.na(map_headernames$rname)] <- ""
# map_headernames$apiname[is.na(map_headernames$apiname)] <- ""
# map_headernames$csvname2.2[is.na(map_headernames$csvname2.2)] <- ""
# map_headernames$oldnames[is.na(map_headernames$oldnames)] <- ""
# map_headernames$longname_tableheader[is.na(map_headernames$longname_tableheader)] <- ""


# NOTE THE UPDATED VERSION OF THE PACKAGE MUST BE INSTALLED FOR metadata_add() to use the right version info
# or it must be loaded via load.all() and dont use EJAM:::

# attr(map_headernames,"date_created") <- Sys.Date()
map_headernames <- EJAM:::metadata_add(map_headernames)

usethis::use_data(map_headernames, overwrite = TRUE)


cat('must redo sample dataset outputs like in EJAM and  EJAMejscreenapi/data-raw \n')
# EJAM/data-raw/datacreate_testpoints_testoutputs.R
# /datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R

cbind(names(map_headernames))
stop('this is a script')


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
## see variable names in table called map_headernames  from  EJAMejscreenapi
# 
# EJAMejscreenapi::map_headernames[, c("oldnames", "newnames_ejscreenapi", "varlist")]


############################################################# #
## see names as used in EJAM
# 
# names(ejamit(testpoints_50[1:2,])$results_bysite)
# 
# library(EJAM)
# x = names(ejamit(testpoints_50)$results_bysite)
# y = EJAMejscreenapi::map_headernames$newnames_ejscreenapi
# setdiff(x,y) ###  "EJScreen Report" "EJScreen Map"    "ACS Report"      "ECHO report"    
# 
# names_all
# as.vector(unlist(namez)) 
# names(namez)



# see just the friendly names 
# 
# namez[grepl("friendly", names(namez))]
# 
# map_headernames$names_friendly["" != map_headernames$names_friendly]
# unique(map_headernames$shortlabel)
# 
# x = map_headernames[, c("newnames_ejscreenapi", "names_friendly", 'shortlabel', "varlist")]
# x=x[order(x$varlist),]
# # intersect(paste0("names_", names(namez)) , x$varlist )
# 
#   lists that were not yet in map_headernames: 
# setdiff(paste0("names_", names(namez)) , x$varlist )
# 
# 
# 
# setdiff_yx(paste0("names_", names(namez)) , x$varlist )
# 
# # these are missing from namez and from EJAM names_all
# "names_ej_supp"
# "names_ej_supp_pctile"
# "names_ej_supp_state_pctile"
#
#  "names_d_median"             "names_e_median"

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
#  versus  the friendly names stored in EJAMejscreenapi map_headernames  and xlsx that creates it ####

stop('this is a script')
## requires EJAM package to refer to namez
nnn = paste0("names_", names(namez))
nnn = namez[grepl('friendly', nnn)]

reconcile_friendly_names <- list()

for (i in 1:length(nnn)) {
  
  checked =  paste0("names_", gsub("_friendly","", names(nnn)[i])  )
  # cat("checking for varlist = ")
  # cat(checked,'\n')
  # cat("\n")
  
  
  zz <- as.vector(unlist(nnn[i]))
  if (grepl("white", zz[1], ignore.case = TRUE)) { zz = zz[c(2:8,1)]}
  
  thosevars <- map_headernames$names_friendly[map_headernames$varlist == checked]  # and note shortlabel column also
  
  if (i == 1) {
    df = data.frame(
      varlist = checked, 
      in.ejscreenapi.map_headernames  = map_headernames$newnames_ejscreenapi[map_headernames$varlist == checked],
      in.ejam.namez = get(checked),
      friendly.name.in.ejscreenapi.map_headernames = thosevars, 
      friendly.name.in.ejam.namez = zz
    )
  } else {
    dfmore = data.frame(
      varlist = checked, 
      in.ejscreenapi.map_headernames  =  map_headernames$newnames_ejscreenapi[map_headernames$varlist == checked],
      in.ejam.namez = get(checked),
      friendly.name.in.ejscreenapi.map_headernames = thosevars, 
      friendly.name.in.ejam.namez = zz
    )
    df = rbind(df, dfmore)
  }
  
  # print(  cbind(
  #   friendly.name.in.ejam.namez = zz, 
  #   friendly.name.in.ejscreenapi.map_headernames = thosevars)
  #   )
  #  cat("\n\n")
}
reconcile_friendly_names <- df 

vcheck = cbind(df[,2:3], identicalnaming = df[,2] == df[,3])
vcheck[!vcheck$identicalnaming,]

save(reconcile_friendly_names, file = "reconcile_friendly_names.rda")
write.csv(reconcile_friendly_names, file = "reconcile_friendly_names.csv", row.names = F)

